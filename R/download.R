



# -----------------------------------------------------------------------
# -----------------------------------------------------------------------

#' @param x an object of class \code{eupp_config}.
#' @param output_file character length 1, name of the output file.
#' @param output_format character length 1, defaults to \code{"guess"} (see details).
#' @param verbose logical length 1, verbosity, defaults to \code{FALSE}.
#'
#' @details Using partial matching for \code{x} and \code{level}.
#'
#' @rdname download
#' @importFrom httr GET add_headers
#' @importFrom tools file_ext
#' @author Reto Stauffer
#' @export
download_dataset <- function(x,
                             output_file,
                             output_format = c("guess", "grb", "nc"),
                             verbose = FALSE) {

    # ----------------------------------------------
    # Sanity checks
    # ----------------------------------------------

    # Checking main input object
    stopifnot(inherits(x, c("eupp_config", "eupp_inventory")))

    # Sanity check for putput file
    stopifnot(is.character(output_file), length(output_file) == 1L)
    if (dir.exists(output_file)) stop("'output_file' is an existing directory.")
    # TODO: We can also just ... kill it?
    if (file.exists(output_file)) stop("'output_file' exists.")
    # Checking if output path exists
    if (!dir.exists(dirname(output_file)))
        stop("Cannot write 'output_file' to \"{:s}\", directory does not exist.", dirname(output_file))

    # Now guessing output file type
    if (grepl("[;<>]", output_file)) stop("'output_file' contains illegal characters.")
    output_format <- match.arg(output_format)
    if (output_format == "guess") {
        # pattern matches grb, grib, GRIB, GRIB1 ...
        fext <- file_ext(output_file)
        if (length(regmatches(fext, gregexpr("[grb]", fext, ignore.case = TRUE))[[1]]) >= 3) {
            output_format <- "grb"
        } else if (length(regmatches(fext, gregexpr("[nc]", fext, ignore.case = TRUE))[[1]]) >= 2) {
            output_format <- "nc"
        } else {
            stop("Please provide 'output_format'.")
        }
    }

    # If the user requests netCDF: check grib_to_netcdf is available.
    if (output_format == "nc") {
        g2nc_bin <- Sys.which("grib_to_netcdf")
        if (nchar(g2nc_bin) == 0) {
            # Stop
            stop("To be able to use 'output_format = \"nc\" you must install 'grib_to_netcdf' from ECMWFs ecCodes toolbox.")
        }
    }

    # We will make use of stars for spatial subsets which
    # is only possible if we store the data as NetCDF.
    if (output_format == "nc" & !is.null(x$area))
        stop("Areal subsets ('eupp_config area') only allowed in combination with NetCDF file format.")

    # Reforecasts only initialized on Mondays (1) and Thursdays (4)
    if (grepl(x$type, "^reforecast$") & !all(format(x$date, "%w") %in% c(1, 4)))
        stop("Reforecasts only available on Mondays and Thursdays, check 'date'.")

    # ----------------------------------------------
    # Main content of the function
    # ----------------------------------------------
    inv       <- get_inventory(x)           # Loading inventory information
    grib_url  <- get_source_url(x)          # Getting data url (location of grib file)
    tmp_file <- tempfile(fileext = ".grb")  # Temporary location for download
    print(tmp_file)

    # Open binary file connection; temporary file.
    # Download everything and then create final ouptut file.
    con      <- file(tmp_file, "wb"); on.exit(close(con))
    pb <- txtProgressBar(0, nrow(inv), style = 3)
    for (i in seq_len(nrow(inv))) {
        setTxtProgressBar(pb, i)
        rng <- sprintf("bytes=%d-%d", inv$offset[i], inv$offset[i] + inv$length[i])
        req <- GET(grib_url, add_headers(Range = rng))
        writeBin(req$content, con = con)
    }
    close(pb)

    # Move file to final destination
    if (output_format == "grb") {
        file.rename(tmp_file, output_file)
    } else {
        cat("Calling grib_to_netcdf to convert file format\n")
        system(sprintf("grib_to_netcdf %s -o %s", tmp_file, output_file), intern = !verbose)
    }

    unlink(tmp_file)         # Delete temporary file
    invisible(output_file)   # Return final file name

}



# -----------------------------------------------------------------------
# -----------------------------------------------------------------------

#' @rdname download
#' @importFrom dplyr bind_rows
#' @importFrom rjson fromJSON
#' @importFrom digest digest
#' @importFrom httr GET status_code content
#' @author Reto Stauffer
#' @export
get_inventory <- function(x) {

    stopifnot(inherits(x, "eupp_config"))

    # Getting the URL where the grib file index is stored
    index_url <- do.call(get_source_url, list(x = x, fileext = "index"))

    # Helper function to download and parse requests
    fn_GET <- function(x) {
        request <- GET(x)
        if (!status_code(request) == 200L)
            stop(sprintf("Problems accessing \"%s\"; return code %d.", x, status_code(request)))
        tmp <- readLines(con = textConnection(content(request, "text", encoding = "UTF-8")))
        return(tmp[sapply(tmp, nchar) > 0])
    }

    # In case caching is used:
    # - hashing URL for unique file names
    # - If file does exist on disc: read file
    # - Else download data; save to cache file for next time
    if (is.character(x$cache)) {
        # Create file <cache_dir>/<hashed_url>-<version>.index
        cached_file <- file.path(x$cache,
                                 sprintf("%s-%s.index", digest(index_url), x$version))
        if (file.exists(cached_file)) {
            inv <- readLines(cached_file)
        } else {
            inv <- fn_GET(index_url)
            writeLines(inv, con = cached_file)
        }
    # In case cache is NULL (default): request and extract
    } else {
        inv <- fn_GET(index_url)
    }

    # - Find non-empty rows (last is empty) and decode JSON string
    inv <- lapply(inv, fromJSON)
    # - Convert to data.frame and fix leading underscores (not good)
    inv <- as.data.frame(bind_rows(inv))
    names(inv) <- gsub("^_(?=[a-zA-Z])", "", names(inv), perl = TRUE)
    inv <- transform(inv, init  = as.POSIXct(paste(date, time), tz = "UTC", format = "%Y%m%d %H%M"))
    inv <- transform(inv, valid = init + as.integer(step) * 3600)
    inv <- within(inv, {date <- time <- NULL})

    # Subsetting to what the user has requested
    inv <- subset(inv, init %in% x$date & param %in% x$parameter)
    class(inv) <- c("eupp_inventory", class(inv))
    return(inv)
}

# -----------------------------------------------------------------------
# -----------------------------------------------------------------------

######' @param x character, length \code{1}. Either \code{reforecast} or \code{forecast}.
######' @param level character, length \code{1}. Allowed are \code{surface}, \code{pressure}
######'        and \code{efi} (extreme forecast index).
######' @param date object of class \code{character}, \code{Date}, or inheriting from
######'        \code{POSIXt}.
######' @param version integer, defaults to \code{0L}.
######' @param cache \code{NULL} or single character, used for data caching. Defaults to \code{NULL}.
######'
######' @rdname download
######' @author Reto Stauffer
######
######download_inputcheck <- function(x     = c("reforecast", "forecast", "analysis"),
######                                kind  = c("ctr", "ens", "hr"),
######                                level = c("surface", "pressure", "efi"),
######                                date, parameter, version = 0L, cache = NULL) {
######
######    # ----------------------------------------------
######    # Sanity checks:
######    # ----------------------------------------------
######    stopifnot(is.character(x),          length(x) == 1L)
######    stopifnot(is.character(kind),       length(kind) == 1L)
######    stopifnot(is.character(level),      length(level) == 1L)
######    stopifnot(is.character(parameter),  length(parameter) > 0L)
######    stopifnot(is.integer(version),      length(version) == 1L)
######    stopifnot(inherits(cache, c("NULL", "character")))
######    if (!is.null(cache)) {
######        stopifnot(length(cache) == 1L)
######        if (!dir.exists(cache))
######            stop("Argument 'code' must point to an existing folder if set.")
######    }
######
######    x     <- match.arg(x)
######    kind  <- match.arg(kind)
######    level <- match.arg(level)
######    if (anyDuplicated(parameter)) {
######        warning("Got duplicated parameters; unified.")
######        parameter <- unique(parameter)
######    }
######
######    #@TODO: Allow to download multiple dates at once
######    # Make sure input for 'date' is allowed.
######    stopifnot(inherits(date, c("character", "Date", "POSIXt")), length(date) == 1L)
######    # If input 'date' is character; try to convert to character.
######    if (is.character(date)) {
######        tryCatch(date <- as.POSIXct(date),
######                 warning = function(w) warning(w),
######                 error   = function(e) stop("Input 'date' not recognized; use ISO format."))
######    # If input was of class Date or POSIXlt: convert to POSIXct.
######    } else if (!inherits(date, "POSIXct")) {
######        date <- as.POSIXct(date)
######    }
######
######    return(list(x = x, kind = kind, level = level, date = date,
######                parameter = parameter, version = version, cache = cache))
######}




