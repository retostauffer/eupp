



# -----------------------------------------------------------------------
# -----------------------------------------------------------------------

#' Downloading EUPP Datasets
#'
#' Main function downloading data.
#'
#' @param x an object of class \code{\link{eupp_config}}.
#' @param output_file character length 1, name of the output file.
#' @param output_format character length 1, defaults to \code{"guess"} (see details).
#' @param verbose logical length 1, verbosity, defaults to \code{FALSE}.
#'
#' @details The function allows to store data sets in either GRIB version 1 or
#' NetCDF (classic 64bit; v3). The original data set is provided as GRIB, the 
#' conversion to NetCDF is done locally usning ECMWFs ecCodes tools which must
#' be installed when using NetCDF.
#'
#' By default (\code{output_format = "guess"}) the function tires to guess the
#' \code{output_format} based on the \code{output_file} name and will stop if
#' not possible. You can always use \code{output_format} to control this explicitly.
#'
#' \itemize{
#'    \item \code{output_format = "guess"}: Guess output format based on \code{output_file}.
#'    \item \code{output_format = "grb"}: Store result in GRIB version 1.
#'    \item \code{output_format = "nc"}: Store result as NetCDF file.
#' }
#'
#' @rdname download
#' @importFrom httr GET add_headers
#' @importFrom tools file_ext
#'
#' @author Reto Stauffer
#' @export
eupp_download_dataset <- function(x,
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
    inv       <- eupp_get_inventory(x)           # Loading inventory information
    grib_url  <- eupp_get_source_url(x)          # Getting data url (location of grib file)
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
#' @export
eupp_get_inventory <- function(x) {

    stopifnot(inherits(x, "eupp_config"))

    # Getting the URL where the grib file index is stored
    index_url <- do.call(eupp_get_source_url, list(x = x, fileext = "index"))

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




