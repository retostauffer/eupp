
#' Downloading EUPP Datasets
#'
#' Main function downloading data.
#'
#' @param x an object of class \code{\link{eupp_config}}.
#' @param output_file character length 1, name of the output file.
#' @param output_format character length 1 (\code{"grib"} or \code{"nc"}),
#'        defaults to \code{"grib"} (see details).
#' @param verbose logical length 1, verbosity, defaults to \code{FALSE}.
#' @param overwrite logical length 1, defaults to \code{FALSE}. If set to \code{TRUE}
#'        the \code{output_file} will be overwritten if needed. If \code{FALSE} and
#'        \code{output_file} exists an error will be raised.
#'
#' @details The function allows to store data sets in either GRIB version 1 or
#' NetCDF (classic 64bit; v3). The original data set is provided as GRIB, the 
#' conversion to NetCDF is done locally usning ECMWFs ecCodes tools which must
#' be installed when using NetCDF.
#'
#' @rdname download
#' @importFrom httr GET add_headers
#' @importFrom tools file_ext
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @author Reto Stauffer
#' @rdname gridded
#' @export
eupp_download_gridded <- function(x,
                             output_file,
                             output_format = c("grib", "nc"),
                             verbose = FALSE, overwrite = FALSE) {

    # Checking main input object
    stopifnot(inherits(x, c("eupp_config", "eupp_inventory")))
    stopifnot(isTRUE(verbose)   || isFALSE(verbose))
    stopifnot(isTRUE(overwrite) || isFALSE(overwrite))

    # Sanity check for putput file
    stopifnot(is.character(output_file), length(output_file) == 1L)
    if (dir.exists(output_file)) stop("'output_file' is an existing directory.")
    # TODO: We can also just ... kill it?
    if (!overwrite && file.exists(output_file)) stop("'output_file' exists.")
    # Checking if output path exists
    if (!dir.exists(dirname(output_file)))
        stop("Cannot write 'output_file' to \"{:s}\", directory does not exist.", dirname(output_file))

    # Now guessing output file type
    if (grepl("[;<>]", output_file)) stop("'output_file' contains illegal characters.")
    output_format <- match.arg(output_format)

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
    if (output_format != "nc" & !is.null(x$area))
        stop("Areal subsets ('eupp_config area') only allowed in combination with NetCDF file format.")

    # Reforecasts only initialized on Mondays (1) and Thursdays (4)
    if (grepl("^reforecast$", x$type) && !all(format(x$date, "%w") %in% c(1, 4)))
        stop("Reforecasts only available on Mondays and Thursdays, check 'date'.")

    # ----------------------------------------------
    # Main content of the function
    # ----------------------------------------------
    inv       <- eupp_get_inventory(x)           # Loading inventory information
    grib_url  <- eupp_get_source_url(x)          # Getting data url (location of grib file)
    tmp_file <- tempfile(fileext = ".grb")  # Temporary location for download

    # Open binary file connection; temporary file.
    # Download everything and then create final ouptut file.
    con      <- file(tmp_file, "wb")
    if (verbose) pb <- txtProgressBar(0, nrow(inv), style = 3)
    for (i in seq_len(nrow(inv))) {
        if (verbose) setTxtProgressBar(pb, i)
        rng <- sprintf("bytes=%d-%d", inv$offset[i], inv$offset[i] + inv$length[i])
        req <- GET(grib_url, add_headers(Range = rng))
        writeBin(req$content, con = con)
    }
    if (verbose) close(pb)
    close(con) # Properly closing the binary file connection

    # Move file to final destination
    if (output_format == "grib") {
        file.rename(tmp_file, output_file)
    } else {
        cat("Calling grib_to_netcdf to convert file format\n")
        system(sprintf("grib_to_netcdf %s -o %s", tmp_file, output_file), intern = !verbose)
    }

    unlink(tmp_file)         # Delete temporary file
    invisible(output_file)   # Return final file name

}


#' Getting Grib Data as stars
#'
#' Requires ncdf4 and the NetCDF Library to be installed.
#'
#' @param x object of class \code{\link{eupp_config}}.
#' @param verbose logical, sets verbosity level. Defaults to \code{FALSE}.
#'
#' @return Object of class \code{stars}; see Details.
#'
#' @details This function interfaces \code{\link{eupp_download_gridded}}
#' to download the data and converts the original data set (GRIB version 1)
#' to NetCDF. Thus NetCDF support and ecCodes tools are required.
#'
#' If \code{x} (\code{\link{eupp_config}}) contains a \code{bbox} definition
#' on \code{area} the data is subsetted directly.
#'
#' @seealso \code{\link{eupp_download_gridded}}
#'
#' @importFrom stars read_stars
#' @author Reto Stauffer
#' @rdname gridded
#' @export
eupp_get_gridded <- function(x, verbose = FALSE) {
    stopifnot(inherits(x, "eupp_config"))
    stopifnot(isTRUE(verbose) || isFALSE(verbose))
    stopifnot(requireNamespace("stars", quietly = TRUE))

    tmp_file <- tempfile(fileext = ".nc")
    tmp_file <- eupp_download_gridded(x, tmp_file, "nc", verbose = verbose)

    # Reading the NetCDF file as stars
    data <- stars::read_stars(tmp_file)

    # Return; perform subsetting if required
    return(if (!is.null(x$area)) data[x$area] else data)
}


#' @rdname gridded
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

    # Extracting 'step' as integer. We either only get an integer (as text)
    # such as "0" or "12", or a step range like "0-12" or "24-48". In both
    # cases we extract the last numeric part and store it on 'step' again.
    # To keep the old 'step' (from the index file) we will rename it to 'step_char'.
    names(inv)[names(inv) == "step"] <- "step_char"
    inv$step <- as.integer(regmatches(inv$step_char, regexpr("[0-9]+$", inv$step_char))) # <- integer
    inv <- transform(inv, valid = init + step * 3600)
    inv <- transform(inv, step  = as.integer(inv$step))
    inv <- within(inv, {date <- time <- NULL})

    # Subsetting to what the user has requested
    inv <- subset(inv, init %in% x$date)
    if (is.character(x$parameter))     inv <- subset(inv, param %in% x$parameter)
    if (is.integer(x$steps))           inv <- subset(inv, step  %in% x$steps)

    class(inv) <- c("eupp_inventory", class(inv))
    if (nrow(inv) == 0) warning("No field match found; check your 'eupp_config()' settings.")
    return(inv)
}



