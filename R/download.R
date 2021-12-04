



# -----------------------------------------------------------------------
# -----------------------------------------------------------------------

#'
#' @details Using partial matching for \code{x} and \code{level}.
#'
#' @rdname download
#' @importFrom httr GET
#' @author Reto Stauffer
#' @export
download_dataset <- function(x, kind, level, date, parameter, version = 0L) {

    # Sanity checks
    inputargs <- download_inputcheck(x, kind, level, date, parameter, version)
    for (n in names(inputargs)) eval(parse(text = sprintf("%1$s <- inputargs[[\"%1$s\"]]", n)))

    # Reforecasts only initialized on Mondays (1) and Thursdays (4)
    if (grepl(x, "^reforecast$") & !all(format(date, "%w") %in% c(1, 4)))
        stop("Reforecasts only available on Mondays and Thursdays, check 'date'.")

    # ----------------------------------------------
    # Getting public URL
    # ----------------------------------------------
    print(c(list(fileext = "index"), inputargs))
    index_url <- do.call(get_source_url, c(list(fileext = "index"), inputargs))
    grib_url  <- do.call(get_source_url, inputargs)


}



# -----------------------------------------------------------------------
# -----------------------------------------------------------------------

#' @rdname download
#' @importFrom dplyr bind_rows
#' @importFrom rjson fromJSON
#' @importFrom digest digest
#' @author Reto Stauffer
#' @export
get_inventory <- function(x, kind, level, date, parameter, version = 0L, cache = NULL) {

    # Sanity checks
    inputargs <- download_inputcheck(x, kind, level, date, parameter, version, cache)
    for (n in names(inputargs)) eval(parse(text = sprintf("%1$s <- inputargs[[\"%1$s\"]]", n)))

    # Getting the URL where the grib file index is stored
    index_url <- do.call(get_source_url, c("fileext" = "index", inputargs))

    # Helper function to download and parse requests
    fn_GET <- function(x) {
        request <- GET(x)
        if (!request$status_code == 200L)
            stop(sprintf("Problems accessing \"%s\"; return code %d.", x, request$status_code))
        tmp <- readLines(con = textConnection(content(request, "text", encoding = "UTF-8")))
        return(tmp[sapply(tmp, nchar) > 0])
    }

    # In case caching is used:
    # - hashing URL for unique file names
    # - If file does exist on disc: read file
    # - Else download data; save to cache file for next time
    if (is.character(cache)) {
        cached_file <- paste(file.path(cache, digest(index_url)), "index", sep = ".")
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
    inv <- subset(inv, init %in% date & param %in% parameter)
    return(inv)
}

# -----------------------------------------------------------------------
# -----------------------------------------------------------------------

#' @param x character, length \code{1}. Either \code{reforecast} or \code{forecast}.
#' @param level character, length \code{1}. Allowed are \code{surface}, \code{pressure}
#'        and \code{efi} (extreme forecast index).
#' @param date object of class \code{character}, \code{Date}, or inheriting from
#'        \code{POSIXt}.
#' @param version integer, defaults to \code{0L}.
#' @param cache \code{NULL} or single character, used for data caching. Defaults to \code{NULL}.
#'
#' @rdname download
#' @author Reto Stauffer
download_inputcheck <- function(x     = c("reforecast", "forecast", "analysis"),
                                kind  = c("ctr", "ens", "hr"),
                                level = c("surface", "pressure", "efi"),
                                date, parameter, version = 0L, cache = NULL) {

    # ----------------------------------------------
    # Sanity checks:
    # ----------------------------------------------
    stopifnot(is.character(x),          length(x) == 1L)
    stopifnot(is.character(kind),       length(kind) == 1L)
    stopifnot(is.character(level),      length(level) == 1L)
    stopifnot(is.character(parameter),  length(parameter) > 0L)
    stopifnot(is.integer(version),      length(version) == 1L)
    stopifnot(inherits(cache, c("NULL", "character")))
    if (!is.null(cache)) {
        stopifnot(length(cache) == 1L)
        if (!dir.exists(cache))
            stop("Argument 'code' must point to an existing folder if set.")
    }

    x     <- match.arg(x)
    kind  <- match.arg(kind)
    level <- match.arg(level)
    if (anyDuplicated(parameters)) {
        warning("Got duplicated parameters; unified.")
        parameter <- unique(parameter)
    }

    #@TODO: Allow to download multiple dates at once
    # Make sure input for 'date' is allowed.
    stopifnot(inherits(date, c("character", "Date", "POSIXt")), length(date) == 1L)
    # If input 'date' is character; try to convert to character.
    if (is.character(date)) {
        tryCatch(date <- as.POSIXct(date),
                 warning = function(w) warning(w),
                 error   = function(e) stop("Input 'date' not recognized; use ISO format."))

        print(date)
        print(class(date))
    # If input was of class Date or POSIXlt: convert to POSIXct.
    } else if (!inherits(date, "POSIXct")) {
        date <- as.POSIXct(date)
    }

    return(list(x = x, kind = kind, level = level, date = date,
                parameter = parameter, version = version, cache = cache))
}
