


#' @param x character, length \code{1}. Either \code{reforecast} or \code{forecast}.
#' @param level character, length \code{1}. Allowed are \code{surface}, \code{pressure}
#'        and \code{efi} (extreme forecast index).
#' @param date object of class \code{character}, \code{Date}, or inheriting from
#'        \code{POSIXt}.
#'
#' @details Using partial matching for \code{x} and \code{level}.
#'
#' @importFrom httr GET
#' @author Reto Stauffer
download_dataset <- function(x     = c("reforecast", "forecast", "analysis"),
                             level = c("surface", "pressure", "efi"),
                             date, parameters) {

    # ----------------------------------------------
    # Sanity checks:
    # ----------------------------------------------
    stopifnot(is.character(x),          length(x) == 1L)
    stopifnot(is.character(level),      length(level) == 1L)
    stopifnot(is.character(parameters), length(parameters) > 0L)

    x     <- match.arg(x)
    level <- match.arg(level)
    if (anyDuplicated(parameters)) {
        warning("Got duplicated parameters; unified.")
        parameters <- unique(parameters)
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
    if (grepl(x, "^reforecast$") & !all(format(date, "%w") %in% c(1, 4)))
        stop("Reforecasts only available on Mondays and Thursdays, check 'date'.")

    # ----------------------------------------------
    # Getting public URL
    # ----------------------------------------------
    index_url <- get_source_url(x, level, date, parameter, fileext = "index")
    grib_url  <- get_source_url(x, level, date, parameter)


    x <- GET(index_url)
    print(x$status)
    print(x)
}

