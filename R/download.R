


#' @param x character, length \code{1}. Either \code{reforecasts} or \code{forecasts}.
#' @param level character, length \code{1}. For now only \code{surface}.
#' @param date object of class \code{character}, \code{Date}, or inheriting from
#'        \code{POSIXt}.
#'
#' @details Using partial matching for \code{x} and \code{level}.
#'
#' @author Reto Stauffer
download_dataset <- function(x = c("reforecasts", "forecasts"), level = c("surface"), date, parameters) {

    # ----------------------------------------------
    # Sanity checks:
    # ----------------------------------------------
    stopifnot(is.character(x),     length(x) == 1L)
    stopifnot(is.character(level), length(level) == 1L)
    stopifnot(is.character(parameters), length(parameters) == 1L)

    level <- match.arg(level)


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

    # ----------------------------------------------
    # Getting public URL
    # ----------------------------------------------
    if      (x == "reforecasts")              { fmt = "%Y-%m-%d" }
    else if (x == "forecasts")                { fmt = "%Y-%m-%d" }
    else if (x == "analysis")                 { fmt = "%Y-%m"    }
    URL <- get_data_url(x, level, format(date, fmt), parameter)
    print(URL)

}

