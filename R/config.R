

#' Creates EUPP Config used as Data Descriptor
#'
#' Used to retrieve data and inventories. 
#'
#' @author Reto Stauffer
#' @export
eupp_config <- function(type  = c("reforecast", "forecast", "analysis"),
                        kind  = c("ctr", "ens", "hr"),
                        level = c("surf", "pressure", "efi"),
                        date, parameter,
                        version = 0L, cache = NULL) {

    # ----------------------------------------------
    # Sanity checks:
    # ----------------------------------------------
    stopifnot(is.character(type),       length(type) == 1L)
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

    type  <- match.arg(type)
    kind  <- match.arg(kind)
    level <- match.arg(level)
    if (anyDuplicated(parameter)) {
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
    # If input was of class Date or POSIXlt: convert to POSIXct.
    } else if (!inherits(date, "POSIXct")) {
        date <- as.POSIXct(date)
    }

    # Store abbrevation of the type to generate the URLs
    type_abbr  <- c(reforecast = "rfcs", forecast = "fcs", analysis = "ana")[type]

    # Crete return; a simple list of class eupp_config.
    res <- list(type = type, type_abbr = type_abbr,
                kind = kind, level = level, date = date,
                parameter = parameter, version = version, cache = cache)
    class(res) <- "eupp_config"
    return(res)
}


#' @export
print.eupp_config <- function(x, ...) {
    fmt <- "   %-20s %s"
    res <- c("EUPP Config",
             sprintf(fmt, "Type:", sprintf("%s (%s)", x$type, x$type_abbr)),
             sprintf(fmt, "Kind:", x$kind),
             sprintf(fmt, "Level:", x$level),
             sprintf(fmt, "Parameter:", paste(x$parameter, collapse = ", ")),
             sprintf(fmt, "Version:", as.character(x$version)),
             sprintf(fmt, "Cache:", ifelse(is.null(x$cache), "disabled", x$cache)))
    cat(res, sep = "\n")
    invisible(x)
}
