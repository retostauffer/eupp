

#' Creates EUPP Config used as Data Descriptor
#'
#' Generates an object of class \code{eupp_config} Containing all the required
#' settings for downloadin data set inventories (index files) and the data
#' itself.
#'
#' @param product character of length \code{1} (see Details).
#' @param level character of length \code{1} or \code{NULL} (see Details).
#' @param type character of length \code{1} (see Details).
#' @param date an object which can be converted to \code{POSIXct}. Character string
#'        in ISO representation or an object of class \code{Date} or \code{POSIXt}.
#' @param parameter \code{NULL} (default) or character. When \code{NULL} all available
#'        parameters will be processed. Can be used to retrieve/process subsets.
#' @param steps \code{NULL} (default) or integer. If set to \code{NULL} all available forecast
#'        steps will be processed. An integer sequence can be provided to only process
#'        specific forecast steps; given in hours (e.g., \code{c(6, 12)} for \code{+6}
#'        and \code{+12} hour ahead forecasts).
#' @param members \code{NULL} (default) or integer. If set to \code{NULL} all available forecast
#'        members will be processed. An integer sequence can be provided to only process
#'        specific forecast members.
#' @param area \code{NULL} (default) or an object of class \code{bbox}. Used for spatial subsetting.
#' @param cache \code{NULL} or character of length \code{1} pointing to an existing
#'        directory. Is used for data caching (caching grib index information/inventories)
#'        which can be handy to save some time.
#' @param version integer lengt \code{1}; version of the data set. Defaults to \code{0L}.
#'        Typically not changed by an end-user.
#'
#' @return Returns an object of class \code{eupp_config}.
#'
#' @details Input argument \code{product}:
#'
#' \itemize{
#'   \item \code{"reforecast"}: Reforecasts (or hindcasts).
#'   \item \code{"forecast"}: Forecast data.
#'   \item \code{"analysis"}: Analysis data; based on ECMWF ERA5 used as ground truth.
#' }
#'
#' Input argument \code{level}. Will be ignored if \code{type = "efi"} (has no level).
#'
#' \itemize{
#'   \item \code{"efi"}: Extreme forecast index (in this situation \code{type} will be ignored).
#'   \item \code{"surf"}: Surface data.
#'   \item \code{"pressure"}: Pressure level data.
#' }
#'
#' Input argument \code{type}:
#'
#' \itemize{
#'   \item \code{"ens"}: Ensemble members including control run (allows for optional argument \code{members}).
#'   \item \code{"hr"}: High-resolution forecast.
#' }
#'
#'
#' Note that reforecasts always only become available on Mondays and Thursdays.
#' If \code{type = "reforecast"} and the \code{date} does not point to Mon/Thu
#' the function will throw an error.
#' TODO(R): This is not true, this happens when processing the data.
#'
#' Ensemble forecasts (\code{type = "ens"} consists of both, the ensemble control
#' run plus \code{50} ensemble members if \code{members} is not specified.
#' Same is true for reforecasts except that it only consists of \code{20} ensemble
#' members. Possible combinations:
#'
#'
#' | ID | product    | level      | type   |
#' | -- | ---------- | ---------- | ------ |
#' |  1 | analysis   | surface    | NULL   |
#' |  2 |            | pressure   | NULL   |
#' | 11 | forecast   | efi        |        |
#' | 12 |            | surface    | ens    |
#' | 13 |            |            | hr     |
#' | 14 |            | pressure   | ens    |
#' | 15 |            |            | hr     |
#' | 21 | reforecast | surface    | ens    |
#' | 22 |            | pressure   | ens    |
#'
#' @examples
#' # Analysis
#' (c1 <- eupp_config("analysis", "surface", date = "2017-01-01", steps = 12)
#' (c2 <- eupp_config("analysis", "pressure", date = "2017-01-01", steps = 12)
#'
#' # Forecasts
#' (c11 <- eupp_config("forecast", "efi", date = "2017-01-01", steps = 12)
#' (c12 <- eupp_config("forecast", "surface", "ens", date = "2017-01-01", steps = 12)
#' (c13 <- eupp_config("forecast", "surface", "hr", date = "2017-01-01", steps = 12)
#' (c14 <- eupp_config("forecast", "pressure", "ens", date = "2017-01-01", steps = 12)
#' (c15 <- eupp_config("forecast", "pressure", "hr", date = "2017-01-01", steps = 12)
#'
#' # Reforecasts
#' (c21 <- eupp_config("reforecast", "surface", "ens", date = "2017-01-01", steps = 12)
#' (c22 <- eupp_config("reforecast", "pressure", "ens", date = "2017-01-01", steps = 12)
#'
#' @author Reto Stauffer
#' @export
eupp_config <- function(product  = c("analysis", "forecast", "reforecast"),
                        level    = c("surface", "pressure", "efi"),
                        type     = c("ens", "hr"),
                        date, parameter = NULL,
                        steps = NULL, members = NULL,
                        area = NULL, cache = NULL, version = 0L) {

    # ----------------------------------------------
    # Sanity checks:
    # ----------------------------------------------
    stopifnot(is.character(product), length(product) == 1L)
    product  <- match.arg(product)

    stopifnot(is.character(level), length(level) == 1L)
    level <- match.arg(level)

    if (product == "analysis" || level == "efi") {
        type <- NULL # Ignored
    } else {
        stopifnot(is.character(type), length(type) == 1L)
        type  <- match.arg(type)
    }

    stopifnot(inherits(parameter,  c("NULL", "character")))
    stopifnot(inherits(area,       c("NULL", "bbox")))
    stopifnot(inherits(steps,      c("NULL", "numeric", "integer")))
    stopifnot(inherits(members,    c("NULL", "numeric", "integer")))

    if (!is.null(steps))   { stopifnot(all(steps   >= 0)); steps   <- as.integer(steps) }
    if (!is.null(members)) { stopifnot(all(members >= 0)); members <- as.integer(members) }

    stopifnot(is.integer(version),      length(version) == 1L)
    stopifnot(inherits(cache, c("NULL", "character")))
    if (!is.null(cache)) {
        stopifnot(length(cache) == 1L)
        if (!dir.exists(cache))
            stop("Argument 'cache' must point to an existing folder if set.")
    }

    if (anyDuplicated(parameter)) {
        warning("Got duplicated parameters; unified.")
        parameter <- unique(parameter)
    }

    #@TODO: Allow to download multiple dates at once
    #       Idea: allow for POSIXt vector; take unique(URLs)
    #       and start to download Put everything into one file??????
    # Make sure input for 'date' is allowed.
    stopifnot(inherits(date, c("character", "Date", "POSIXt")), length(date) == 1L)

    # If input 'date' is character; try to convert to character.
    if (is.character(date)) {
        tryCatch(date <- as.POSIXct(date, tz = "UTC"),
                 warning = function(w) warning(w),
                 error   = function(e) stop("Input 'date' not recognized; use ISO format."))
    # If input was of class Date or POSIXlt: convert to POSIXct.
    } else if (!inherits(date, "POSIXct")) {
        date <- as.POSIXct(date)
    }

    # Analysis: requires 'Date', hours are controlled via steps.
    if (product == "analysis" && !all(as.POSIXct(as.Date(date)) == date))
        stop("For product = \"analysis\" the 'date' argument must be date or 00 UTC; hours controlled via argument 'step'.")

    # Store abbrevation of the type to generate the URLs
    product_abbr  <- c(reforecast = "rfcs", forecast = "fcs", analysis = "ana")[product]

    # Crete return; a simple list of class eupp_config.
    res <- list(product = product, product_abbr = product_abbr,
                level = level, type = type, date = date, steps = steps,
                members = members, area = area,
                parameter = parameter, version = version, cache = cache)
    class(res) <- "eupp_config"
    return(res)
}


#' @export
print.eupp_config <- function(x, ...) {
    fmt <- "   %-20s %s"
    res <- c("EUPP Config",
             sprintf(fmt, "Product:",   sprintf("%s (%s)", x$product, x$product_abbr)),
             sprintf(fmt, "Level:",     x$level),
             sprintf(fmt, "Type:",      x$type),
             sprintf(fmt, "Parameter:", ifelse(is.null(x$parameter), "all available",
                                               paste(x$parameter, collapse = ", "))),
             sprintf(fmt, if (x$product == "analysis") "Hours:" else "Steps:",     ifelse(is.null(x$steps),     "all available",
                                               paste(x$steps, collapse = ", "))),
             if (is.character(x$type) && x$type == "ens") 
                sprintf(fmt, "Members:",   ifelse(is.null(x$members),     "all available",
                                                  paste(x$members, collapse = ", "))) else NULL,
             sprintf(fmt, "Version:",   as.character(x$version)),
             sprintf(fmt, "Cache:",     ifelse(is.null(x$cache), "disabled", x$cache)),
             sprintf(fmt, "Area:",      ifelse(is.null(x$area), "not defined", "defined!")))
    cat(res, sep = "\n")
    invisible(x)
}
