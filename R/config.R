

#' Creates EUPP Config used as Data Descriptor
#'
#' Generates an object of class \code{eupp_config} Containing all the required
#' settings for downloadin data set inventories (index files) and the data
#' itself.
#'
#' @param type character of length \code{1} (see Details).
#' @param kind character of length \code{1} or \code{NULL} (see Details).
#' @param level character of length \code{1} (see Details).
#' @param date an object which can be converted to \code{POSIXct}. Character string
#'        in ISO representation or an object of class \code{Date} or \code{POSIXt}.
#' @param parameter \code{NULL} (default) or character. When \code{NULL} all available
#'        parameters will be processed. Can be used to retrieve/process subsets.
#' @param steps \code{NULL} (default) or integer. If set to \code{NULL} all available forecast
#'        steps will be processed. An integer sequence can be provided to only process
#'        specific forecast steps; given in hours (e.g., \code{c(6, 12)} for \code{+6}
#'        and \code{+12} hour ahead forecasts).
#' @param steps \code{NULL} (default) or integer. If set to \code{NULL} all available forecast
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
#' @details Input argument \code{type}:
#'
#' \itemize{
#'   \item \code{"reforecast"}: Reforecasts (or hindcasts).
#'   \item \code{"forecast"}: Forecast data.
#'   \item \code{"analysis"}: Analysis data; based on ECMWF ERA5 used as ground truth.
#' }
#'
#' Input argument \code{kind}. Will be ignored if \code{level = "efi"} (has no kind).
#'
#' \itemize{
#'   \item \code{"ctr"}: Control run.
#'   \item \code{"ens"}: Ensemble members (allows for optional argument \code{members}).
#'   \item \code{"hr"}: High-resolution forecast.
#' }
#'
#' Input argument \code{level}:
#'
#' \itemize{
#'   \item \code{"surf"}: Surface data.
#'   \item \code{"pressure"}: Pressure level data.
#'   \item \code{"efi"}: Extreme forecast index data.
#' }
#'
#' Note that reforecasts always only become available on Mondays and Thursdays.
#' If \code{type = "reforecast"} and the \code{date} does not point to Mon/Thu
#' the function will throw an error.
#'
#' The forecast ensemble (\code{type = "forecast"}, \code{kind = "ctr"}) consists
#' of 50 ensemble members (\code{1-50}), the reforecasts or hindcasts
#' (\code{type = "forecast"}, \code{kind = "ctr"}) only consists of 20 ensemble
#' runs (\code{1-20}).
#'
#' @examples
#' conf_A <- eupp_config(type = "reforecast", kind = "ctr", level = "surf",
#'                       date = "2017-01-02", parameter = "2t")
#'
#' conf_B <- eupp_config(type = "reforecast", kind = "ctr", level = "surf",
#'                       date = "2017-01-02", parameter = "2t",
#'                       steps = c(6, 12))
#'
#' @author Reto Stauffer
#' @export
eupp_config <- function(type  = c("reforecast", "forecast", "analysis"),
                        kind  = c("ctr", "ens", "hr"),
                        level = c("surf", "pressure", "efi"),
                        date, parameter = NULL,
                        steps = NULL, members = NULL,
                        area = NULL, cache = NULL, version = 0L) {

    # ----------------------------------------------
    # Sanity checks:
    # ----------------------------------------------
    stopifnot(is.character(type), length(type) == 1L)
    type  <- match.arg(type)
    stopifnot(is.character(level), length(level) == 1L)
    level <- match.arg(level)

    if (level == "efi") {
        if (!is.null(kind)) stop("Please set 'kind = NULL' when requesting 'level = \"efi\"'.")
    } else {
        stopifnot(is.character(kind), length(kind) == 1L)
        kind  <- match.arg(kind)
    }

    stopifnot(inherits(parameter,  c("NULL", "character")))
    stopifnot(inherits(area,       c("NULL", "bbox")))
    stopifnot(inherits(steps,      c("NULL", "numeric", "integer")))
    stopifnot(inherits(members,    c("NULL", "numeric", "integer")))

    if (!is.null(steps))   { stopifnot(all(steps   >= 0)); steps   <- as.integer(steps) }
    if (!is.null(members)) { stopifnot(all(members >  0)); members <- as.integer(members) }

    stopifnot(is.integer(version),      length(version) == 1L)
    stopifnot(inherits(cache, c("NULL", "character")))
    if (!is.null(cache)) {
        stopifnot(length(cache) == 1L)
        if (!dir.exists(cache))
            stop("Argument 'code' must point to an existing folder if set.")
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

    # Store abbrevation of the type to generate the URLs
    type_abbr  <- c(reforecast = "rfcs", forecast = "fcs", analysis = "ana")[type]

    # Crete return; a simple list of class eupp_config.
    res <- list(type = type, type_abbr = type_abbr,
                kind = kind, level = level, date = date, steps = steps, area = area,
                parameter = parameter, version = version, cache = cache)
    class(res) <- "eupp_config"
    return(res)
}


#' @export
print.eupp_config <- function(x, ...) {
    fmt <- "   %-20s %s"
    res <- c("EUPP Config",
             sprintf(fmt, "Type:",      sprintf("%s (%s)", x$type, x$type_abbr)),
             sprintf(fmt, "Kind:",      x$kind),
             sprintf(fmt, "Level:",     x$level),
             sprintf(fmt, "Parameter:", ifelse(is.null(x$parameter), "all available",
                                               paste(x$parameter, collapse = ", "))),
             sprintf(fmt, "Steps:",     ifelse(is.null(x$steps),     "all available",
                                               paste(x$steps, collapse = ", "))),
             sprintf(fmt, "Members:",   ifelse(is.null(x$members),     "all available",
                                               paste(x$members, collapse = ", "))),
             sprintf(fmt, "Version:",   as.character(x$version)),
             sprintf(fmt, "Cache:",     ifelse(is.null(x$cache), "disabled", x$cache)),
             sprintf(fmt, "Area:",      ifelse(is.null(x$area), "not defined", "defined!")))
    cat(res, sep = "\n")
    invisible(x)
}
