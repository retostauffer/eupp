

#' Get Configuration
#'
#' Provides a series of parameters. Internal use only.
#'
#' @details The variable \code{BASEURL} provides the ... base URL,
#' \code{PATTERN} is a character for the path/file pattern used to 
#' request data via HTTP. Not using a special format; {{variable}} will
#' be replaced using `gsub()` where needed. Will be extended with \code{.grb}
#' or \code{.grb.index} to either fetch the grib index (JSON format) or the
#' grib file itself.
#'
#' @author Reto Stauffer
get_config <- function() {
    list("BASEURL" = "https://storage.ecmwf.europeanweather.cloud/benchmark-dataset",
         "PATTERN" = "data/{{type}}/{{level}}/EU_analysis_{{level}}_params_{{isodate}}.grb")
}


#' Get Source URL
#'
#' For internal use only, returns the URL to the \code{.grb} file given
#' the input parameters. Called by the download functions.
#'
#' @param x character length \code{1}, type of forecast.
#' @param level character length \code{1}, type of level.
#' @param date object of class \code{POSIXt}, length 1. Initialization date of
#'        the data set.
#' @param parameter character.
#' @param fileext \code{NULL} or character, file extension added to the URL.
#'        used to generate the \code{.index} URL.
#'
#' @return Returns a character of length 1, URL to retrieve the data set.
#'
#' @author Reto Stauffer
get_source_url <- function(x     = c("reforecast", "forecast", "analysis"),
                           level = c("surface", "pressure", "efi"),
                           date, parameter, fileext = NULL) {
    # ---------------------
    # Quick sanity check
    # ---------------------
    x     <- match.arg(x)
    level <- match.arg(level)
    stopifnot(inherits(date, "POSIXt"), length(date) == 1L)

    if (length(fileext) == 0) fileext <- NULL
    stopifnot(inherits(fileext, c("NULL", "character")))
    stopifnot(grepl("^[0-9]{4}-[0-9]{2}(-[0-9]{2})?$", date))

    # Get date in the format required for the URL; must be
    # done before converting 'x' below.
    fmt <- c(reforecast = "%Y-%m-%d", forecast = "%Y-%m-%d", analysis = "%Y-%m")[x]
    date <- format(date, fmt)

    # Convert 'x' (type) and level
    x     <- c(reforecast = "rfcs", forecast = "fcs", analysis = "ana")[x]
    level <- c(surface = "surf", pressure = "pressure", efi = "efi")[level]

    # Getting basic config
    conf <- get_config()

    URL <- paste(conf$BASEURL, conf$PATTERN, sep = "/")
    URL <- gsub("\\{\\{type\\}\\}",    x,     URL)
    URL <- gsub("\\{\\{level\\}\\}",   level, URL)
    URL <- gsub("\\{\\{isodate\\}\\}", date,  URL)
    if (!is.null(fileext)) URL <- sprintf("%s.%s", URL, fileext)

    return(URL)
}
