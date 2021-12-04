

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
         "PATTERN" = "data/{{type_abbr}}/{{level}}/EU_{{type}}_ctr_{{level}}_params_{{isodate}}_{{version}}.grb")
}


#' Get Source URL
#'
#' For internal use only, returns the URL to the \code{.grb} file given
#' the input parameters. Called by the download functions.
#'
#' @param ... unused.
#'
#' @return Returns a character of length 1, URL to retrieve the data set.
#'
#' @author Reto Stauffer
get_source_url <- function(x, fileext = NULL, ...) {
    stopifnot(inherits(x, "eupp_config"))

    if (length(fileext) == 0) fileext <- NULL
    stopifnot(inherits(fileext, c("NULL", "character")))

    # Get date in the format required for the URL; must be
    # done before converting 'x' below.
    fmt <- c(reforecast = "%Y-%m-%d", forecast = "%Y-%m", analysis = "%Y-%m")[x$type]
    x$date <- format(x$date, fmt)

    # Convert 'x' (type) and level
    type_abbr <- c(reforecast = "rfcs", forecast = "fcs", analysis = "ana")[x$type]
    level     <- c(surface = "surf", pressure = "pressure", efi = "efi")[x$level]

    # Getting basic config
    conf <- get_config()

    URL <- paste(conf$BASEURL, conf$PATTERN, sep = "/")
    URL <- gsub("\\{\\{type_abbr\\}\\}",   x$type_abbr,  URL)
    URL <- gsub("\\{\\{type\\}\\}",        x$type,       URL)
    URL <- gsub("\\{\\{level\\}\\}",       x$level,      URL)
    URL <- gsub("\\{\\{isodate\\}\\}",     x$date,       URL)
    URL <- gsub("\\{\\{version\\}\\}",     x$version,    URL)
    if (!is.null(fileext)) URL <- paste(URL, fileext, sep = ".")

    return(URL)
}
