

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
         "PATTERN" = "data/ana/{{level}}/EU_analysis_{{level}}_params_{{isodate}}")
}

# @TODO
get_data_url <- function(x, level, date, parameter, fileext = NULL) {
    # Getting SANE
    stopifnot(is.character(date), length(date) == 1L)
    if (length(fileext) == 0) fileext <- NULL
    stopifnot(inherits(fileext, c("NULL", "character")))
    stopifnot(grepl("^[0-9]{4}-[0-9]{2}(-[0-9]{2})?$", date))

    # Getting basic config
    conf <- get_config()

    URL <- paste(conf$BASEURL, conf$PATTERN, sep = "/")
    URL <- gsub("\\{\\{level\\}\\}", level, URL)
    URL <- gsub("\\{\\{isodate\\}\\}", date, URL)
    if (!is.null(fileext)) URL <- sprintf("%s.%s", URL, fileext)

    URL
}
