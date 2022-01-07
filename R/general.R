
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
#' @keywords internal
eupp_get_url_config <- function() {
    list("BASEURL"  = "https://storage.ecmwf.europeanweather.cloud/benchmark-dataset",
         "analysis" = "data/{{product_abbr}}/{{level}}/EU_{{product}}_{{level}}_params_{{yyyy-mm}}.grb",
         "hr"       = "data/{{product_abbr}}/{{level}}/EU_{{product}}_{{type}}_{{level}}_params_{{yyyy-mm}}_{{version}}.grb",
         "efi"      = "data/{{product_abbr}}/{{level}}/EU_{{product}}_{{level}}_params_{{yyyy-mm}}_{{version}}.grb",
         "ens"      = c("data/{{product_abbr}}/{{level}}/EU_{{product}}_ctr_{{level}}_params_{{yyyy-mm}}_{{version}}.grb",
                        "data/{{product_abbr}}/{{level}}/EU_{{product}}_ens_{{level}}_params_{{yyyy-mm-dd}}_{{version}}.grb"))
}


#' Get Source URL
#'
#' For internal use only, returns the URL to the \code{.grb} file given
#' the input parameters. Called by the download functions.
#'
#' @param x object of class \code{eupp_config} returned by \code{\link{eupp_config}}.
#' @param fileext \code{NULL} or character length \code{1}. File extension (or
#'        URL extension) added to the URL. Used to generate URLs to GRIB index files.
#'        Defaults to \code{NULL}.
#' @param ... unused.
#'
#' @return Returns a character of length 1, URL to retrieve the data set.
#'
#' @author Reto Stauffer
#' @export
eupp_get_source_urls <- function(x, fileext = NULL, ...) {
    stopifnot(inherits(x, "eupp_config"))

    if (length(fileext) == 0) fileext <- NULL
    stopifnot(inherits(fileext, c("NULL", "character")))

    # Getting basic config
    conf <- eupp_get_url_config()
    if (!is.null(x$type) && x$type %in% names(conf)) {
        template <- paste(conf$BASEURL, conf[[x$type]], sep = "/")
    } else if (x$level %in% names(conf)) {
        template <- paste(conf$BASEURL, conf[[x$level]], sep = "/")
    } else if (x$product %in% names(conf)) {
        template <- paste(conf$BASEURL, conf[[x$product]], sep = "/")
    } else {
        stop("Whoops! Unexpected case identifying the URL pattern ... (yes, it's a bug).")
    }

    # Convert 'x' (type) and level
    x$level     <- c(surface = "surf", pressure = "pressure", efi = "efi")[x$level]

    URLS <- c()
    for (i in seq_along(x$date)) {

        # Make a copy of the URL template first
        tmp <- template

        # Appending required date/dates.
        x$`yyyy-mm`    <- format(x$date[i], "%Y-%m")
        x$`yyyy-mm-dd` <- format(x$date[i], "%Y-%m-%d")

        # In case 'type == "ens"' we are getting two URL's, one for the control run
        # and one for the actual ensemble members. If 'members' is given check if we
        # really need both or only one.
        if (!is.null(x$type) && !is.null(x$members) && x$type == "ens") {
            tmp <- tmp[c(any(x$members == 0), any(x$members > 0))]
        }

        # Replacing variables in the basic URL pattern
        for (n in names(x)) {
            ##cat("------\n", n, "\n", paste(x[[n]], collapse = ", "), "\n")
            if (!is.null(x[[n]]) && length(x[[n]]) == 1L) tmp <- gsub(sprintf("\\{\\{%s\\}\\}", n), x[[n]],  tmp)
        }

        # Appending file extension if required (for .index files)
        if (!is.null(fileext)) tmp <- paste(tmp, fileext, sep = ".")
        URLS <- c(URLS, tmp)
    }

    return(unique(URLS))
}
