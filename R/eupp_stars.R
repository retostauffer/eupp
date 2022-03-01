


#' EUPP Stars Objects
#'
#' Custom class based on \code{stars} objects. The additional
#' class \code{eupp_stars} is used to provide some additional
#' methods to conveniently process the data.
#'
#' Internally, an \code{eupp_stars} object is only a \code{stars}
#' object with one additional attribute (the extra class).
#'
#' @param x stars object
#' @param ... unused
#'
#' @author Reto Stauffer
#' @name eupp_stars
#' @method [ eupp_stars
#' @export
"[.eupp_stars" <- function(x, ...) {
    x <- NextMethod(x, ...)
    class(x) <- c("eupp_stars", class(x))
    invisible(x)
}

#' @author Reto Stauffer
#' @rdname eupp_stars
#' @method [[ eupp_stars
#' @export
"[[.eupp_stars" <- function(x, ...) {
    x <- NextMethod(x, ...)
    class(x) <- c("eupp_stars", class(x))
    return(x)
}


#' @method print eupp_stars
#' @export
print.eupp_stars <- function(x, ...) {
    class(x) <- class(x)[-1]
    return(print(x))
}

#'
#' @param at object of class \code{sf} or \code{sfc} forwarded to
#'        \code{\link[stars]{st_extract}}
#' @param bilinear logical, forwarded to \code{\link[stars]{st_extract}}.
#'        Defaults to \code{TRUE} (bilinear interpolation); \code{FALSE}
#'        results in the use of nearest-neighbour interpolation.
#' @param atname character length \code{1} or \code{NULL} (default).
#'        Can be used to specify a variable in the object \code{at} which should
#'        be appended to the final object.
#'
#' @return Returns an object of class \code{c("sf", "data.frame")}.
#'
#' @author Reto Stauffer
#' @importFrom stars st_extract st_get_dimension_values
#' @rdname eupp_stars
#' @method st_extract eupp_stars
#' @export
st_extract.eupp_stars <- function(x, at, bilinear = FALSE, atname = NULL, ...) {
    stopifnot(isTRUE(bilinear) | isFALSE(bilinear))
    stopifnot(is.character(atname) | is.null(atname), length(atname) <= 1L)

    #x <- NextMethod(x, bilinear = bilinear, ...)
    if (!bilinear) {
        res <- NextMethod(x, at = at, bilinear = bilinear, ...)
    } else {
        # Check if we have a 'number' (perturbation number/ensemble member) dimension.
        # If so, we have to split the data set into member-by-member stars objects
        # to be able to perform bilinar interpolation (mainly for bilinear).
        idx_number <- which(attr(attr(x, "dimensions"), "names") == "number")

        # If we don't have a member dimension: simply call NextMethod (st_extract.stars)
        if (length(idx_number) == 0) {
            res <- st_as_sf(NextMethod(x, at = at, bilinear = bilinear, ...), long = TRUE)
        # Else (number dimension existing): loop over each number (member),
        # Interpolate member-by-member and combine the resulting stars object.
        # Variables (attributes) are renamed; adding '_<number>' to distinguish
        # the different members.
        } else {
            res <- NULL
            val_number <- st_get_dimension_values(x, "number")
            ndim <- length(dim(x))
            pat  <- paste(ifelse(seq(0, ndim) == idx_number, "%d", ""), collapse = ",")
            for (i in seq_along(val_number)) {
                tmp <- sprintf("x[%s, drop = TRUE]", sprintf(pat, i))
                tmp <- eval(parse(text = tmp))
                k <- st_extract(tmp, at = at, bilinear = bilinear, ...)
                # Renaming non-geometry columns
                names(k) <- ifelse(names(k) == attr(k, "sf_column"),
                                   names(k), paste(names(k), val_number[i], sep = "_"))
                res <- if (is.null(res)) k else cbind(res, st_drop_geometry(k))
            }
        }
    }
    if (!is.null(atname) && atname %in% names(at)) {
        res <- sf_append_atname_if_possible(res, at, atname)
        res <- res[,c(attr(res, "sf_column"), atname,
                   sort(names(res)[!names(res) %in% c(atname, attr(res, "sf_column"))]))]
    } else {
        res <- res[,c(attr(res, "sf_column"),
                   sort(names(res)[!names(res) == attr(res, "sf_column")]))]
    }
    return(res)
}


# Append Additional Variable to object x if possible
#
# Small hidden helper function used in \code{\link{st_extract.eupp_stars}}.
# If the locations \code{at} contain a dedicated column called \code{name},
# these names will be added to the stars object.
#
# @author Reto Stauffer
sf_append_atname_if_possible <- function(x, at, atname) {
    if (atname %in% names(at) && !atname %in% names(x)) {
        idx    <- match(st_geometry(x), st_geometry(at))
        x$name <- at[[atname]][idx]
    }
    return(x)
}
