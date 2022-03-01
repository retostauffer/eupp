


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
"[.eupp_stars" <- function(x, ...) {
    x <- NextMethod(x, ...)
    class(x) <- c("eupp_stars", class(x))
    return(x)
}

#' @author Reto Stauffer
#' @rdname eupp_stars
"[[.eupp_stars" <- function(x, ...) {
    x <- NextMethod(x, ...)
    class(x) <- c("eupp_stars", class(x))
    return(x)
}


#'
#' @param at object of class \code{sf} or \code{sfc} forwarded to
#'        \code{\link[stars]{st_extract}}
#' @param bilinear logical, forwarded to \code{\link[stars]{st_extract}}.
#'        Defaults to \code{TRUE} (bilinear interpolation); \code{FALSE}
#'        results in the use of nearest-neighbour interpolation.
#'
#' @author Reto Stauffer
#' @importFrom stars st_extract st_get_dimension_values
#' @rdname eupp_stars
#' @export
st_extract.eupp_stars <- function(x, at, bilinear = FALSE, ...) {
    stopifnot(isTRUE(bilinear) | isFALSE(bilinear))

    #x <- NextMethod(x, bilinear = bilinear, ...)
    if (!bilinear) {
        res <- NextMethod(x, at = at, bilinear = bilinear, ...)
    } else {
        res <- list()
        # Check if we have a 'number' (perturbation number/ensemble member) dimension.
        # If so, we have to split the data set into member-by-member stars objects
        # to be able to perform bilinar interpolation (mainly for bilinear).
        idx_number <- which(attr(attr(x, "dimensions"), "names") == "number")

        # If we don't have a member dimension: simply call NextMethod (st_extract.stars)
        if (length(idx_number) == 0) {
            res <- NextMethod(x, at = at, bilinear = bilinear, ...)
        # Else (number dimension existing): loop over each number (member),
        # Interpolate member-by-member and combine the resulting stars object.
        # Variables (attributes) are renamed; adding '_<number>' to distinguish
        # the different members.
        } else {
            val_number <- st_get_dimension_values(x, "number")
            ndim <- length(dim(x))
            pat  <- paste(ifelse(seq(0, ndim) == idx_number, "%d", ""), collapse = ",")
            for (i in seq_len(dim(x)[idx_number])) {
                tmp <- sprintf("x[%s, drop = TRUE]", sprintf(pat, i))
                tmp <- eval(parse(text = tmp))
                class(tmp) <- "stars"
                k <- stars::st_extract(tmp, at = at, bilinear = bilinear, ...)
                names(k) <- paste(names(k), val_number[i], sep = "_")
                res[[i]] <- k
            }
            res <- do.call(c, res)
        }
    }
    return(res)
}
