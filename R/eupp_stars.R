


#' EUPP Stars Objects
#'
#' Custom class based on \code{stars} objects. The additional
#' class \code{eupp_stars} is used to provide some additional
#' methods to conveniently process the data.
#'
#' Internally, an \code{eupp_stars} object is only a \code{stars}
#' object with one additional attribute (the extra class).
#'
#' @author Reto Stauffer
#' @rdname eupp_stars
#' @export
"[.eupp_stars" <- function(x, ...) {
    x <- NextMethod(x)
    class(x) <- c("eupp_stars", class(x))
    return(x)
}

#' @author Reto Stauffer
#' @rdname eupp_stars
#' @export
"[[.eupp_stars" <- function(x, ...) {
    x <- NextMethod(x)
    class(x) <- c("eupp_stars", class(x))
    return(x)
}
