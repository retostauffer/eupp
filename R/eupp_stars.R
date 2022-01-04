


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


#' @author Reto Stauffer
#' @importFrom stars st_extract
#' @rdname eupp_stars
#' @export
st_extract.eupp_stars <- function(x, at, bilinear = FALSE, ...) {
    stopifnot(isTRUE(bilinear) | isFALSE(bilinear))

    #x <- NextMethod(x, bilinear = bilinear, ...)
    if (!bilinear) {
        print('xxxxxxxxxx')
        res <- NextMethod(x, at = at, bilinear = bilinear, ...)
    } else {
        res <- list()
        (idx_number <- which(attr(attr(x, "dimensions"), "names") == "number"))
        (idx_time   <- which(attr(attr(x, "dimensions"), "names") == "time"))
        if (!length(idx_number) == 0 && !length(idx_time) == 0) {
            ndim <- length(dim(x))
            pat  <- paste(ifelse(seq(0, ndim) == idx_time, "%d", ""), collapse = ",")
            for (i in seq_len(dim(x)[idx_time])) {
                tmp <- sprintf("x[%s, drop = TRUE]", sprintf(pat, i))
                print(tmp)
                tmp <- eval(parse(text = tmp))
                res <- c(res, stars::st_extract(tmp, at = at, bilinear = bilinear, ...))
            }
        }
    }
    return(res)

}
