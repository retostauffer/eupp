


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

#' @param at object of class \code{sf} or \code{sfc} forwarded to
#'        \code{\link[stars]{st_extract}}
#' @param bilinear logical, forwarded to \code{\link[stars]{st_extract}}.
#'        Defaults to \code{TRUE} (bilinear interpolation); \code{FALSE}
#'        results in the use of nearest-neighbour interpolation.
#' @param atname character length \code{1} or \code{NULL} (default).
#'        Can be used to specify a variable in the object \code{at} which should
#'        be appended to the final object.
#' @param ... currently unused.
#' @param subsequent Defaults to \code{NULL}, please leave as it is.
#'
#' @details
#' Depending on the stars object this function is calling itself multiple times.
#' This is controlled using the 'subsequent' argument which becomes a named
#' vector (internally). As an end-user, please leave this argument \code{NULL}.
#' Have not found a better solution yet.
#'
#' @return Returns an object of class \code{c("sf", "data.frame")}.
#'
#' @author Reto Stauffer
#' @importFrom sf st_sf
#' @importFrom stars st_extract st_get_dimension_values
#' @importFrom units drop_units
#' @importFrom stats setNames
#' @rdname eupp_stars
#' @method st_extract eupp_stars
#' @export
st_extract.eupp_stars <- function(x, at, bilinear, atname = NULL, ..., subsequent = NULL) {


    # The interpolation of stars objects with levels or numbers (non-common
    # dimensions) requires a special approach. We first check if we have 
    # a dimension 'level' or 'number', or both.
    idx <- which(dimnames(x) %in% c("level", "number"))

    # No level or number dimension? In this case we can interpolate
    # the stars object using st_extract.
    if (length(idx) == 0) {
        class(x) <- class(x)[-1] # Remving 'eupp_stars' class (-> 'stars')
        res <- st_extract(x, at = at, bilinear = bilinear, long = TRUE)
        res <- as.data.frame(res)
        if (!is.null(subsequent)) {
            for (i in seq_along(subsequent)) res[[names(subsequent)[i]]] <- subsequent[i]
        }

    # Else we have a level or number dimension (or both)
    } else {
        # In this iteration we take the first index (either level or number)
        idx    <- idx[1]
        # Checking values of the dimension
        dimval <- as.vector(st_get_dimension_values(x, dimnames(x)[idx]))
        # .. and create the command used for subsetting the data set.
        cmds   <- sprintf("x[%s, drop = TRUE]",
                          sprintf(paste(ifelse(0:length(dim(x)) == idx, "%d", ""), collapse = ","), seq_along(dimval)))

        # Loop over the dimension of the current dimension (e.g., if we have 3 levels
        # c(500, 700, 850) we will loop i = 1:3.
        res <- list()
        for (i in seq_along(dimval)) {
            # Subset the stars object, pick the i'th entry of the dimension we are looping
            # over at the moment. The function we are in calls itself recursively as long as needed
            # (typically only once if we have both, a 'level' and a 'number' dimension).
            tmp_sub <- if (is.null(subsequent)) setNames(dimval[i], dimnames(x)[idx]) else c(subsequent, setNames(dimval[i], dimnames(x)[idx]))
            tmp <- st_extract(eval(parse(text = cmds[i])), at = at, bilinear = bilinear, subsequent = tmp_sub)
            sep <- if (dimnames(x)[idx] == "level") "" else "_"
            # Append current data.frame (interpolated values)
            res[[paste(sep, as.character(dimval[i]))]] <- tmp
        }
    }

    # Thanks to https://stackoverflow.com/users/1174421/michael
    # https://stackoverflow.com/questions/16300344/how-to-flatten-a-list-of-lists
    flatten_list <- function(x){
        morelists <- sapply(x, function(xprime) class(xprime)[1] == "list")
        out <- c(x[!morelists], unlist(x[morelists], recursive = FALSE))
        if (sum(morelists)) Recall(out) else return(out)
    }

    # Flatten list and row-bind the results and prepare final result
    if (is.null(subsequent)) {
        if (inherits(res, "list")) res <- do.call(rbind, flatten_list(res)) # Combine list -> data.frame if needed
        res <- st_sf(res, sf_column_name  = "geometry")
        rownames(res) <- NULL

        # Create a vector with the leading colums (those whill end up most left in the data.frame)
        leading_cols <- c(attr(res, "sf_column"),
                          if (!is.null(atname) && atname %in% names(at)) atname else NULL,
                          if ("time" %in% names(res)) "time" else NULL)
        print(leading_cols)

        # Append naming column if needed ...
        if (!is.null(atname) && atname %in% leading_cols) res <- sf_append_atname_if_possible(res, at, atname)

        # Need to convert the date?
        if (is.numeric(res$date)) res$date <- as.POSIXct(as.Date(res$date, "1970-01-01"))
        # Sort the object
        res <- drop_units(res[, c(leading_cols, sort(names(res)[!names(res) %in% leading_cols]))])
    }

    # And there we go ...
    return(res)
}



#' Append Additional Variable to object x if possible
#'
#' Small hidden helper function used in \code{\link{st_extract.eupp_stars}}.
#' If the locations \code{at} contain a dedicated column called \code{name},
#' these names will be added to the stars object.
#'
#' @param x sf object (interpolated data).
#' @param at sf object with \code{POINTS} and, possibly, a variable named \code{atname}.
#' @param atname character length \code{1} or \code{NULL}.
#'
#' @importFrom sf st_geometry
#' @author Reto Stauffer
sf_append_atname_if_possible <- function(x, at, atname) {
    if (atname %in% names(at) && !atname %in% names(x)) {
        idx    <- match(st_geometry(x), st_geometry(at))
        x$name <- at[[atname]][idx]
    }
    return(x)
}
