


#' onAttach function
#'
#' @author Reto Stauffer
.onAttach <- function(...) {

    # Terms of use
    tos <- "
    By downloading data from this dataset, you agree to the terms and conditions defined in
    https://github.com/retostauffer/eupp/blob/main/README.md
    If you do not agree with such terms, do not download the data.
    \n"
    cat(tos)
}
