


#' onAttach function
#'
#' @author Reto Stauffer
.onAttach <- function(...) {

    # Terms of use
    tos <- "
    By downloading data from this dataset, you agree to the terms and conditions defined at
    https://github.com/Climdyn/climetlab-eumetnet-postprocessing-benchmark/blob/main/LICENSE and
    https://github.com/Climdyn/climetlab_eumetnet_postprocessing_benchmark/blob/main/DATA_LICENSE
    If you do not agree with such terms, do not download the data.
    \n"
    cat(tos)
}
