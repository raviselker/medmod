#' Center a vector
#'
#' \code{center} returns a numeric vector with centered values.
#'
#' @param x Numeric vector.
center <- function(x) {
    
    centered <- as.vector(scale(x = x, center = TRUE, scale = FALSE))
    
    return(centered)
}