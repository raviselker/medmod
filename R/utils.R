
center <- function(x) {
    
    centered <- as.vector(scale(x = x, center = TRUE, scale = FALSE))
    
    return(centered)
}