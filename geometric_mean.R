geo_mean <- function(x) {
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector.")
  }
  
  if (any(x <= 0)) {
    stop("All elements of the vector must be positive.")
  }
  
  n <- length(x)
  product <- prod(x)
  gm <- product^(1/n)
  
  return(gm)
}