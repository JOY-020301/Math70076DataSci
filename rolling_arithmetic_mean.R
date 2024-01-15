roll_ari_mean <- function(data, windowSize) {
  # Check if the data is a numeric vector
  if (!is.numeric(data)) {
    stop("Data must be a numeric vector.")
  }
  
  # Check if window size is valid
  if (!is.numeric(windowSize) || windowSize <= 0 || windowSize > length(data)) {
    stop("Invalid window size.")
  }
  
  # Calculate the rolling mean
  rollingMean <- filter(data, rep(1/windowSize, windowSize), sides=1)
  
  return(rollingMean)
}
