rolling_mean <- function(x, window_width, ...){
  # -----Input Checks ----------------------------------------------------------
  # Check that x is a vector with numerical interpretation
  stopifnot(is.logical(x) | is.integer(x) | is.double(x) | is.complex(x))
  stopifnot(length(x) > 0)
  
  # Check window_width is an odd, positive integer
  stopifnot(length(window_width) == 1)
  stopifnot(window_width %% 1 == 0)
  stopifnot((window_width / 2) %% 1 != 0)
  stopifnot(window_width > 0)
  
  # ----- Function Body --------------------------------------------------------
  
  # number of values left and right to include in each mean
  half_width <- floor(window_width / 2)
  x_padded <- pad_with_NAs(x, n_left = half_width, n_right = half_width)
  evaluation_locations <- seq_along(x) + half_width
  
  output <- rep(NA, length(x))
  
  for (index in evaluation_locations) {
    # Extract relevant values from x_padded
    indices_in_window <- seq(index - half_width, index + half_width, by = 1)
    values_in_window <- x_padded[indices_in_window]
    
    # Calculate and store mean
    output[index - half_width] <- mean(values_in_window, ...)
  }
  
  return(output)
}