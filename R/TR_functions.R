#' Transfer entropy (Tr) functions (following Knuth et al. 2005)
#' Includes basic Tr calculations, lagged Tr, and Tr across time
#'
#' @import entropy
#' @importFrom dplyr lag

transfer_entropy <- function(x, y, xlag, ylag, bins = 10, normalize = TRUE) {
  
  x_lag <- dplyr::lag(x, n = xlag)
  y_lag <- dplyr::lag(y, n = ylag)
  
  #clip_set <- data.frame(x_lag, y_lag, y)
  #clip_set[!complete.cases(clip_set), ] <- NA #this follows Laurel and Ben's code
  clip_set <- na.omit(data.frame(x_lag, y_lag, y)) #clip rows were any value is missing
  
  Hxy <- entropy.empirical(discretize2d(clip_set$x_lag, clip_set$y_lag, numBins1 = bins, numBins2 = bins), unit = "log2")
  Hyy <- entropy.empirical(discretize2d(clip_set$y, clip_set$y_lag,  numBins1 = bins, numBins2 = bins), unit = "log2")
  Hyl <- entropy.empirical(discretize(na.omit(clip_set$y_lag), numBins = bins), unit = "log2")
  Hxyz <- entropy.empirical(discretize3d(clip_set$x_lag, clip_set$y_lag, clip_set$y, numBins = bins), unit = "log2")
  Hy <- entropy.empirical(discretize(na.omit(clip_set$y), numBins = bins), unit = "log2") # entropy at current state
  
  if (normalize == T) {
    Tr = (Hxy + Hyy - Hyl - Hxyz)/Hy
  } else {
    Tr = Hxy + Hyy - Hyl - Hxyz
  }
  return(Tr)
}

#calculate Tr at multiple lags of x variable
lag_tr <- function(x, y, lags, bins = 10, normalize = TRUE) {
  
  lag_tr <- vector("double", lags) #store lagged MI values
  
  for (i in 1:(lags)) {
    lag_tr[[i]] <- transfer_entropy(x, y, x_lag = i, ylag = 1, bins, normalize = normalize) # using immediate history of y
  }
  
  lag_tr
}

#calculate Tr time series based on data in list form grouped by time index
tr_timeseries <- function(x, y, data_list, bins = 10, normalize = TRUE) {
  
  tr_series <- vector("double", nrow(data_list)) #vector to store transfer entropy
  
  #iterate through indexed datasets and calculate Tr
  for (i in 1:nrow(data_list)) {
    current_df <- data_list$data[[i]] #extract current dataframe
    x_var <- eval(substitute(x), current_df)
    y_var <- eval(substitute(y), current_df)
    
    #NOTE this needs to be expanded to include lags!!!!!!!!!!!!!!!!!!
    tr_series[[i]] <- transfer_entropy(x_var, y_var, bins, normalize = normalize)
  }
  tr_series
}