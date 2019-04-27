#' Script for mutual information calculations
#' Including functions for basic MI calculations, lagged MI, and MI across time
#'
#' @import entropy
#' @importFrom dplyr lag

#calculate relative mutual information between two variables and designate number of bins to use
mutual_info <- function(x, y, bins = 10, normalize = TRUE) {

  clip_set <- na.omit(data.frame(x, y)) #clear NA values from x,y variables

  y2d <- discretize2d(clip_set$x, clip_set$y, numBins1 = bins, numBins2 = bins) #joint distribution

  Hx <- entropy.empirical(rowSums(y2d)) #entropy x variable
  Hy <- entropy.empirical(colSums(y2d)) #entropy y variable
  Hxy <- entropy.empirical(y2d) #joint entropy

  if (normalize == T) {
    I <- (Hx + Hy - Hxy)/Hy #relative mutual information
  } else {
    I <- (Hx + Hy - Hxy) #absolute mutual information
  }

  return(I)
}


#calculate MI at multiple lags of x variable
lag_mi <- function(x, y, lags, bins = 10, normalize = TRUE) {

  lag_mi <- vector("double", lags) #store lagged MI values

  for (i in 1:(lags+1)) {
    x_lag <- dplyr::lag(x, n = i-1) # include syncronhous interactions
    lag_mi[[i]] <- mutual_info(x_lag, y, bins, normalize = normalize)
  }

  lag_mi
}

#calculate MI time series based on data in list form grouped by time index
mi_timeseries <- function(x, y, data_list, bins = 10, normalize = TRUE) {

  mi_series <- vector("double", nrow(data_list)) #vector to store mutual information

  #iterate through indexed datasets and calculate relative MI
  for (i in 1:nrow(data_list)) {
    current_df <- data_list$data[[i]] #extract current dataframe
    x_var <- eval(substitute(x), current_df)
    y_var <- eval(substitute(y), current_df)

    mi_series[[i]] <- mutual_info(x_var, y_var, bins, normalize = normalize)
  }
  mi_series
}

#calculate correlation coefficients for comparison
corr_timeseries <- function(x, y, data_list, method = 'kendall') {

  corr_series <- vector("double", nrow(data_list)) #vector to store correlation coefficients

  #iterate through indexed datasets and calculate relative MI
  for (i in 1:nrow(data_list)) {
    current_df <- data_list$data[[i]] #extract current dataframe
    x_var <- eval(substitute(x), current_df)
    y_var <- eval(substitute(y), current_df)

    out <- cor.test(x_var, y_var, method = method)
    corr_series[[i]] <- out$estimate[[1]]
  }
  corr_series
}


