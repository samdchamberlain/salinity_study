#' Script for entropy and mutual information calculations
#' Including functions for basic MI calculations, lagged MI, and MI across time
#'
#' @import entropy
#' @importFrom dplyr lag

#calculate relative mutual information between two variables and designate number of bins to use
mutual_info <- function(x, y, bins = 10) {

  clip_set <- na.omit(data.frame(x, y)) #clear NA values from x,y variables

  y2d <- discretize2d(clip_set$x, clip_set$y, numBins1 = bins, numBins2 = bins) #joint probability distribution with num of bins

  y_entropy <- entropy(discretize(na.omit(y), numBins = bins)) #entropy of dependent variable
  mi.empirical(y2d)/y_entropy #relative mutual information (standardized by entropy of y)
}

#calculate MI at multiple lags of x variable
lag_mi <- function(x, y, lags, bins = 10, plotting = F) {

  lag_mi <- vector("double", lags) #store lagged MI values

  for (i in 1:lags) {
    x_lag <- dplyr::lag(x, n = i)
    lag_mi[[i]] <- mutual_info(x_lag, y, bins)
  }

  lag_mi
}

#calculate MI time series based on data in list form grouped by time index
mi_timeseries <- function(x, y, data_list, bins = 10) {

  mi_series <- vector("double", nrow(data_list)) #vector to store mutual information

  #iterate through indexed datasets and calculate relative MI
  for (i in 1:nrow(data_list)) {
    current_df <- data_list$data[[i]] #extract current dataframe
    x_var <- eval(substitute(x), current_df)
    y_var <- eval(substitute(y), current_df)

    mi_series[[i]] <- mutual_info(x_var, y_var, bins)
  }
  mi_series
}

#calculate correlation time series based on data in list form grouped by time index
corr_timeseries <- function(x, y, data_list, method = "pearson") {

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

#calculate upper confidence bound for statistical significance
mi_confidence <- function(x, y, runs = 1000, cutoff = 0.975) {

  mc_out <- rep(NA, runs) #vector for statistic output

  for (i in 1:runs) {

    rand_x <- sample(x, length(x), replace = F) #randomly shuffle x variable
    mc_out[i] <- mutual_info(rand_x, y) #calculate MI between shuffled x and y
  }
  quantile(mc_out, c(cutoff))[[1]]
}

conf_series <- function(x, y, data_list, runs = 1000) {

  limit_series <- vector("double", nrow(data_list))

  for (i in 1:nrow(data_list)) {
    current_df <- data_list$data[[i]]
    x_var <- eval(substitute(x), current_df)
    y_var <- eval(substitute(y), current_df)

    upper_limit <- mi_confidence(x_var, y_var, runs)
    limit_series[[i]] <- upper_limit
  }
  limit_series
}


