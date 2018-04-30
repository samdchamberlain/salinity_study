#' Functions for statistical signficance of mutual information and transfer entropy via
#' Monte Carlo reshuffling of datasets. Includes functions for single test and time series
#'
#' @importFrom dplyr lag

#calculate confidence bound for Mutual Information statistical significance
mi_confidence <- function(x, y, alpha = 0.05, runs = 100, bins = 10, normalize = T) {

  mc_out <- rep(NA, runs) #vector for statistic output

  for (i in 1:runs) {
    rand_x <- sample(x, length(x), replace = F) #randomly shuffle x variable
    mc_out[i] <- mutual_info(rand_x, y, bins, normalize) #calculate MI between shuffled x and y
  }

  if (alpha > 0 && alpha < 1) {
    limit <- quantile(mc_out, probs = (1 - alpha))[[1]]
  } else {
    return("Error: sig. limit out of range (0 - 1).")
  }

  # if (alpha == 0.01) {
  #   limit <- mean(mc_out) + 2.36*sd(mc_out)
  # } else if (alpha == 0.05) {
  #   limit <- mean(mc_out) + 1.66*sd(mc_out)
  # } else {
  #   return("This threshold is not supported")
  # }
  limit
}

#calculate confidence bound for Transfer Entropy statistical significance
tr_confidence <- function(x, y, xlag, ylag = 1, alpha = 0.05, runs = 25, bins = 10, normalize = T) {

  mc_out <- rep(NA, runs) #vector for statistic output

  for (i in 1:runs) {
    rand_x <- sample(x, length(x), replace = F) #randomly shuffle x variable
    mc_out[i] <- transfer_entropy(rand_x, y, xlag, ylag, bins = bins, normalize = normalize)
  }

  if (alpha > 0 && alpha < 1) {
    limit <- quantile(mc_out, probs = (1 - alpha))[[1]]
  } else {
    return("Error: sig. limit out of range (0 - 1).")
  }
  # if (alpha == 0.01) {
  #   limit <- mean(mc_out) + 2.49*sd(mc_out)
  # } else if (alpha == 0.05) {
  #   limit <- mean(mc_out) + 1.71*sd(mc_out)
  #   limit2 <- quantile(mc_out, probs = 0.95)[[1]]
  # } else {
  #   return("This threshold is not supported")
  # }
  limit
}

#calculate times series of Monte Carlo limits (does not include time lags)
conf_series <- function(x, y, data_list, alpha = 0.05, runs = 1000, bins = 10, type=c("MI", "TR")) {

  MC_series <- vector("double", nrow(data_list))

  for (i in 1:nrow(data_list)) {
    current_df <- data_list$data[[i]]
    x_var <- eval(substitute(x), current_df)
    y_var <- eval(substitute(y), current_df)

    if (type == "MI") {
      MC_mean <- mi_confidence(x_var, y_var, alpha, runs, bins)
    } else if (type == "TR") {
      MC_mean <- tr_confidence(x_var, y_var, alpha, runs, bins)
    } else {
      return("Warning: not a valid test")
    }

    MC_series[[i]] <- MC_mean
  }
  MC_series
}

lag_confidence <- function(x, y, lags, type = c("MI", "TR"), alpha,
                           runs = 1000, bins = 10, normalize = TRUE) {

  MC_lagseries <- vector("double", lags) #vector for shuffled metrics

  if (type == "MI") {
    for (i in 1:(lags + 1)) {
      x_lag <- dplyr::lag(x, n = i-1) # include syncronhous interactions
      MC_lagseries[[i]] <- mi_confidence(xlag, y, alpha, runs, bins, normalize)
    }
  } else if (type == "TR") {
    for (i in 1:(lags)) {
      MC_lagseries[[i]] <- tr_confidence(x, y, xlag = i, ylag = 1, alpha, runs, bins, normalize)
    }
  } else {
    return("Warning: not a valid test")
  }
  MC_lagseries
}
