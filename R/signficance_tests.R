#' Functions for statistical signficance of mutual information and transfer entropy via
#' Monte Carlo reshuffling of datasets. Includes functions for single test and time series

#calculate confidence bound for Mutual Information statistical significance
mi_confidence <- function(x, y, runs = 1000) {
  
  mc_out <- rep(NA, runs) #vector for statistic output
  
  for (i in 1:runs) {
    
    rand_x <- sample(x, length(x), replace = F) #randomly shuffle x variable
    mc_out[i] <- mutual_info(rand_x, y, normalize = T) #calculate MI between shuffled x and y
  }
  mean(mc_out)
}

#calculate confidence bound for Transfer Entropy statistical significance
tr_confidence <- function(x, y, runs = 1000) {
  
  mc_out <- rep(NA, runs) #vector for statistic output
  
  for (i in 1:runs) {
    
    rand_x <- sample(x, length(x), replace = F) #randomly shuffle x variable
    mc_out[i] <- transfer_entropy(rand_x, y, normalize = T) #calculate MI between shuffled x and y
  }
  mean(mc_out)
}

#calculate times series of Monte Carlo limits
conf_series <- function(x, y, data_list, runs = 1000, type=c("I", "T")) {
  
  MC_series <- vector("double", nrow(data_list))
  
  for (i in 1:nrow(data_list)) {
    current_df <- data_list$data[[i]]
    x_var <- eval(substitute(x), current_df)
    y_var <- eval(substitute(y), current_df)
    
    if (type == "I") {
      MC_mean <- mi_confidence(x_var, y_var, runs)
    } else if (type == "T") {
      MC_mean <- tr_confidence(x_var, y_var, runs)
    } else {
      return("Warning: not a valid test")
    }
    
    MC_series[[i]] <- MC_mean
  }
  MC_series
}