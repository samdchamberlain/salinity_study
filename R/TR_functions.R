#' Transfer entropy (Tr) functions (following Knuth et al. 2005)
#' Includes basic Tr calculations, lagged Tr, and Tr across time
#'
#' @import entropy
#' @importFrom dplyr lag

transfer_entropy <- function(x, y, xlag, ylag = 1, bins = 10, normalize = TRUE) {
  
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

#three-way joint distributions for transfer entropy calculation
discretize3d = function( x1, x2, x3, numBins, r1=range(x1), r2=range(x2), r3=range(x3))
{
  b1 = seq(from = r1[1], to = r1[2], length.out = numBins+1)
  b2 = seq(from = r2[1], to = r2[2], length.out = numBins+1)
  b3 = seq(from = r3[1], to = r3[2], length.out = numBins+1)
  
  y3d = table( cut(x1, breaks=b1, include.lowest=TRUE ), 
               cut(x2, breaks=b2, include.lowest=TRUE ),
               cut(x3, breaks=b3, include.lowest=TRUE ))
  
  return( y3d )
}

#calculate Tr at multiple lags of x variable
lag_tr <- function(x, y, lags, bins = 10, normalize = TRUE) {
  
  lag_tr <- vector("double", lags) #store lagged MI values
  
  for (i in 1:(lags)) {
    lag_tr[[i]] <- transfer_entropy(x, y, xlag = i, ylag = 1, bins, normalize = normalize) # using immediate history of y
  }
  
  lag_tr
}

#calculate Tr time series based on data in list form grouped by time index
tr_timeseries <- function(x, y, data_list, lags, bins = 10, normalize = TRUE, type = c("observed", "shuffled")) {
  
  output_list <- list()
  
  #iterate through indexed datasets and calculate Tr
  for (i in 1:nrow(data_list)) {
    
    name <- as.character(data_list[[1]][[i]]) #extract year
    output_list[[name]]
    
    current_df <- data_list$data[[i]] #extract current dataframe
    x_var <- eval(substitute(x), current_df)
    y_var <- eval(substitute(y), current_df)
    
    if (type == "observed") {
      output_list[[name]] <- lag_tr(x_var, y_var, lags, bins, normalize)
    } else if (type == "shuffled") {
      output_list[[name]] <- lag_confidence(x_var, y_var, alpha = 0.01, lags, type = "TR", runs = 10, bins, normalize)
    } else {
      print("Warning: type not assigned correctly")
    }
  }
  output_list
}