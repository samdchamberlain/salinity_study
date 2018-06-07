#' Misc functions for data and list transformations:
#' anom_filter - removes periodic signal from data
#' tidy_list - convert list format back into dataframe time series
#' ec2pss (from wq package) - build issues with wq not on CRAN so moved directly to coded functions
#'
#'
#' @importFrom dplyr lag
#' @importFrom tidyr gather
#' @importFrom stringr str_replace
#' @importFrom purrr map_df

anom_filter <- function(var, window = 5) {
  as.numeric(var - ma(var, order = window))
}

tidy_list <- function(list) {

  df <- as.data.frame(list)
  df$lag <- rownames(df) #rownames correspond to lag
  df <- gather(df, year, tr, -lag) #tidy with year by lag
  df$year <- str_replace(df$year, 'X', '') #clean up year column
  df <- map_df(df, as.numeric) #make all columns numeric
  df$hour <- df$lag/2

  #create year index that also displays mean annual conductivity

  return(df)
}

ec2pss <- function(ec, t, p = 0) {
  R <- ec/42.914
  c <- c(0.6766097, 0.0200564, 0.0001104259, -6.9698e-07, 1.0031e-09)
  rt <- c[1] + c[2] * t + c[3] * t^2 + c[4] * t^3 + c[5] *
    t^4
  d <- c(0.03426, 0.0004464, 0.4215, -0.003107)
  e <- c(2.07e-05, -6.37e-10, 3.989e-15)
  Rp <- 1 + p * (e[1] + e[2] * p + e[3] * p^2)/(1 + d[1] *
                                                  t + d[2] * t^2 + (d[3] + d[4] * t) * R)
  Rt <- R/(Rp * rt)
  a <- c(0.008, -0.1692, 25.3851, 14.0941, -7.0261, 2.7081)
  b <- c(5e-04, -0.0056, -0.0066, -0.0375, 0.0636, -0.0144)
  ft <- (t - 15)/(1 + 0.0162 * (t - 15))
  S <- a[1] + a[2] * Rt^0.5 + a[3] * Rt + a[4] * Rt^1.5 + a[5] *
    Rt^2 + a[6] * Rt^2.5 + ft * (b[1] + b[2] * Rt^0.5 + b[3] *
                                   Rt + b[4] * Rt^1.5 + b[5] * Rt^2 + b[6] * Rt^2.5)
  x <- 400 * Rt
  y <- 100 * Rt
  ifelse(S >= 2, S, S - a[1]/(1 + 1.5 * x + x^2) - b[1] * ft/(1 +
                                                                y^0.5 + y + y^1.5))
}

# joint_entropy <- function(x, y, bins = 11) {
#
#   clip_set <- na.omit(data.frame(x, y)) #clear NA values from x,y variables
#   r1 = range(clip_set$x); r2 = range(clip_set$y)
#
#   # bin label for every observation of x and y
#   x_breaks <- seq(from=r1[1], to=r1[2], length.out=bins+1)
#   y_breaks <- seq(from=r2[1], to=r2[2], length.out=bins+1)
#
#   #create vector of bin assignments
#   x_bin <- table(cut(clip_set$x, x_breaks, include.lowest = T, labels = 1:bins))
#   y_bin <- table(cut(clip_set$y, y_breaks, include.lowest = T, labels = 1:bins))
#
#   #calculate joint and marginal entropies
#   p_x <- x_bin/sum(x_bin)
#   p_y <- y_bin/sum(y_bin)
#
#   -sum(p_y[p_y > 0]*log(p_y[p_y > 0]))
#   double <- table(cut(clip_set$x, x_breaks, include.lowest = T, labels = 1:bins),
#                   cut(clip_set$y, y_breaks, include.lowest = T, labels = 1:bins),
#                   cut(clip_set$y, y_breaks, include.lowest = T, labels = 1:bins))
#
#   double
#   #p_double <- double/sum(double)
#   #-sum(p_double[p_double > 0]*log(p_double[p_double > 0]))
#
# }
#
# discretize3d = function( x1, x2, x3, numBins, r1=range(x1), r2=range(x2), r3=range(x3))
#   {
#   b1 = seq(from = r1[1], to = r1[2], length.out = numBins+1)
#   b2 = seq(from = r2[1], to = r2[2], length.out = numBins+1)
#   b3 = seq(from = r3[1], to = r3[2], length.out = numBins+1)
#
#   y3d = table( cut(x1, breaks=b1, include.lowest=TRUE ),
#                cut(x2, breaks=b2, include.lowest=TRUE ),
#                cut(x3, breaks=b3, include.lowest=TRUE ))
#
#   return( y3d )
# }
