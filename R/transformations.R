#' Misc functions for data and list transformations:
#' anom_filter - removes periodic signal from data
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