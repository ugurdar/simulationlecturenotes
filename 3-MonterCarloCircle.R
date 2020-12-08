
# ggplot2 package for plotting
library(ggplot2)
# Monte-Carlo hit and miss method
hitmiss <- function(N,r=1,f = function(x,y){x^2 + y^2},plot=FALSE){
  # To approximate rho generating random numbers, UNIF(-r,r)  distribution
  x <- runif(N, -r, r); y <- runif(N, -r, r)
  #  calculate the ratio of points that fall into the ,
  #  circumference to the points that fall off the circumference.
  dist <- f(x,y)
  # If distance_from_center < r^2 it gives TRUE
  # inbounds variable is logical. 
  inbounds <- (dist < r^2)
  # In R, TRUE = 1 and FALSE = 0, so if we sum inbounds's elements 
  # it gives number of points which in the circle.
  # According to equation 2 , rho = sum(inbounds)/N
  piestimated <- 4 * sum(inbounds)/N
  piestimated
  # Plotting 
  plt <- ggplot(data.frame(x, y, inbounds), aes(x, y, color=inbounds)) +
    theme_bw() + guides(color=FALSE) +
    geom_point(size=0.002) +
    ggtitle(paste(format(N, scientific = FALSE), 'N','Estimated Pi:',piestimated))
  # Drawing plot the square, circle and dots.Result shown on plot title.
  if(plot == TRUE){
    return(plt)
  }else{
    return(piestimated)
  }
}

# # for reproducibility
# set.seed(3)
# # Call hitmiss function for N = 10000 default r=1.
# hitmiss(100000,plot=TRUE)
# 
# 
# N <- t(seq(100,10000,by=200))
# results <- data.frame(N=t(N),Ihat = apply(N,2,hitmiss))
# results
# 
# 
# 
# pi <- 3.141593 # Actual pi value
# 
# ggplot(results,aes(x=N,y=Ihat))+
#   geom_line() +
#   theme_bw() +
#   geom_hline(yintercept=pi, linetype="dashed", 
#                 color = "red", size=1)
