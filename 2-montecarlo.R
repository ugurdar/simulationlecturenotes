# 
f <- function(x) {
  exp(x)
}

hitmiss <- function (N, f, a, b) {
  fmax = f(b)
  fmin = f(a)
  
  curve(f, 0, 1, ylim = c(0, 3), col = "red")
  rpts <- data.frame(runif(N, a, b), runif(N, fmin, fmax))
  
  
  ptunder <- rpts[which(f(rpts[1]) > rpts[2]), ]
  ptabove <- rpts[which(!f(rpts[1]) > rpts[2]), ]
  points(ptunder, col = "red")
  points(ptabove, col = "blue")
  
  int <- (b - a) * fmin + (dim(ptunder)[1] / N) * (fmax - fmin)
  return(int)
  
}


f <- function(x) {
  exp(x)
}
mc.integral <- function(f, n) {
  # Monte-Carlo integral of ftn over [0, 1] using a sample of size n
  u <- runif(n, 0, 1)
  x <- f(u)
  Ihat <- mean(x)
  return(Ihat)
}
