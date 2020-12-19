library(dplyr)
lcg <- function(a,c,m,len,z0) {
  z <- rep(0,len);z[1] <- z0 # seed
  for (i in 1:(len-1)) {
    z[i+1] <- (a * z[i] + c) %% m}
  U <- z/m # scaling (0,1)
  return(list(Z=z,U=U))}
rnd <- lcg(a=3,c=0,z0=2,m=77,len=60)
df <- data.frame(i=1:60,Z_i=rnd$Z,U_i = rnd$U)

#hist(df$U_i,xlab="U_i",main="Histogram of U_i")

#par(mfrow = c(2,2))
#plot(df$U_i,xlab="i",ylab="U_i",main="Z_i vs i")
#plot(df$Z_i[1:49],df$Z_i[2:50],ylab="Z_(i+1)",xlab="Z_i",main="Z_i vs Z_(i+1)")
#plot(df$i,df$U_i,xlab="i",ylab="U_i",main="U_i vs i")
#plot(df$U_i,lag(df$U_i),xlab="U_i",ylab="U_(i-1)",main="U_i vs U_(i-1)")

rnd <- lcg(a=2^16+3,c=0,z0=2,m=2^31,len=100000) #IBM's RANDU - RANDOM NUMBER GENARATOR PARAMETERS
df <- data.frame(i=1:100000,Z_i=rnd$Z,U_i = rnd$U)
#hist(df$U_i)
