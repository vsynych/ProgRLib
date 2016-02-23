
# ------------------------------------------------------------
# Decompose a vector into a matrix of "fourier" components
#
# Author:  Aidan McDermott (AMcD)
# Date  :  Dec 8, 2000
# ------------------------------------------------------------
decompose <- function(x,breaks) {
  
  # need to be careful if length(x) is even or odd
  is.even <- !length(x)%%2
  
  xf  <- fft(x)/length(x)
  xf1 <- xf[1]   # first bit is the sum of x
  
  xf.first <- xf[2:(1+floor(length(xf)/2))]
  
  # break xf.first into various components
  cuts  <- cut(1:length(xf.first),breaks,include.lowest=T)
  lcuts <- attr(cuts,"levels")
  ncuts <- length(attr(cuts,"levels"))
  
  mat <- matrix(0,nrow=length(x),ncol=ncuts)
  
  for ( i in 1:ncuts ) {
    xf.temp <- rep(0,length(xf.first))
    xf.temp[cuts==lcuts[i]] <- xf.first[cuts==lcuts[i]]
    
    if (is.even==T) {
      mat[,i] <- Re(fft(c(xf1/ncuts,xf.temp,
                          rev(Conj(xf.temp[1:(length(xf.temp)-1)]))),inverse=T))
    }
    else {
      mat[,i] <- Re(fft(c(xf1/ncuts,xf.temp,rev(Conj(xf.temp))),inverse=T))
    }
  }
  return(mat)
}


