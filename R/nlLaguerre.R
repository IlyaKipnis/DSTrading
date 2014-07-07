#'Nonlinear Laguerre Trend Indicator
#'@param HLC an xts object containing High, Low, and Close price data
#'@param n the period for computation
#'@param priceMethod use "Close" for the close, any other string will result in Ehlers's (H+L)/2 method.
#'@return an xts object
#'@references
#'\cr \url{http://www.mesasoftware.com/Seminars/TradeStation\%20World\%2005.pdf}\cr
#'@note TODO--implement loop in a faster language.
#'@export
"nlLaguerre" <- function(HLC, n=20, priceMethod="Close") {
  price <- as.numeric(ehlersPriceMethod(HLC, method=priceMethod))
  diff <- HH <- LL <- alpha <- L0 <- L1 <- L2 <- L3 <- filt <- rep(0, length(price))
  L0[1:n] <- L1[1:n] <- L2[1:n] <- L3[1:n] <- filt[1:n] <- price[1:n]
  for(i in (n+1):length(price)) {
    diff[i] <- abs(price[i]-filt[i-1])
    HH[i] <- max(diff[i:(i-n+1)])
    LL[i] <- min(diff[i:(i-n+1)])
    if((HH[i]-LL[i])!=0) {
      tmp <- diff[i:(i-4)]-LL[i:(i-4)]
      tmp2 <- HH[i:(i-4)]-LL[i:(i-4)]
      tmp3 <- tmp/tmp2
      tmp3[is.na(tmp3)] <- 0
      alpha[i] <- median(tmp3)
    }
    L0[i] <- alpha[i]*price[i]+(1-alpha[i])*L0[i-1]
    L1[i] <- -(1-alpha[i])*L0[i] + L0[i-1] + (1-alpha[i])*L1[i-1]
    L2[i] <- -(1-alpha[i])*L1[i] + L1[i-1] + (1-alpha[i])*L2[i-1]
    L3[i] <- -(1-alpha[i])*L2[i] + L2[i-1] + (1-alpha[i])*L3[i-1]
    filt[i] <- (L0[i]+2*L1[i]+2*L2[i]+L3[i])/6  
  }
  out <- xts(filt, order.by=index(HLC))
  return (out)
}
