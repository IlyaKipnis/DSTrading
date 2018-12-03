#'Ehlers Predictive Moving Averages
#'@param HLC an xts object containing High, Low, and Close price data
#'@param n the period for computation
#'@param priceMethod use "Close" for the close, any other string will result in Ehlers's (H+L)/2 method.
#'@return an xts object
#'@references Rocket Science For Traders, Chapter 20
#'@export
"PMA" <- function(HLC, n, priceMethod="Close") {
  price <- ehlersPriceMethod(HLC, method=priceMethod)
  normalizingSum <- n*(n+1)/2
  
  lags <- lag(price, k=c(0:(n-1)))
  WMA1 <- xts(lags%*%c(n:1)/normalizingSum, order.by=index(price))
  
  maLags <- lag(WMA1, k=c(0:(n-1)))
  WMA2 <- xts(maLags%*%c(n:1)/normalizingSum, order.by=index(price))
  
  predict <- 2*WMA1-WMA2
  triggerN <- ceiling(n/2)
  triggerNormalizingSum <- triggerN*(triggerN+1)/2
  triggerLags <- lag(predict, k=c(0:(triggerN-1)))
  trigger <- xts(triggerLags%*%c(triggerN:1)/triggerNormalizingSum, order.by=index(price))
  out <- cbind(predict, trigger)
  colnames(out) <- c("predict", "trigger")
  return(out)
}

