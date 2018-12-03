#'Ehlers Filters -- moving average type smoothers
#'@param HLC an HLC type xts object
#'@param n number of periods to look back
#'@param nCoefLookback number of periods over which to compute one coefficient
#'@param priceMethod "Close" for closing price, otherwise uses Ehlers (H+L)/2
#'@param coefMethod one of three possible methods to compute a coefficient.
#'Distance computes the sum of squares of differences of the last nCoefLookback prices and the current price.
#'AbsVal computes the absolute difference between the current price and the price nCoefLookback periods ago.
#'Ichimoku computes the average of the highest high of the last nCoefLookback periods and the lowest low.
#'@param sumType either of "arithmetic" or "wilder" type arguments for the type of sum.
#'@return the quotient of the sum of the last n coefficients multiplied by price divided by the sum of the coefficients.
#'@note TODO: implement more possible coefficient methods.
#'@references
#'\cr \url{http://www.mesasoftware.com/Papers/Ehlers\%20Filters.pdf}\cr
#'@export

"EhlersFilter" <- function(HLC, n=15, nCoefLookback=10, priceMethod="Close", 
                         coefMethod=c("Distance","AbsVal","Ichimoku"), 
                         sumType=c("arithmetic","wilder")) {
  price <- ehlersPriceMethod(HLC, method=priceMethod)
  if(missing(coefMethod)){
    coefMethod <- "Distance"
  }
  else if(coefMethod=="Distance") {
    prices <- lag(price, rep(0,nCoefLookback))
    lags <- lag(price, c(1:nCoefLookback))
    diffs <- prices - lags
    diffSq <- diffs*diffs
    sumDiffSq <- rowSums(diffSq)
    coefs <- xts(sumDiffSq, order.by=index(prices))
  }
  else if(coefMethod=="AbsVal") {
    coefs <- abs(price-lag(price,nCoefLookback))
  }
  else if(coefMethod=="Ichimoku") {
    coefs <- (runMax(Hi(HLC), nCoefLookback)+runMin(Lo(HLC), nCoefLookback))/2
  } else {
    stop("Invalid coefficient method for Ehlers Filter")
  }
  if(missing(sumType)) {
    sumType <- "arithmetic"
  }
  if(sumType=="wilder") {
    coefPriceSum <- wilderSum(coefs*price, n)
    coefSum <- wilderSum(coefs, n)
  }
  else if (sumType=="arithmetic") { 
    coefPriceSum <- runSum(coefs*price, n)
    coefSum <- runSum(coefs, n)
  } else {
    stop("invalid sum mechanism")
  }
  out <- coefPriceSum/coefSum
  return(out) 
}