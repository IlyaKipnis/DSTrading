#'Center of Gravity Oscillator
#'@param HLC an xts object containing High, Low, and Close price data
#'@param n the period for computation
#'@param priceMethod use "Close" for the close, any other string will result in Ehlers's (H+L)/2 method.
#'@param scale an option whether or not to scale the oscillator. Defaults to TRUE.
#'@param nSD an argument for scaling. Sets the number of days over which to compute a running volatility.
#'In effect, a larger nSD value will assign more extreme values to the scaled oscillator.
#'@return an xts object
#'@references
#'\cr \url{http://www.mesasoftware.com/Papers/The\%20CG\%20Oscillator.pdf}\cr
#'@export
"CGO" <- function(HLC, n=10, priceMethod="Close", scale=TRUE, nSD=100) {
  price <- ehlersPriceMethod(HLC, method=priceMethod)
  lags <- lag(price, k=c(0:(n-1)))
  denominator <- runSum(price, n=n)
  numerator <- lags%*%c(1:n)
  CGO <- numerator/denominator*-1
  
  if(scale) {
    #Ilya's modification to normalize scale
    CGO <- CGO+mean(c(1,n))
    CGO <- CGO/runSD(CGO, n=nSD)
  }
  
  return(CGO)  
}