#'Ehlers Cyber Cycle
#'@param HLC an xts object containing High, Low, and Close price data
#'@param alpha a tuning parameter
#'@param priceMethod use "Close" for the close, any other string will result in Ehlers's (H+L)/2 method.
#'@param iFish defaults to TRUE. Whether or not to apply the inverse fisher transform. See ?iFish
#'@return an xts object
#'@references
#'\cr \url{http://www.mesasoftware.com/Papers/THE\%20INVERSE\%20FISHER\%20TRANSFORM.pdf}\cr
#'@export
"cyberCycle" <- function(HLC, alpha=.07, priceMethod="Close", iFish=TRUE) {
  price <- ehlersPriceMethod(HLC, method=priceMethod)
  smooth <- (price+2*lag(price)+2*lag(price,2)+lag(price,3))/6
  
  initialCycle <- (price-2*lag(price)+lag(price,2))/4
  initialCycle <- initialCycle[1:6]
  cycle <- (1-.5*alpha)*(1-.5*alpha)*(smooth-2*lag(smooth)+lag(smooth,2))
  cycle[1:6] <- initialCycle
  cycle[1] <- cycle[2] <- 0
  cycle <- stats::filter(cycle, c(2*(1-alpha), -1*(1-alpha)*(1-alpha)), method="recursive", init=c(0,0))
  if(iFish) {
    cycle <- iFish(cycle)
  }
  cycle <- xts(cycle, order.by=index(HLC))
  return(cycle)
}
