#'Ehlers Instantaneous Trendline Indicator
#'@param HLC an xts object containing High, Low, and Close price data.
#'@param alpha a dampening factor -- default .07
#'@param priceMethod use "Close" for close prices or any other string for (H+L)/2
#'@return iTrend, the instantaneous trendline indicator
#'iTrigger -- the trigger line. Buy when this crosses above/sell when crossing under.
#'@references
#'\cr \url{http://www.mesasoftware.com/Seminars/AfTA\%20May\%2003.pdf}\cr
#'@export
"iTrend" <- function(HLC, alpha=.07, priceMethod="Close") {
  price <- ehlersPriceMethod(HLC, method=priceMethod)
  iTrend <- rep(0, length(price))
  tmp <- (price+2*lag(price)+lag(price,2))/4
  iTrend[1:6] <- tmp[1:6]
  iTrend[1:2] <- price[1:2]
  alphas <- rep(alpha, length(price))
  iTrend <- xts(computeItrend(price, iTrend, alphas), order.by=index(price))
  iTrigger <- 2*iTrend - lag(iTrend,2)
  out <- cbind(iTrend=iTrend, iTrigger=iTrigger)
  return(out)
}


