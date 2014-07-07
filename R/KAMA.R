
#'Kaufman Adaptive Moving Average
#'@param HLC an xts object containing High, Low, and Close price data
#'@param nER a parameter for the Kaufman efficiency ratio lookback period
#'@param nFast the fast limit of the adaptive EMA
#'@param nSlow the slow limit of the adaptive EMA
#'@param priceMethod use "Close" for the close, any other string will result in Ehlers's (H+L)/2 method.
#'@return an xts object
#'@references
#'\cr \url{http://www.mesasoftware.com/Seminars/TradeStation\%20World\%2005.pdf}\cr
#'@export
"KAMA" <- function(HLC, nER=10, nFast=2, nSlow=30, priceMethod="Close") {
  price <- ehlersPriceMethod(HLC, method=priceMethod)
  nDayPriceMovement <- diff(price, nER)
  dailyDiffs <- abs(diff(price))
  nDayTotalMovement <- runSum(dailyDiffs, nER)
  ER <- nDayPriceMovement/nDayTotalMovement
  smoothConstant <- ER*(2/(1+nFast)-2/(1+nSlow)) + 2/(1+nSlow)
  alpha <- smoothConstant * smoothConstant
  alpha[is.na(alpha)] <- 1
  alphaComplement <- 1-alpha
  KAMA <- rep(0, length(price))
  KAMA[1:nER] <- price[1:nER]
  KAMA <- computeKAMA(alpha, alphaComplement, KAMA, price)
  KAMA <- xts(KAMA, order.by=index(price))
  return (KAMA)
}
