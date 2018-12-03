#'Ehlers Relative Vigor Index
#'@param OHLC an OHLC xts object
#'@param n number of periods to use
#'@return an XTS object
#'@references the following site was used to code this indicator:
#'\cr \url{http://www.davenewberg.com/Trading/TS_Code/Ehlers_Indicators/Relative_Vigor_Index.html}\cr
#'@export
"RVI" <- function(OHLC, n=10) {
  if(!is.OHLC(OHLC)) {stop("Object passed to RVI not an OHLC object. Stopping.")}
  ClOp <- Cl(OHLC)-Op(OHLC)
  HiLo <- Hi(OHLC)-Lo(OHLC)
  val1 <- (ClOp+2*lag(ClOp)+2*lag(ClOp,2)+lag(ClOp,3))/6
  val2 <- (HiLo+2*lag(HiLo)+2*lag(HiLo,2)+lag(HiLo,3))/6
  num <- runSum(val1, n=n)
  den <- runSum(val2, n=n)
  RVI <- num/den
  trigger <- lag(RVI)
  out <- xts(cbind(RVIsig=RVI, RVItrigger=trigger), order.by=index(OHLC))
  return(out)
}