#'Variable Index Dynamic Average
#'@param x a price series
#'@param n a short period number over which to take a running SD
#'@param m a long period number over which to take a running SD
#'@param smoothConstant a parameter that affects smoothing. k*p(t) + (1-k)*p(t-1)
#'@return a stream of Variable Index Dynamic Average computations.
#'@references 
#'\cr \url{"http://www.mesasoftware.com/Seminars/TradeStation\%20World\%2005.pdf"} \cr
#'@export
"VIDA" <- function(x, n=9, m=30, smoothConstant=.2, ...) {
  k <- runSD(x,n=n)/runSD(x,n=m)
  k[is.na(k)] <- 0
  filt <- rep(0, length(x))
  filt[1] <- x[1]
  consts <- rep(smoothConstant, length(x))
  out <- computeVIDA(k, x, filt, consts)
  out <- xts(out, order.by=index(x))
}