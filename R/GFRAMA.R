#'Generalized FRAMA
#'@description the generalized FRAMA (see: FRAMA)
#'@param x a time series
#'@param n a lookback period
#'@param FC fast constant for an EMA
#'@param SC slow constant for an EMA
#'@return the FRAMA for the time series
#'@export 
"GFRAMA" <- function(x, n=20, FC=1, SC=200, ...) {
  runSum(x) #test for non-leading NAs
  index <- index(x)
  if (n%%2==1) n=n-1 #n must be even
  N3 <- (runMax(x, n)-runMin(x, n))/n
  N1 <- (runMax(x, n/2)-runMin(x, n/2))/(n/2)
  lagSeries <- lag(x, n/2)
  N2 <- (runMax(lagSeries, n/2)-runMin(lagSeries, n/2))/(n/2)
  dimen <- (log(N1+N2)-log(N3))/log(2)
  w <- log(2/(SC+1))
  oldAlpha <- exp(w*(dimen-1))
  oldN <- (2-oldAlpha)/oldAlpha
  newN <- ((SC-FC)*(oldN-1)/(SC-1))+FC
  alpha <- 2/(newN+1)
  alpha[which(alpha > 1)] <- 1
  alpha[which(alpha < w)] <- w
  alphaComplement <- 1-alpha
  initializationIndex <- index(alpha[is.na(alpha)])
  alpha[is.na(alpha)] <- 1; alphaComplement[is.na(alphaComplement)] <- 0
  initialNAs <- rep(NA, sum(is.na(x)))
  x <- x[!is.na(x)]
  FRAMA <- rep(0, length(x))
  FRAMA[1] <- x[1]
  FRAMA <- computeFRAMA(alpha, alphaComplement, FRAMA, x)
  FRAMA <- c(initialNAs, FRAMA)
  FRAMA <- xts(FRAMA, order.by=index)
  FRAMA[initializationIndex] <- alpha[initializationIndex] <- NA
  out <- FRAMA
  colnames(out) <- "GFRAMA"
  return(out)
}