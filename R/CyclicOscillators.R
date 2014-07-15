#'Cycle Analytics For Traders Stochastic Oscillator
#'@description Computes the modified stochastic oscillator from Chapter 7-3 of Cycle Analytics For Traders by Dr. John Ehlers
#'@param x a time series
#'@param n a lookback period
#'@return a stochastic oscillator between 0 and 1
#'@export
"CycleStoch" <- function(x, n=20) {
  filt <- superSmoother(highPassFilter(x))
  filtMax <- runMax(filt, n)
  filtMin <- runMin(filt, n)
  stoc <- (filt-filtMin)/(filtMax-filtMin)
  stoc <- superSmoother(stoc)
  colnames(stoc) <- "CycleStoch"
  return(stoc)
}

#'Cycle Analytics For Traders RSI Oscillator
#'@description  Computes the modified RSI from Chapter 7-4 of Cycle Analytics for Traders by Dr. John Ehlers
#'@param x a time series
#'@param n a lookback period
#'@return an RSI between 0 and 100
#'@export
"CycleRSI" <- function(x, n=20) {
  filt <- superSmoother(highPassFilter(x))
  diffFilt <- diff(filt)
  posDiff <- negDiff <- diffFilt
  posDiff[posDiff < 0] <- 0
  negDiff[negDiff > 0] <- 0
  negDiff <- negDiff*-1
  posSum <- runSum(posDiff, n)
  negSum <- runSum(negDiff, n)
  denom <- posSum+negSum
  rsi <- posSum/denom
  rsi <- superSmoother(rsi)*100
  colnames(rsi) <- "CycleRSI"
  return(rsi)
}