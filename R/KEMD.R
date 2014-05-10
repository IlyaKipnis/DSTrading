#Cycle Amplitude Calculation
#http://www.mesasoftware.com/Seminars/Trend%20Modes%20and%20Cycle%20Modes.pdf
#'Kipnis Empirical Mode Decomposition
#'@param x a time series
#'@param delta bandpass parameter (see reference paper), default .1
#'@param n a lookback period, default 20
#'@param bandFraction trending vs. mean-reverting band control. Raise to bias towards mean reversion.
#'Lower to bias towards trend-following. Defaults to .1
#'@param maType type of moving average to use. Defaults to SMA.
#'@return an xts consisting of a trend, a peak band, a valley band, a pctB, and momentum.
#'The market is classified as trending upwards when the trend is above the peak band,
#'trending downwards when below the valley, and rangebound in between.
#'The pctB is a different way of looking at the relationship between the trend and the peak and valleys.
#'The momentum column indicates trend direction.
#'@note -- I (Ilya Kipnis) made a special modification compared to Ehlers that divides the peak and valley bands
#'by an running n-point standard deviation of the bandpass. This helps make the bands more adaptive
#'and less likely to give whipsaw signals compared to Ehlers's Empirical Mode Decomposition. PctB and momentum are added features as well.
#'@references
#'\url{http://www.mesasoftware.com/Seminars/Trend\%20Modes\%20and\%20Cycle\%20Modes.pdf}
#'@export
"KEMD" <- function(x, delta=.1, n=20, bandFraction=.1, maType="SMA", ...) {
  maType <- match.fun(maType)
  beta <- cos(2*pi/n)
  gamma <- 1/cos(4*pi*delta/n)
  alpha <- gamma-sqrt(gamma*gamma-1)
  lag2x <- lag(x,2)
  lag2x[is.na(lag2x)] <- x[1]
  bandpass <- xts(filter(.5*(1-alpha)*(x-lag2x), c(beta*(1+alpha), -1*alpha), method="recursive"), order.by=index(x))
  bandpassSD <- runSD(bandpass, n) #this is Ilya's own modification. Bandpass SD is usually < 1 except in cases of trends.
  trend <- xts(maType(bandpass, n=2*n), order.by=index(x))
  peak <- valley <- rep(NA, length(bandpass))
  peakIndex <- which(lag(bandpass) > bandpass & lag(bandpass) > lag(bandpass,2))-1
  valleyIndex <- which(lag(bandpass) < bandpass & lag(bandpass) < lag(bandpass, 2))-1
  peak[peakIndex] <- bandpass[peakIndex]; valley[valleyIndex] <- bandpass[valleyIndex]
  peak <- xts(peak, order.by=index(x)); valley <- xts(valley, order.by=index(x))
  peak <- na.locf(peak); valley <- na.locf(valley)
  period <- min(n+30, 2.5*n)
  meanPeak <- bandFraction*maType(peak, n=floor(period))/bandpassSD
  meanValley <- bandFraction*maType(valley, n=floor(period))/bandpassSD
  pctB <- (trend-meanValley)/(meanPeak-meanValley)
  momentum <- trend-lag(trend, n)
  out <- cbind(trend, meanPeak, meanValley, pctB, momentum)
  colnames(out) <- c("trend", "peak", "valley", "pctB", "momentum")
  return (out)
}
