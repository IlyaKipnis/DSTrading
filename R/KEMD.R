
cppFunction('
  NumericVector computePeak(NumericVector BP, NumericVector peak)  {
            int n = BP.size();
            for(int i=1; i<n; i++) {
            peak[i] = peak[i-1];
            if (BP[i-1] > BP[i] && BP[i-1] > BP[i-2]){
            peak[i] = BP[i-1];
            }
            }
            return peak;
            }        
            ')


cppFunction('
            NumericVector computeValley(NumericVector BP, NumericVector valley)  {
            int n = BP.size();
            for(int i=1; i<n; i++) {
            valley[i] = valley[i-1];
            if (BP[i-1] < BP[i] && BP[i-1] < BP[i-2]){
            valley[i] = BP[i-1];
            }
            }
            return valley;
            }        
            ')

#Cycle Amplitude Calculation
#http://www.mesasoftware.com/Seminars/Trend%20Modes%20and%20Cycle%20Modes.pdf
#'Kipnis Empirical Mode Decomposition
#'@param x -- a time series
#'@param delta -- bandpass parameter (see reference paper), default .1
#'@param n -- a lookback period, default 20
#'@param bandFraction -- trending vs. mean-reverting band control. Raise to bias towards mean reversion.
#'Lower to bias towards trend-following. Defaults to .1
#'@param maType -- type of moving average to use. Defaults to SMA.
#'@return an xts consisting of a trend, a peak band and a valley band.
#'The market is classified as trending upwards when the trend is above the peak band,
#'trending downwards when below the valley, and rangebound in between.
#'@note -- I (Ilya Kipnis) made a special modification compared to Ehlers that divides the peak and valley bands
#'by an running n-point standard deviation of the bandpass. This helps make the bands more adaptive
#'and less likely to give whipsaw signals compared to Ehlers's Empirical Mode Decomposition.
#'@references
#'\cr \url{http://www.mesasoftware.com/Seminars/Trend\%20Modes\%20and\%20Cycle\%20Modes.pdf}\cr
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
  peak <- valley <- rep(0, length(bandpass))
  peak <- xts(computePeak(bandpass, peak), order.by=index(x))
  valley <- xts(computeValley(bandpass, valley), order.by=index(x))
  meanPeak <- bandFraction*maType(peak, n=floor(2.5*n))/bandpassSD
  meanValley <- bandFraction*maType(valley, n=floor(2.5*n))/bandpassSD
  out <- cbind(trend, meanPeak, meanValley)
  colnames(out) <- c("trend", "peak", "valley")
  return (out)
}
