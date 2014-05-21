#'Ehlers Sinewave Indicator (v. 2013)
#'@param x a price series
#'@param duration a lookback period
#'@return The Sinewave Indicator (Oscillator type)
#'@references Cycle Analytics For Traders, Chapter 12
#'@export
"SWI" <- function(x, duration=40, ...) {
  alpha1 <- 1-(sin(2*pi/duration))/cos(2*pi/duration)
  HP <- (1-alpha1/2)*(1-alpha1/2)*(x-2*lag(x)+lag(x, 2))
  HP[1] <- HP[2] <- 0
  HP <- filter(HP, c(2*(1-alpha1), -1*(1-alpha1)*(1-alpha1)), method="recursive")
  a1 <- exp(-sqrt(2)*pi/10)
  b1 <- 2*a1*cos(sqrt(2)*pi/10)
  c2 <- b1
  c3 <- -a1*a1
  c1 <- 1-c2-c3
  HP <- xts(HP, order.by=index(x))
  LHP <- lag(HP)
  LHP[1] <- 0
  filt <- filter(c1*(HP+LHP)/2, c(c2, c3), method="recursive")
  filt <- xts(filt, order.by=index(x))
  wave <- (filt+lag(filt)+lag(filt, 2))/3
  power <- (filt*filt+lag(filt)*lag(filt)+lag(filt, 2)*lag(filt, 2))/3
  out <- wave/sqrt(power)
  colnames(out) <- "wave"
  return(out)
}