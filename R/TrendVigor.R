#'Trend Vigor Indicator
#'@description John Ehlers's Trend Vigor Indicator, along with a signal and lead combination to form an oscillator indicator.
#'@param x a univariate series
#'@param period an integer number of days to look back. Not implemented yet: enter 0 for a dynamic period computation.
#'@param delta a trigonometric parameter for computing bandpass filter.
#'@param triggerLag -- a delay by which to lag the vigor computation.
#'@return vigor -- the trend vigor indicator. An absolute value higher than 1 indicates a trend. \cr \cr
#'Signal and lead--a combination of indicators to form an oscillator. Buy when the signal crosses under the lead and vice versa.
#'Signal and lead are centered at zero. Trigger is the vigor computation lagged by the triggerLag indicator.
#'@note TODO: implement method for using a dynamic/adaptive period computation for greater accuracy.
#'@references \url{http://www.mesasoftware.com/Seminars/Trend\%20Modes\%20and\%20Cycle\%20Modes.pdf}
#'@export
"TVI" <- function(x, period=20, delta=.2, triggerLag=1) {
  if(period!=0) { #static, length-1 period
    beta <- cos(2*pi/period)
    gamma <- 1/cos(4*pi*delta/period)
    alpha <- gamma - sqrt(gamma*gamma-1)
    BP <- .5*(1-alpha)*(x-lag(x,2))
    BP[1] <- BP[2] <- 0
    BP <- filter(BP, c(beta*(1+alpha),-1*alpha),method="recursive")
    BP <- xts(BP, order.by=index(x))
    signal <- BP - lag(BP, round(period/2))
    lead <- 1.4*(BP-lag(BP, round(period/4)))
    
    BP2 <- BP*BP
    LBP2 <- lag(BP2, round(period/4))
    power <- runSum(BP2, period)+runSum(LBP2, period)
    RMS <- sqrt(power/period)
    PtoP <- 2*sqrt(2)*RMS
    
    a1 <- exp(-sqrt(2)*pi/period)
    b1 <- 2*a1*cos(sqrt(2)*pi/period)
    coef2 <- b1
    coef3 <- -a1*a1
    coef1 <- 1-coef2-coef3
    trend <- coef1*(x-lag(x, period))
    trend[is.na(trend)] <- 0
    trend <- filter(trend, c(coef2, coef3), method="recursive")
    trend <- xts(trend, order.by=index(x))
    vigor <- trend/PtoP
    vigor[vigor > 2] <- 2
    vigor[vigor < -2] <- -2
    vigor[is.na(vigor)] <- 0
    trigger <- lag(vigor, triggerLag)
    out <- cbind(vigor, signal, lead, trigger)
    colnames(out) <- c("vigor", "signal", "lead", "trigger")
    return(out)
  } else {
    stop("Dynamic period computation not implemented yet.")
    #TODO -- DYNAMIC PERIOD TREND VIGOR
  }
}