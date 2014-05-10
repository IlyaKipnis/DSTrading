\name{KEMD}
\alias{KEMD}
\title{Kipnis Empirical Mode Decomposition}
\usage{
  KEMD(x, delta = 0.1, n = 20, bandFraction = 0.1,
    maType = "SMA", ...)
}
\arguments{
  \item{x}{a time series}

  \item{delta}{bandpass parameter (see reference paper),
  default .1}

  \item{n}{a lookback period, default 20}

  \item{bandFraction}{trending vs. mean-reverting band
  control. Raise to bias towards mean reversion. Lower to
  bias towards trend-following. Defaults to .1}

  \item{maType}{type of moving average to use. Defaults to
  SMA.}
}
\value{
  an xts consisting of a trend, a peak band, a valley band,
  a pctB, and momentum. The market is classified as
  trending upwards when the trend is above the peak band,
  trending downwards when below the valley, and rangebound
  in between. The pctB is a different way of looking at the
  relationship between the trend and the peak and valleys.
  The momentum column indicates trend direction.
}
\description{
  Kipnis Empirical Mode Decomposition
}
\note{
  -- I (Ilya Kipnis) made a special modification compared
  to Ehlers that divides the peak and valley bands by an
  running n-point standard deviation of the bandpass. This
  helps make the bands more adaptive and less likely to
  give whipsaw signals compared to Ehlers's Empirical Mode
  Decomposition. PctB and momentum are added features as
  well.
}
\references{
  \url{http://www.mesasoftware.com/Seminars/Trend\%20Modes\%20and\%20Cycle\%20Modes.pdf}
}

