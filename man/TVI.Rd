\name{TVI}
\alias{TVI}
\title{Trend Vigor Indicator}
\usage{
  TVI(x, period = 20, delta = 0.2, triggerLag = 1)
}
\arguments{
  \item{x}{a univariate series}

  \item{period}{an integer number of days to look back. Not
  implemented yet: enter 0 for a dynamic period
  computation.}

  \item{delta}{a trigonometric parameter for computing
  bandpass filter.}

  \item{triggerLag}{-- a delay by which to lag the vigor
  computation.}
}
\value{
  vigor -- the trend vigor indicator. An absolute value
  higher than 1 indicates a trend. \cr \cr Signal and
  lead--a combination of indicators to form an oscillator.
  Buy when the signal crosses under the lead and vice
  versa. Signal and lead are centered at zero. Trigger is
  the vigor computation lagged by the triggerLag indicator.
}
\description{
  John Ehlers's Trend Vigor Indicator, along with a signal
  and lead combination to form an oscillator indicator.
}
\note{
  TODO: implement method for using a dynamic/adaptive
  period computation for greater accuracy.
}
\references{
  \url{http://www.mesasoftware.com/Seminars/Trend\%20Modes\%20and\%20Cycle\%20Modes.pdf}
}

