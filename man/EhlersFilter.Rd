\name{EhlersFilter}
\alias{EhlersFilter}
\title{Ehlers Filters -- moving average type smoothers}
\usage{
  EhlersFilter(HLC, n = 15, nCoefLookback = 10,
    priceMethod = "Close",
    coefMethod = c("Distance", "AbsVal", "Ichimoku"),
    sumType = c("arithmetic", "wilder"))
}
\arguments{
  \item{HLC}{an HLC type xts object}

  \item{n}{number of periods to look back}

  \item{nCoefLookback}{number of periods over which to
  compute one coefficient}

  \item{priceMethod}{"Close" for closing price, otherwise
  uses Ehlers (H+L)/2}

  \item{coefMethod}{one of three possible methods to
  compute a coefficient. Distance computes the sum of
  squares of differences of the last nCoefLookback prices
  and the current price. AbsVal computes the absolute
  difference between the current price and the price
  nCoefLookback periods ago. Ichimoku computes the average
  of the highest high of the last nCoefLookback periods and
  the lowest low.}

  \item{sumType}{either of "arithmetic" or "wilder" type
  arguments for the type of sum.}
}
\value{
  the quotient of the sum of the last n coefficients
  multiplied by price divided by the sum of the
  coefficients.
}
\description{
  Ehlers Filters -- moving average type smoothers
}
\note{
  TODO: implement more possible coefficient methods.
}
\references{
  \cr
  \url{http://www.mesasoftware.com/Papers/Ehlers\%20Filters.pdf}\cr
}

