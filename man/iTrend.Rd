\name{iTrend}
\alias{iTrend}
\title{Ehlers Instantaneous Trendline Indicator}
\usage{
  iTrend(HLC, alpha = 0.07, priceMethod = "Close")
}
\arguments{
  \item{HLC}{an xts object containing High, Low, and Close
  price data.}

  \item{alpha}{a dampening factor -- default .07}

  \item{priceMethod}{use "Close" for close prices or any
  other string for (H+L)/2}
}
\value{
  iTrend, the instantaneous trendline indicator iTrigger --
  the trigger line. Buy when this crosses above/sell when
  crossing under.
}
\description{
  Ehlers Instantaneous Trendline Indicator
}
\references{
  \cr
  \url{http://www.mesasoftware.com/Seminars/AfTA\%20May\%2003.pdf}\cr
}

