\name{LRSI}
\alias{LRSI}
\title{Laguerre RSI}
\usage{
  LRSI(HLC, gamma = 0.5, priceMethod = "Close")
}
\arguments{
  \item{HLC}{-- an xts object containing High, Low, and
  Close price data}

  \item{gamma}{-- a dampening factor}

  \item{priceMethod}{-- use "Close" for the close, any
  other string will result in Ehlers's (H+L)/2 method.}
}
\value{
  an xts object
}
\description{
  Laguerre RSI
}
\references{
  \cr
  \url{http://www.mesasoftware.com/Papers/TIME\%20WARP.pdf}\cr
}

