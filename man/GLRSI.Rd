\name{GLRSI}
\alias{GLRSI}
\title{Generalized Laguerre RSI}
\usage{
  GLRSI(HLC, n = 4, gamma = 0.5, priceMethod = "Close")
}
\arguments{
  \item{HLC}{-- an xts object containing High, Low, and
  Close price data}

  \item{n}{-- the period for computation}

  \item{gamma}{-- a dampening factor}

  \item{priceMethod}{-- use "Close" for the close, any
  other string will result in Ehlers's (H+L)/2 method.}
}
\value{
  an xts object
}
\description{
  Generalized Laguerre RSI
}
\references{
  \cr
  \url{http://www.mesasoftware.com/Papers/TIME\%20WARP.pdf}\cr
}

