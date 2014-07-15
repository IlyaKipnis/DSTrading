\name{nlLaguerre}
\alias{nlLaguerre}
\title{Nonlinear Laguerre Trend Indicator}
\usage{
  nlLaguerre(HLC, n = 20, priceMethod = "Close")
}
\arguments{
  \item{HLC}{an xts object containing High, Low, and Close
  price data}

  \item{n}{the period for computation}

  \item{priceMethod}{use "Close" for the close, any other
  string will result in Ehlers's (H+L)/2 method.}
}
\value{
  an xts object
}
\description{
  Nonlinear Laguerre Trend Indicator
}
\note{
  TODO--implement loop in a faster language.
}
\references{
  \cr
  \url{http://www.mesasoftware.com/Seminars/TradeStation\%20World\%2005.pdf}\cr
}

