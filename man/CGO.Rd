\name{CGO}
\alias{CGO}
\title{Center of Gravity Oscillator}
\usage{
  CGO(HLC, n = 10, priceMethod = "Close", scale = TRUE,
    nSD = 100)
}
\arguments{
  \item{HLC}{an xts object containing High, Low, and Close
  price data}

  \item{n}{the period for computation}

  \item{priceMethod}{use "Close" for the close, any other
  string will result in Ehlers's (H+L)/2 method.}

  \item{scale}{an option whether or not to scale the
  oscillator. Defaults to TRUE.}

  \item{nSD}{an argument for scaling. Sets the number of
  days over which to compute a running volatility. In
  effect, a larger nSD value will assign more extreme
  values to the scaled oscillator.}
}
\value{
  an xts object
}
\description{
  Center of Gravity Oscillator
}
\references{
  \cr
  \url{http://www.mesasoftware.com/Papers/The\%20CG\%20Oscillator.pdf}\cr
}

