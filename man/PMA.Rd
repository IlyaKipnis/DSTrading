\name{PMA}
\alias{PMA}
\title{Ehlers Predictive Moving Averages}
\usage{
  PMA(HLC, n, priceMethod = "Close")
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
  Ehlers Predictive Moving Averages
}
\references{
  Rocket Science For Traders, Chapter 20
}

