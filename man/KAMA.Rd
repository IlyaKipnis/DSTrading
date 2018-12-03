\name{KAMA}
\alias{KAMA}
\title{Kaufman Adaptive Moving Average}
\usage{
  KAMA(HLC, nER = 10, nFast = 2, nSlow = 30,
    priceMethod = "Close")
}
\arguments{
  \item{HLC}{an xts object containing High, Low, and Close
  price data}

  \item{nER}{a parameter for the Kaufman efficiency ratio
  lookback period}

  \item{nFast}{the fast limit of the adaptive EMA}

  \item{nSlow}{the slow limit of the adaptive EMA}

  \item{priceMethod}{use "Close" for the close, any other
  string will result in Ehlers's (H+L)/2 method.}
}
\value{
  an xts object
}
\description{
  Kaufman Adaptive Moving Average
}
\references{
  \cr
  \url{http://www.mesasoftware.com/Seminars/TradeStation\%20World\%2005.pdf}\cr
}

