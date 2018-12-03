\name{cyberCycle}
\alias{cyberCycle}
\title{Ehlers Cyber Cycle}
\usage{
  cyberCycle(HLC, alpha = 0.07, priceMethod = "Close",
    iFish = TRUE)
}
\arguments{
  \item{HLC}{an xts object containing High, Low, and Close
  price data}

  \item{alpha}{a tuning parameter}

  \item{priceMethod}{use "Close" for the close, any other
  string will result in Ehlers's (H+L)/2 method.}

  \item{iFish}{defaults to TRUE. Whether or not to apply
  the inverse fisher transform. See ?iFish}
}
\value{
  an xts object
}
\description{
  Ehlers Cyber Cycle
}
\references{
  \cr
  \url{http://www.mesasoftware.com/Papers/THE\%20INVERSE\%20FISHER\%20TRANSFORM.pdf}\cr
}

