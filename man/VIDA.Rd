\name{VIDA}
\alias{VIDA}
\title{Variable Index Dynamic Average}
\usage{
  VIDA(x, n = 9, m = 30, smoothConstant = 0.2, ...)
}
\arguments{
  \item{x}{a price series}

  \item{n}{a short period number over which to take a
  running SD}

  \item{m}{a long period number over which to take a
  running SD}

  \item{smoothConstant}{a parameter that affects smoothing.
  k*p(t) + (1-k)*p(t-1)}
}
\value{
  a stream of Variable Index Dynamic Average computations.
}
\description{
  Variable Index Dynamic Average
}
\references{
  \cr
  \url{"http://www.mesasoftware.com/Seminars/TradeStation\%20World\%2005.pdf"}
  \cr
}

