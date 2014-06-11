\name{FRAMA}
\alias{FRAMA}
\title{FRactal Adaptive Moving Average}
\usage{
  FRAMA(HLC, n = 20, FC = 1, SC = 200, triggerLag = 1, ...)
}
\arguments{
  \item{HLC}{an HLC price series}

  \item{n}{a lookback period}

  \item{FC}{a fast constant--aka lowest n for an EMA}

  \item{SC}{a slow constant--aka lowest n for an EMA}

  \item{triggerLag}{a number of days by which to lag the
  original computation}
}
\value{
  an xts containing the FRAMA and the trigger
}
\description{
  FRactal Adaptive Moving Average
}
\references{
  \cr\url{http://www.mesasoftware.com/Papers/FRAMA.pdf}
  \cr\url{http://etfhq.com/blog/2010/09/30/fractal-adaptive-moving-average-frama/}
}

