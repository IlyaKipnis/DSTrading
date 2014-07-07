\name{GFRAMA}
\alias{GFRAMA}
\title{Generalized FRAMA}
\usage{
  GFRAMA(x, n = 20, FC = 1, SC = 200, ...)
}
\arguments{
  \item{x}{a time series}

  \item{n}{a lookback period}

  \item{FC}{fast constant for an EMA}

  \item{SC}{slow constant for an EMA}
}
\value{
  the FRAMA for the time series
}
\description{
  the generalized FRAMA (see: FRAMA)
}

