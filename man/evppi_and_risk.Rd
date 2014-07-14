\name{evppi_and_risk}
\alias{evppi_and_risk}
\title{Expected Value of Partial Pefect Information and Risk}
\usage{
evppi_and_risk(x, G, Failure)
}
\arguments{
  \item{x}{the parameter simulation vector}

  \item{G}{the objective simuation matrix for all
  alternatives}

  \item{Failure}{the failure simulation matrix for all
  alternatives}
}
\value{
A vector with elements \code{evppi} and \code{delta_risk}.
If \code{delta_risk} is negative, then risk is reduced.
}
\description{
Computes the expexted value of partial perfect information
(evppi) for a single parameter and the impact of seeking
such information on risk.  The function extends Sadatsafavi
et al's method for computing evppi.
}
\seealso{
\code{\link{evppi}}, \code{\link{evtpi}},
\code{\link{evtpi_and_risk}}
}

