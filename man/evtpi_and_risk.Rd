\name{evtpi_and_risk}
\alias{evtpi_and_risk}
\title{Expected Value of Total Perfect Information and Risk}
\usage{
evtpi_and_risk(Value, Failure)
}
\arguments{
  \item{Value}{a matrix of value simulations, usually
  produced as output of a Monte-Carlo simulation.}

  \item{Failure}{a matrix of Boolean values, having the
  same dimensions as \code{Value}, and such that
  \code{Failure[i, j]} is \code{TRUE} if and only if
  alternative \code{i} fails in scenario \code{i}. What it
  means for an alternative to fail is to be defined by the
  users of this function.}
}
\value{
A vector with elements \code{evtpi} and \code{delta_risk}.
If \code{delta_risk} is negative, then risk is reduced.
}
\description{
Computes the expected value of total perfect information
and the expected difference in risk from optimising value
with total perfect information.
}
\examples{
# A simple MC simuation of the value of the two alternatives
N = 10^4
Value = matrix(ncol = 2, nrow = N)
Value[ , 1] = rnorm(N, mean = 100, sd = 50)
Value[ , 2] = rnorm(N, mean = 80, sd = 10)

Failure = (Value < 0)

x = evtpi_and_risk(Value, Failure)
}
\references{
E. Letier, D. Stefan, E. T. Barr, Uncertainty, Risk, and
Information Value in Software Requirements and
Architecture, Proc. 36th International Conference on
Software Engineering 2014 (ICSE 2014)
}
\seealso{
\code{\link{evtpi}}
}

