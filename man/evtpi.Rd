\name{evtpi}
\alias{evtpi}
\title{Expected Value of Total Perfect Information}
\usage{
evtpi(Value)
}
\arguments{
  \item{Value}{a matrix of value simulations, usually
  produced as output of a Monte-Carlo simulation. The
  number of columns is the number of alternatives under
  consideration, the number of rows is the number of
  simulations. The element \code{Value[i,j]} denotes the
  value obtained by alternative \code{j} in simulation
  \code{i}.}
}
\value{
The expected value of total perfect information.
}
\description{
A function to compute the expected value of total perfect
information
}
\examples{
# A simple simuation of the value of the two alternatives
N = 10^4
Value = matrix(ncol = 2, nrow = N)
Value[ , 1] = rnorm(N, mean = 100, sd = 50)
Value[ , 2] = rnorm(N, mean = 80, sd = 10)

evtpi = evtpi(Value)
}
\references{
The concept of expected value of total perfect information
was first introduced in

Howard, R.A., "Information Value Theory," IEEE Transactions
on Systems Science and Cybernetics, vol.2, no.1, pp.22,26,
Aug. 1966
}
\seealso{
\code{\link{evtpi_and_risk}}
}

