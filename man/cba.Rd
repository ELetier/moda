\name{cba}
\alias{cba}
\title{Statistical Cost-Benefit Analysis}
\usage{
cba(Cost, Benefit)
}
\arguments{
  \item{Cost}{a costs simulation matrix: each colum
  corresond to one candidate solution; each row to one
  simuation scenario}

  \item{Benefit}{a benefits simulation matrix: each colum
  corresond to one candidate solution; each row to one
  simuation scenario}
}
\description{
Computes Expected Net Benefit(ENB), Loss Probability (LP),
Probable Loss Magnitue (PLM), Expected Value of Total
Perfect Information (EVTPI), and Expected Value of Partial
Perfect Information (EVPPI).
}
\examples{
N = 10^4
cost = matrix(nrow = N, ncol = 2)
benefit = matrix(nrow = N, ncol = 2)
cost[ , 1] = rnorm(N, 100, 10)
benefit[, 1] = rnorm(N, 200, 10)
cost[, 2] = rnorm(N, 200, 50)
benefit[, 2] = rnorm(N, 400, 200)

result = cba(cost, benefit)
}

