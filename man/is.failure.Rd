\name{is.failure}
\alias{is.failure}
\title{Checking whether a goal succeeded or failed}
\usage{
is.failure(value, must, modality)
}
\arguments{
  \item{value}{the value, or a vector of simulated values,
  associated with a goal}

  \item{must}{the value the goal must achieve to avoid
  failure}

  \item{modality}{'min' for minimisation goals; 'max' for
  maximisation goals}
}
\description{
Returns True if the goal level does not achieved the
required 'must' value
}

