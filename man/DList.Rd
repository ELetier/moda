\name{DList}
\alias{DList}
\alias{dSpec}
\alias{n_alternatives}
\alias{n_decisions}
\alias{n_options}
\alias{options_names}
\title{Decision List}
\usage{
n_decisions(DList)

n_options(DList)

n_alternatives(DList)

options_names(DList)

dSpec(DList)
}
\arguments{
  \item{DList}{a list of decsions where each decision has a
  vector of possible options}
}
\description{
A decision list is a list of decisions with their
alternatives options. The following functions return the
number of decisions, number of options, number of
alternatives, option names and DSpec vector of a decision
list. The DSpec vector specifies the number of options in
each decision.
}
\examples{
DList = list(
   Decision1 = c("A", "B", "C"),
   Decision2 = c("X", "Y")
)
n_decisions(DList)
# 2
n_options(DList)
# 5
n_alternatives(DList)
#  6
dSpec(DList)
#  3 2
}
\seealso{
\code{\link{all_alternatives}}
}

