\name{makeDList}
\alias{makeDList}
\title{Construct a DList from a DSpec}
\usage{
makeDList(DSpec)
}
\arguments{
  \item{DSpec}{a vector specifying the number of options in
  each decision}
}
\value{
A decision list with default decision and option names. If
DSpec has names, these names are used as decision names.
}
\description{
Construct a DList from a DSpec
}
\examples{
DSpec = c(3, 2)
DList = makeDList(DSpec)
}
\seealso{
\code{\link{all_alternatives}}
}

