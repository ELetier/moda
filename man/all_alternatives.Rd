\name{all_alternatives}
\alias{all_alternatives}
\title{All alternatives of a DList}
\usage{
all_alternatives(DList, type = "absolute")
}
\arguments{
  \item{DList}{a list of decsions where each decision has a
  vector of possible options}

  \item{type}{the type of decision vectors, either
  'absolute', 'relative', or 'named'}
}
\value{
A matrix where each row is a decision vector representing
one alternative. Each decision vector specifies the option
selected for each decision by a number. If type =
'relative', the option number is the position of that
option in its decision vector; if type = 'absolute', the
option number is the position of that option in the full
decision list; if type = 'named' the options are listed by
their names.
}
\description{
Generates all alternatives of a proplem with multitple
decisions
}
\examples{
DList = list(
   Decision1 = c("A", "B", "C"),
   Decision2 = c("X", "Y")
)

alt = all_alternatives(DList)
alt_relative = all_alternatives(DList, 'relative')
alt_named = all_alternatives(DList, 'named')
}
\seealso{
\code{ \link{DList}, \link{relative2absolute},
\link{relative2named} }
}

