\name{shortlist}
\alias{shortlist}
\title{Shortlisting Alternatives}
\usage{
shortlist(DList, Criteria, margin = 0, mode = "max", plot = TRUE,
  type = "named")
}
\arguments{
  \item{DList}{the decision list}

  \item{Criteria}{a matrix whose rows are all candidate
  alternatives and columns the shortlisting criteria,
  \code{Criteria[i, j]} denotes the score of alternative
  \code{i} on criterion \code{j}}

  \item{margin}{a vector of numbers specifying the
  tolerance margin used when comparing alternatives, or a
  single number if the same margin is used for all
  criteria}

  \item{mode}{"max" for critiria to be maximized; "min" for
  criteria to be minimized. Either a single string if all
  criteria have the same mode, or a vector specifying the
  mode for each criteria}

  \item{plot}{set to TRUE if you want the function to
  generate a 2D plot of the Pareto front (works only for
  problems with 2 goals)}

  \item{type}{how the shortlisted candidates are
  represented: 'named' for option names'; 'relative' for
  the option number in each decision; 'aboluste' for the
  absolute decision numbers.}
}
\value{
a list with the following elements: \itemize{

\item \code{$which.alternatives} a vector with the ID
number of the shortlisted alternatives in
\code{all_alternatives(DList)}

\item

}
}
\description{
Shortlist alternatives in a DList based on a set of
criteria. The function identifies the pareto optimal
solutions, presents these solutions in both the solution
and objective spaces, and identifies the closed and open
decisions in the shortlised solutions.
}
\seealso{
\code{\link{pareto}}
}

