\name{pareto}
\alias{pareto}
\title{Pareto optimality}
\usage{
pareto(A, margin = 0, mode = "max", plot = FALSE)
}
\arguments{
  \item{A}{a matrix where \code{ncol(A)} is the number of
  goals, \code{row(A)} is the number of alternatives, and
  \code{A[i, j]} denotes the value of alternative \code{i}
  for goal \code{j}.}

  \item{margin}{a vector of numbers specifying the
  tolerance margin used when comparing alternatives, or a
  single number if the same margin is used for all goals.}

  \item{mode}{"max" for goals to be maximized; "min" for
  goals to be minimized. Either a single string if all
  goals have the same mode, or a vector specifying the mode
  for each goal.}

  \item{plot}{set to TRUE if you want the function to
  generate a 2D plot of the Pareto front (works only for
  problems with 2 goals).}
}
\value{
The function returns a Boolean vector of size
\code{ncol(A)} indicating which solutions in \code{A} are
pareto-optimal.
}
\description{
Finds the non-dominated solutions in a set.
}
\details{
A solutions S1 is dominates a solution S2 if, and only if:
\itemize{ \item S1 is better than S2 for at least one goal,
i.e. G(S1) > G(S2) + margin(G) for some goal G; and \item
there is no goal G for which S1 is worse than S, i.e. G(S1)
>= G(2) for all goals G. }
}
\examples{
# A problem with 5 alternatives and 2 goals
Alternatives = c("A1", "A2", "A3", "A4", "A5")
G1 = c(4, 5, 0, 3, 2)
G2 = c(0, 3, 3, 5, 3)
A = cbind(G1, G2)
rownames(A) = Alternatives

# To compute the pareto optimal solutions
optimal = pareto(A)

# If you have 2 goals and want to see the plot
optimal = pareto(A, plot = TRUE)

# To return the indices of pareto optimal alternatives
optimal_alternatives = which(pareto(A))

# To return the objective matrix of all pareto optimal solutions
optimal_matrix = A[pareto(A), ]

# If both goals must be minimized
optimal = pareto(A, mode = "min")

# If G1 is to be minimised and G2 maximized
optimal = pareto(A, mode = c("min", "max"))

# To specify tolerance margins
optimal = pareto(A, margin = c(1, 2))
}

