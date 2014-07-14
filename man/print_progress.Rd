\name{print_progress}
\alias{print_progress}
\title{Print progress through a loop}
\usage{
print_progress(pos, end)
}
\arguments{
  \item{pos}{number of the current iteration}

  \item{end}{total number of iternations}
}
\description{
Print progress through a loop
}
\examples{
N = 10^4
cat("Progress: ")
for (i in 1:N){
   print_progress(i, N)
   # insert loop body here
}
}

