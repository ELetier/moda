#' Expected Value of Total Perfect Information
#' 
#' A function to compute the expected value of total perfect information
#' 
#' @param Value   a matrix of value simulations, usually produced as output
#' of a Monte-Carlo simulation. 
#' The number of columns is the number of alternatives under consideration, 
#' the number of rows is the number of simulations. 
#' The element \code{Value[i,j]} denotes the value obtained
#' by alternative \code{j} in simulation \code{i}.
#' 
#' @return The expected value of total perfect information.
#' 
#' @references
#' The concept of expected value of total perfect information 
#' was first introduced in 
#' 
#' Howard, R.A., "Information Value Theory," 
#' IEEE Transactions on Systems Science and Cybernetics, 
#' vol.2, no.1, pp.22,26, Aug. 1966
#' 
#' @seealso \code{\link{evtpi_and_risk}}
#' 
#' @examples
#' # A simple simuation of the value of the two alternatives
#' N = 10^4
#' Value = matrix(ncol = 2, nrow = N)
#' Value[ , 1] = rnorm(N, mean = 100, sd = 50)
#' Value[ , 2] = rnorm(N, mean = 80, sd = 10)
#' 
#' evtpi = evtpi(Value)
#' 
#' @export
evtpi = function(Value){
  mean(apply(Value, 1, max)) - max(colMeans(Value))
}

#' Expected Value of Total Perfect Information and Risk
#' 
#' Computes the expected value of total perfect information and the expected
#' difference in risk from optimising value with total perfect information.
#' 
#' @param Value   a matrix of value simulations, usually produced as output
#' of a Monte-Carlo simulation.
#' 
#' @param Failure  a matrix of Boolean values, having the same dimensions as
#' \code{Value}, and such that \code{Failure[i, j]} is \code{TRUE} if and only
#' if alternative \code{i} fails in scenario \code{i}. What it means
#' for an alternative to fail is to be defined by the users of this function.
#' 
#' @return
#' A vector with elements \code{evtpi} and \code{delta_risk}. 
#' If \code{delta_risk} is negative, then risk is reduced.
#' 
#' 
#' @references
#' E. Letier, D. Stefan, E. T. Barr, Uncertainty, Risk, and Information Value 
#' in Software Requirements and Architecture, 
#' Proc. 36th International Conference on Software Engineering 2014 (ICSE 2014)
#' 
#' @examples
#' # A simple MC simuation of the value of the two alternatives
#' N = 10^4
#' Value = matrix(ncol = 2, nrow = N)
#' Value[ , 1] = rnorm(N, mean = 100, sd = 50)
#' Value[ , 2] = rnorm(N, mean = 80, sd = 10)
#' 
#' Failure = (Value < 0)
#' 
#' x = evtpi_and_risk(Value, Failure)
#' 
#' @seealso \code{\link{evtpi}}
#' @export
evtpi_and_risk = function(Value, Failure){
  
  if (! is.matrix(Value)) stop("The Value argument must be a matrix")
  if (! is.matrix(Failure)) stop("The Failure argument must be a matrix")
  
  if(ncol(Value) != ncol(Failure) || nrow(Value) != nrow(Failure)){
    stop("The Value and Failure matrices must have the same dimensions")
  }
  
  evtpi = evtpi(Value)
  delta_risk = delta_risk_tpi(Value, Failure)
  result = c(evtpi = evtpi, delta_risk = delta_risk)
  return(result)
}


delta_risk_tpi = function(Value, Failure){
  
  star = which.max(colMeans(Value))
  
  Failure_tpi = c() 
  N = nrow(Failure)
  i = 1
  
  while (i<=N){
    scenario_star = which.max(Value[i, ])
    Failure_tpi[i] = Failure[i, scenario_star]
    i = i + 1
  }
  
  delta_risk_tpi = mean(Failure_tpi) - mean(Failure[ , star])
  
  return(delta_risk_tpi)
  
}

