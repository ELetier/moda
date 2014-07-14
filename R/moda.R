#' Multi-Objectice Decision Analysis (Moda)
#' 
#' Moda provides functions for modelling and solving multi-objective
#' decision problem under uncertainty.
#' 
#' The first step in using the package is to define
#' and simulate the decision problem. 
#' You can use a \code{\link{DList}} to specify a set of decisions
#' and use the function \code{\link{all_alternatives}} 
#' to generate the whole solution space.
#' 
#' Once you have generated Monte-Carlo the simulation matrix for your
#' decision problem, you can
#' \itemize{
#' \item shortlist the best solutions using \code{\link{shortlist}} 
#' or \code{\link{pareto}};
#' \item perform information value analysis using \code{\link{evtpi}} 
#' and \code{\link{evppi}}.
#' }
#' 
#' If your problem is a cost-benefit decision problem, 
#' you can use the function \code{\link{cba}} for statistical cost-benefit analysis.
#' 
#' @docType package
#' @name moda
NULL