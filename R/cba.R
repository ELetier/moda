#' Statistical Cost-Benefit Analysis
#' 
#' Computes Expected Net Benefit(ENB), Loss Probability (LP), 
#' Probable Loss Magnitue (PLM), 
#' Expected Value of Total Perfect Information (EVTPI),
#' and Expected Value of Partial Perfect Information (EVPPI).
#' 
#' @param Cost        a costs simulation matrix: each colum corresond to one candidate
#' solution; each row to one simuation scenario
#' @param Benefit     a benefits simulation matrix: each colum corresond to one candidate
#' solution; each row to one simuation scenario
#' 
#' @examples
#' N = 10^4
#' cost = matrix(nrow = N, ncol = 2)
#' benefit = matrix(nrow = N, ncol = 2)
#' cost[ , 1] = rnorm(N, 100, 10)
#' benefit[, 1] = rnorm(N, 200, 10)
#' cost[, 2] = rnorm(N, 200, 50)
#' benefit[, 2] = rnorm(N, 400, 200)
#' 
#' result = cba(cost, benefit)
#' 
#' @export
cba = function(Cost, Benefit){
  
  n_sln = ncol(Cost)
  
  if (! is.null(colnames(Cost))){
    sln_names = colnames(Cost)
  } else if (! is.null(colnames(Benefit))){
    sln_names = colnames(Benefit)
  } else {
    sln_names = 1:n_sln
  }
  
  NB = Benefit - Cost
  ENB = colMeans(NB)
  star = which.max(ENB)
  
  Losing = NB<0
  LP = colMeans(Losing)
  safest = which.min(LP)
  
  Loss = Losing * NB
  PLM = colSums(Loss)/colSums(Losing)
  PLM[is.nan(PLM)] = 0
  
  expectations = matrix(nrow = 3, ncol = n_sln)
  rownames(expectations) = c("ENB", "LP", "PLM")
  colnames(expectations) = sln_names
  expectations[1, ] = ENB
  expectations[2, ] = LP
  expectations[3, ] = PLM
  
  total_pi = evtpi_and_risk(NB, Losing)
  
  partial_pi = matrix(nrow = (n_sln * 2), ncol = 2)
  colnames(partial_pi) = c("EVPPI", "Delta_LP")
  rownames(partial_pi) = vector(length = n_sln * 2)
  for (i in 1: n_sln){
    j = 2*(i-1) + 1
    rownames(partial_pi)[j] = paste("Cost(", sln_names[i], ")", sep = "")
    partial_pi[j, ] = evppi_and_risk(Cost[ , i], NB, Losing)
    
    rownames(partial_pi)[j+1] = paste("Benefit(", sln_names[i], ")", sep = "")
    partial_pi[j+1, ] = evppi_and_risk(Benefit[ , i], NB, Losing)
  }
  
  evppi_order = order(partial_pi[ , 1], decreasing = TRUE)
  partial_pi = partial_pi[evppi_order, ]
  
  return(list(
    star = star,
    safest = safest,
    expectations = expectations,
    information_value = list(total_pi = total_pi, partial_pi = partial_pi)
  ))
}