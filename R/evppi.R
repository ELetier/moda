#' Expected Value of Partial Pefect Information
#' 
#'  Function for efficiently computing the expected value of partial 
#'  perfect information (EVPPI) using Sadatssfavi et al's method 
#'  (see reference).
#'  
#'  @reference 
#'  Sadatsafavi et al. 
#'  "Need for Speed: An efficient algorithm for calculation of single-parameter 
#'  expected value of partial perfect information", Value in Health - March 2013
#'  
#'  @param x      the parameter simulation vector
#'  @param G      the objective simuation matrix for all alternatives
#'  
#'  @seealso 
#'  \code{\link{evppi_and_risk}}, 
#'  \code{\link{evtpi}}, 
#'  \code{\link{evtpi_and_risk}}
#'  
#'  @export
evppi = function(x, G){
  
  # Checking predconditions
  if (!is.vector(x)){
    stop("x must be a vector of simulated parameters values")
  }
  
  if(length(unique(x))==1){
    warning("The input parameter is not stochastic")
    return(0)
  }
  
  if(length(x) != nrow(G)){
    stop("The parameter and objective simulations must be of same size")
  }
  
  # Sorting simulations in ascending order of parameter's value
  ordering = order(x)
  x = x[ordering]
  G = G[ordering, ]
  
  # Finding the segmentation points
  segPoints = find_segPoints(G)
  
  # If no seg points, then decision change from knowing x, so evppi = 0
  if (is.null(segPoints)) {
    evppi = 0
    return(evppi)
  }
  
  # Computing evppi by iterating over segPoints
  Sum_G_with_ppi = 0
  for (i in 1: (length(segPoints) - 1)){
      segment = (1 + segPoints[i]): segPoints[i + 1]
      Sum_G_with_ppi = Sum_G_with_ppi + max(colSums(G[segment, , drop = FALSE]))
  }
  evppi = (Sum_G_with_ppi / nrow(G)) - max(colMeans(G))
  return(evppi)
}

#' Expected Value of Partial Pefect Information and Risk
#' 
#'  Computes the expexted value of partial perfect information (evppi) for
#'  a single parameter and the impact of seeking such information on risk.
#'  The function extends Sadatsafavi et al's method for computing evppi.
#'  
#'  @reference 
#'  For evppi:
#'  Sadatsafavi et al. 
#'  "Need for Speed: An efficient algorithm for calculation of single-parameter 
#'  expected value of partial perfect information", Value in Health - March 2013
#'  
#'  For impact on risk:
#'  E. Letier, D. Stefan, E. T. Barr, Uncertainty, Risk, and Information Value 
#'  in Software Requirements and Architecture, 
#'  Proc. 36th International Conference on Software Engineering 2014 (ICSE 2014)
#'  
#'  @param x      the parameter simulation vector
#'  @param G      the objective simuation matrix for all alternatives
#'  @param Failure  the failure simulation matrix for all alternatives
#'  
#'  @return
#'  A vector with elements \code{evppi} and \code{delta_risk}. 
#'  If \code{delta_risk} is negative, then risk is reduced.
#'  
#'  @seealso 
#'  \code{\link{evppi}}, 
#'  \code{\link{evtpi}}, 
#'  \code{\link{evtpi_and_risk}}
#'   
#'  @export
evppi_and_risk = function(x, G, Failure){
  
  # Checking predconditions
  if (!is.vector(x)){
    stop("x must be a vector of simulated parameters values")
  }
  
  if(length(unique(x))==1){
    warning("The input parameter is not stochastic")
    return(c(evppi = 0, delta_risk = 0))           
  }
  
  if(length(x) != nrow(G) || length(x) != nrow(Failure)){
    stop("The parameter, objective, and failure simulations must be of same size")
  }
  
  if(ncol(G) != ncol(Failure)){
    stop("The objective and failure matrix must have the same columns")
  }
  
  
  # Sorting simulations in ascending order of parameter's value
  ordering = order(x)
  x = x[ordering]
  G = G[ordering, ]
  
  # Finding the segmentation points
  segPoints = find_segPoints(G)
  
  # If no seg points, then decision change from knowing x, so evppi = 0
  if (is.null(segPoints)) {
    return(c(evppi = 0, delta_risk = 0))
  }
  
  # Computing evppi by iterating over segPoints
  Sum_G_with_ppi = 0
  Sum_Failure_with_ppi = 0
  for (i in 1: (length(segPoints) - 1)){
    segment = (1 + segPoints[i]): segPoints[i + 1]
    Sums_in_segment = colSums(G[segment, , drop = FALSE]) 
    
    Sum_G_with_ppi = Sum_G_with_ppi + max(Sums_in_segment)
    
    star_in_segment = which.max(Sums_in_segment)
    Sum_Failure_with_ppi = 
      Sum_Failure_with_ppi + sum(Failure[segment, star_in_segment])
    
  }
  N = nrow(G)
  evppi = (Sum_G_with_ppi / N) - max(colMeans(G))
  
  star = which.max(colMeans(G))
  delta_risk = (Sum_Failure_with_ppi / N)  - mean(Failure[ , star])
  
  return(c(evppi = evppi, delta_risk = delta_risk))
}

# 
# Generates segmentations points for computing evppi
# Returns a vector of simulation indices or NULL
#
find_segPoints = function(G, plot = FALSE){
  
  nbs = G #renaming G into nbs to be consistent with initial implementation
  
  d = ncol(G)
  n = nrow(G)
  nSegs<-matrix(1,d,d)
  segPoints<-c()
  
  for(i in 1:(d-1))
    for(j in (i+1):d) {
      # message(paste('Fitting ',nSegs[i,j],' segmentation points for decisions ',i,j))
      
      cm<-cumsum(nbs[,i]-nbs[,j])/n
      
      if(nSegs[i,j]==1) {
        l<-which.min(cm)
        u<-which.max(cm)
        if(cm[u]-max(cm[1],cm[n])>min(cm[1],cm[n])-cm[l])
          segPoint<-u
        else
          segPoint<-l
        if (segPoint>1 && segPoint<n)
          segPoints<-c(segPoints, segPoint)
      }
      
      if(nSegs[i,j]==2) {
                
        distMaxMin<-0
        distMinMax<-0
        minL<-Inf
        maxL<--Inf
        
        for(k in 1:n) {
          #max-min pattern
          if(cm[k]>maxL) {
            maxLP<-k
            maxL<-cm[k]
          } else if(maxL-cm[k]>distMaxMin) {
              distMaxMin<-maxL-cm[k]
              segMaxMinL<-maxLP	
              segMaxMinR<-k
            }	
          
          
          #min-max pattern
          if(cm[k]<minL) {
            minLP<-k
            minL<-cm[k]
          } else if(cm[k]-minL>distMinMax) {
              distMinMax<-cm[k]-minL
              segMinMaxL<-minLP	
              segMinMaxR<-k
            }	
        }
        
        siMaxMin<-cm[segMaxMinL]+distMaxMin+(cm[n]-cm[segMaxMinR])
        siMinMax<--cm[segMaxMinL]+distMinMax-(cm[n]-cm[segMinMaxR])
        
        if(siMaxMin>siMinMax) {
          segPoint<-c(segMaxMinL,segMaxMinR)
        } else {
          segPoint<-c(segMinMaxL,segMinMaxR)
        }
        
        if (segPoint[1]>1 && segPoint[1]<n){
          segPoints<-c(segPoints, segPoint[1])
        }
        
        if (segPoint[2]>1 && segPoint[2]<n){
          segPoints<-c(segPoints, segPoint[2])
        }
      }
    }
  
  if(is.null(segPoints)) {return(NULL)}
  
  segPoints = unique(c(0, segPoints[order(segPoints)], n))
  
  return(segPoints)
}


