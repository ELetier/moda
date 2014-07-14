#' Linear Utility
#' 
#' Returns the utility value associated to a goal level
#' assuming a linear utiltiy function for levels
#' associated to zero and one utilt.y
#' 
#' @param value   the value, or a vector of simulated values, associated with a goal
#' @param zeroUtility the goal level with a utiltiy of zero
#' @param oneUtility  the goal level with a utility of one
#' 
#' @export
linear_utility = function(value, zeroUtility, oneUtility){
  utility = (value - zeroUtility) / (oneUtility - zeroUtility)
  utility[utility < 0] = 0
  utility[utility > 1] = 1
  return(utility)
}

#' Checking whether a goal succeeded or failed 
#' 
#' Returns True if the goal level does not achieved the required 'must' value
#' 
#' @param value   the value, or a vector of simulated values, associated with a goal
#' @param must    the value the goal must achieve to avoid failure
#' @param modality  'min' for minimisation goals; 'max' for maximisation goals
#' 
#' 
#' @export 
is.failure = function(value, must, modality){
  if (modality == 'min'){return(value > must)}
  if (modality == 'max'){return(value < must)}
}

#' Sum vroduct of vectors and matrices
#' 
#' Vectorized implementation of the sumproduct of vector and matrices
#' 
#' @param x a vector or matrix
#' @param y a vector of matrix whose dimensions are compatible with x
#' for a sum product operation
#'
#' @export 
sumproduct = function(x, y){
  
  if(is.vector(x) && is.vector(y)){
    return(sum(x * y))
  }
  
  if(is.vector(x) && is.matrix(y)){
    return(rowSums(t(x * t(y))))
    # tcrossprod(x, y)[1] -- which is faster?
  }
  
  if(is.matrix(x) && is.vector(y)){
    return(rowSums(t(t(x) * y)))
  }
  
  if(is.matrix(x) && is.matrix(y)){
    return(rowSums(x * y))
  }
  
  stop("Invalid arguments to sumproduct")
  return(0)
  
}


