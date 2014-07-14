#' Pareto optimality
#' 
#' Finds the non-dominated solutions in a set.
#'
#' @param A     a matrix where \code{ncol(A)} is the number of goals, 
#'              \code{row(A)} is the number of alternatives, and
#'              \code{A[i, j]} denotes the value of alternative \code{i}
#'              for goal \code{j}.
#' 
#' @param margin    a vector of numbers specifying the tolerance margin 
#' used when comparing alternatives, or a single number if the same
#' margin is used for all goals.
#' 
#' @param mode       "max" for goals to be maximized; "min" for goals 
#' to be minimized. Either a single string if all goals have the same mode,
#' or a vector specifying the mode for each goal.
#' 
#' @param plot      set to TRUE if you want the function to generate
#' a 2D plot of the Pareto front (works only for problems with 2 goals).
#' 
#' @return
#' The function returns a Boolean vector of size \code{ncol(A)} indicating
#' which solutions in \code{A} are pareto-optimal.
#' 
#' @details
#' A solutions S1 is dominates a solution S2 if, and only if:
#' \itemize{
#' \item S1 is better than S2 for at least one goal, 
#' i.e. G(S1) > G(S2) + margin(G) for some goal G; and
#' \item there is no goal G for which S1 is worse than S, i.e.
#' G(S1) >= G(2) for all goals G.
#' }
#' 
#' @examples
#' # A problem with 5 alternatives and 2 goals
#' Alternatives = c("A1", "A2", "A3", "A4", "A5")
#' G1 = c(4, 5, 0, 3, 2)
#' G2 = c(0, 3, 3, 5, 3)
#' A = cbind(G1, G2)
#' rownames(A) = Alternatives
#'   
#' # To compute the pareto optimal solutions
#' optimal = pareto(A)
#' 
#' # If you have 2 goals and want to see the plot
#' optimal = pareto(A, plot = TRUE)
#' 
#' # To return the indices of pareto optimal alternatives
#' optimal_alternatives = which(pareto(A))
#' 
#' # To return the objective matrix of all pareto optimal solutions
#' optimal_matrix = A[pareto(A), ]
#' 
#' # If both goals must be minimized
#' optimal = pareto(A, mode = "min")
#' 
#' # If G1 is to be minimised and G2 maximized
#' optimal = pareto(A, mode = c("min", "max"))
#' 
#' # To specify tolerance margins
#' optimal = pareto(A, margin = c(1, 2))
#' 
#' 
#' @export
pareto = function(A, margin = 0, mode = "max", plot = FALSE){
  
  # Checking validity of A
  if(! is.matrix(A)) {
    stop("First argument must be a matrix. Type help(pareto)")
  }
  
  
  # Checking margin validaty 
  if (length(margin) != 1 && length(margin) != ncol(A)){
    stop("Margin must be a single number or a vector of number of same length
         as the number of objectives in A")
  }
    
  
  # Checks validity and handle the mode argument:
  # We reverse the sign of any objective to be minimized
  # so that all objectives now have to be maximized.
  AM = handle_mode(A, mode)
  
  n = nrow(A)
  
  is.pareto = rep(TRUE, n)
  names(is.pareto) = rownames(A)  
  i=1
  
  while(i < n){    
    j = i+1    
    while (is.pareto[i] && j <= n){
      if (dominates(AM[i, ], AM[j, ], margin)) {is.pareto[j] = FALSE}
      else if (dominates(AM[j, ], AM[i, ], margin)) {is.pareto[i] = FALSE}
      j = j+1
    }    
    i = i +1        
  }
  
  if (plot) { plot.pareto(is.pareto, A) }
  return(is.pareto)
}

handle_mode = function(A, mode="max"){
  
  if(length(mode) !=1 && length(mode) != ncol(A)){
    stop("Invalid mode argument: 
         its length must be 1 or the number of objectives")
  }
  
  if (length(mode) == 1){
    if (mode == "min"){
      return(-A)
    } else if (mode != "max"){
      stop("Invalid mode argument")
    } 
  }
  
  for (i in 1:length(mode)){
    if (mode[i] == "min") {
      A[ , i] = -A[ , i]
    } else if (mode[i] != "max"){
      stop("Invalid mode argument")
    }
  }
  return(A)
}


dominates = function (S1, S2, margin=0){
  all(S1 >= S2) && any(S1 > S2 + margin)
}

# plots a 2D pareto front
plot.pareto = function(pareto, A){
  if (ncol(A)>2){
    warning("Can only plot problems with 2 objectives \n")
  } else{
    plot(A, pch = 20, col = 8)
    points(A[pareto, ], pch = 4, col = 2)
  }
} 

# Postcondition used for testing
# returns True if pareto is the set of non-dominated solutions in A
# otherwise throws an error
pareto.postcondition = function (pareto, A, margin = 0, mode = "max"){
  A = handle_mode(A, mode)
  n= nrow(A)
  for (i in 1:n){
    if (pareto[i]) {
      # no j should dominate i
      for (j in 1:n){
        if (dominates(A[j, ], A[i, ], margin)) {
          stop("pareto[", i, "] is true but ", i, " is dominated by ", j)
        }
      }
    } else {
      # some j must dominate i
      dominated = FALSE
      for (j in 1:n){
        if (dominates(A[j, ], A[i, ], margin)) {dominated = TRUE}
      }
      
      if (! dominated) {
        stop("pareto[", i, "] is false but ", i, " is non dominated")
      }
    }
  }
  return(TRUE)
}





