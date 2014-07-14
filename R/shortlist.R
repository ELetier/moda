#' Shortlisting Alternatives
#' 
#' Shortlist alternatives in a DList based on a set of criteria. The function
#' identifies the pareto optimal solutions, presents these solutions in both
#' the solution and objective spaces, and identifies the closed and open 
#' decisions in the shortlised solutions.
#' 
#' @param DList       the decision list
#' 
#' @param Criteria    a matrix whose rows are all candidate alternatives
#' and columns the shortlisting criteria, \code{Criteria[i, j]} denotes
#' the score of alternative \code{i} on criterion \code{j}
#' 
#' 
#' @param margin    a vector of numbers specifying the tolerance margin 
#' used when comparing alternatives, or a single number if the same
#' margin is used for all criteria
#' 
#' @param mode       "max" for critiria to be maximized; "min" for criteria 
#' to be minimized. Either a single string if all criteria have the same mode,
#' or a vector specifying the mode for each criteria
#' 
#' @param plot      set to TRUE if you want the function to generate
#' a 2D plot of the Pareto front (works only for problems with 2 goals)
#' 
#' @param type      how the shortlisted candidates are represented: 'named' 
#' for option names'; 'relative' for the option number in each decision;
#' 'aboluste' for the absolute decision numbers. 
#' 
#' @return
#' a list with the following elements:
#' \itemize{
#' 
#' \item \code{$which.alternatives} a vector with the ID number of the shortlisted
#' alternatives in \code{all_alternatives(DList)}
#' 
#' \item 
#' 
#' }
#' 
#' @seealso
#' \code{\link{pareto}}
#' 
#' @export 
shortlist = function(DList, Criteria, margin = 0, mode = "max", 
                     plot = TRUE, type = 'named'){
  
  shortlisted = pareto(Criteria, margin, mode, plot)
  
  which_alternatives = which(shortlisted)
  
  values = Criteria[shortlisted, ]
  
  All_candidates = all_alternatives(DList, type)
  candidates = All_candidates[shortlisted, ]
  
  open = list()
  closed = list()
  Shortlisted_DList = apply(candidates, 2, unique)
  
  for(i in 1: length(DList)){
    if (length(Shortlisted_DList[[i]]) == 1){
      closed = c(closed, Shortlisted_DList[i])
    } else {
      open = c(open, Shortlisted_DList[i])
    }  
  }
  
  return(list(
    which_alternatives = which_alternatives,
    values = values,
    candidates = candidates,
    closed = closed,
    open = open
    ))
  
}
