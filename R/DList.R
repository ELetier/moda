#' Decision List 
#' 
#' A decision list is a list of decisions with their alternatives options.
#' The following functions return the number of decisions, number of options,
#' number of alternatives, option names and DSpec vector of a decision
#' list. The DSpec vector specifies the number of options in each
#' decision.
#' 
#' @param DList   a list of decsions where each decision has a vector
#' of possible options
#' 
#' @examples
#' DList = list(
#'    Decision1 = c("A", "B", "C"),
#'    Decision2 = c("X", "Y")
#' )
#' n_decisions(DList)
#' # 2
#' n_options(DList)
#' # 5
#' n_alternatives(DList)
#' #  6
#' dSpec(DList)
#' #  3 2
#' 
#' @seealso
#' \code{\link{all_alternatives}}
#' @name DList
NULL


#' @rdname DList
#' @export
n_decisions = function(DList){
  length(DList)
}

#' @rdname DList
#' @export
n_options = function(DList){
  sum(dSpec(DList))
}

#' @rdname DList
#' @export
n_alternatives = function(DList){
  prod(dSpec(DList))
}

#' @rdname DList
#' @export
options_names = function(DList){
  unlist(DList)
}

#' @rdname DList
#' @export
dSpec = function(DList){
  DSpec = c()
  for (i in 1:length(DList)){
    
    if (! is.vector(DList[[i]])){
      stop(paste("Options for", names(DList)[i], "is not a vector"))
    }
    
    DSpec[i] = length(DList[[i]]) 
  }
  names(DSpec) = names(DList)
  return(DSpec)
}

#' All alternatives of a DList
#' 
#' Generates all alternatives of a proplem with multitple decisions
#' 
#' @param DList   a list of decsions where each decision has a vector
#' of possible options
#' 
#' @param type    the type of decision vectors, either 'absolute', 'relative', 
#' or 'named'
#' 
#' @return
#' A matrix 
#' where each row is a decision vector representing one alternative.
#' Each decision vector specifies the option selected for each decision
#' by a number. If type = 'relative', the option number is the position
#' of that option in its decision vector; if type = 'absolute', the option
#' number is the position of that option in the full decision list; 
#' if type = 'named' the options are listed by their names.
#' 
#' @seealso
#' \code{
#' \link{DList}, \link{relative2absolute}, \link{relative2named}
#' }
#' 
#' @examples
#' DList = list(
#'    Decision1 = c("A", "B", "C"),
#'    Decision2 = c("X", "Y")
#' )
#' 
#' alt = all_alternatives(DList)
#' alt_relative = all_alternatives(DList, 'relative')
#' alt_named = all_alternatives(DList, 'named')
#' 
#' @export
all_alternatives = function(DList, type = 'absolute'){  
  DSpec = dSpec(DList)
  A = all_relative_decision_vectors(DSpec)
  if (type == 'relative'){return(A)}
  if (type == 'absolute'){return(relative2absolute(A, DSpec))}
  if (type == 'named'){return(relative2named(A, DList))}
  else return(NULL)
}

all_relative_decision_vectors = function(D){
  # By recursion over the lenght of D  
  if (length(D)==1) { 
    A = matrix(c(1:D), ncol=1, nrow=D)
    colnames(A) = names(D)
    return(A)
  } 
  else{
    head=D[1]
    tail=D[2:length(D)]
    
    B = all_relative_decision_vectors(tail)
    n = nrow(B)
    
    A=c()
    for (i in 1:head){
      X = cbind(rep(i,n), B)
      A = rbind(A, X)
    }
    colnames(A) = names(D)
    return(A)
  } 
}

#' Converts between relative and absolute decision vectors
#' 
#' @param A     one or more decision vectors (relative or absolute)
#' @param DSpec a vector specifying the number of options in each decision
#' 
#' @export
relative2absolute = function(A, DSpec){
  # the absolute vector is the relative vector plus a shift
  # where shift is the cumulative sum of DSpec starting at zero
  shift = cumsum(c(0, DSpec))[-(length(DSpec)+1)]
  names(shift) = names(DSpec)
  if (is.vector(A)){return(A + shift)}
  if (is.matrix(A)){return(t(apply(A, 1, function(d){d + shift})))}
}

#' @rdname relative2absolute
#' @export
absolute2relative = function(A, DSpec){
  shift = cumsum(c(0, DSpec))[-(length(DSpec)+1)]
  names(shift) = names(DSpec)
  if (is.vector(A)){return(A - shift)}
  if (is.matrix(A)){return(t(apply(A, 1, function(d){d - shift})))}
}

#' Transform a relativde decision vector into a named decision vector
#' 
#' @param A     one or more relative decision vectors
#' @param DList   a list of decsions where each decision has a vector
#' of possible options
#' 
#' 
#' @export
relative2named = function(A, DList){
  
  if (is.vector(A)){
    stopifnot(valid_relative_decision_vector(A, DList))
    result = vector(length = length(A))
    names(result) = names(DList)
    for (i in 1: length(A)){
      result[i] = DList[[i]][A[i]]
    }
    return(result)
  }
  
  if(is.matrix(A)){
    result = matrix(nrow = nrow(A), ncol = ncol(A))
    colnames(result) = colnames(A)
    rownames(result) = rownames(A)
    for (i in 1:nrow(A)){
      stopifnot(valid_relative_decision_vector(A[i, ], DList))
      for (j in 1:ncol(A)){
        result[i, j] = DList[[j]][A[i,j]]
      }
    }
    return(result) 
  }
}

#' Construct a DList from a DSpec
#' 
#' @param DSpec a vector specifying the number of options in each decision
#' 
#' @return
#' A decision list with default decision and option names. 
#' If DSpec has names, these names are used as decision names.
#' 
#' @examples
#' DSpec = c(3, 2)
#' DList = makeDList(DSpec)
#' 
#' @seealso
#' \code{\link{all_alternatives}}
#' 
#' @export
makeDList = function(DSpec){
  
  stopifnot(is.vector(DSpec) && is.numeric(DSpec))
  
  if (is.null(names(DSpec))) {
    names(DSpec) = paste("D", c(1:length(DSpec)), sep="")
  }
  
  if (length(unique(names(DSpec))) != length(names(DSpec))) {
    stop("Decision names in DSpec must all be different")
  }  
  
  DList = list()
  length(DList) = length(DSpec)
  names(DList) = names(DSpec)
  
  for (i in 1:length(DList)){
    DList[[i]] = paste(names(DSpec)[i], ":O", 1:DSpec[i], sep = "")
  }
  
  return(DList) # will return a DList with default decision and option names
}


valid_relative_decision_vector = function(d, DList){
  DSpec = dSpec(DList)
  length(d) == length(DSpec) && all(0<d) && all(d<=DSpec)
}

valid_absolute_decision_vector = function(d, DList){
  rd = absolute2relative(d, dSpec(DList))
  valid_relative_decision_vector(rd, DList)
}

