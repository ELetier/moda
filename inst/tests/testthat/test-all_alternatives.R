context("Testing DList.R")

# Postcondtion for all_alternatives
# Returns TRUE if post holds, throws exception otherwise
all_alternatives.post = function(Result, DList, type){
  
  # Result is a matrix of the right dimension
  stopifnot(ncol(Result) == n_decisions(DList))
  stopifnot(nrow(Result) == n_alternatives(DList))
  
  # The columns names are correct
  stopifnot(all.equal(colnames(Result), names(DList)))
  
  # All decision vectors in Result are valid
  for (i in 1:nrow(Result)){
    if (type == 'absolute'){
      stopifnot(valid_absolute_decision_vector(Result[i, ], DList))
    } else {
      stopifnot(valid_relative_decision_vector(Result[i, ], DList))
    }
  }
  
  # All decision vectors in A are different
  for (i in 1:nrow(Result)){
    for (j in 1:nrow(Result)){
      if (i != j) stopifnot( ! all(Result[i, ] == Result[j, ]) )
    }
  } 
  
  return(TRUE) 
}

test_that("all_alternatives is correct", {
  
  DList = list(
    Decision1 = c("A", "B", "C"),
    Decision2 = c("X", "Y")
  )
  
  A11 = all_alternatives(DList, type = 'absolute')
  expect_true(all_alternatives.post(A11, DList, type = 'absolute'))
  
  A12 = all_alternatives(DList, type = 'relative')
  expect_true(all_alternatives.post(A12, DList, type = 'relative'))
  
  
  # 4 binary decisions
  DList2 = makeDList(c(2, 2, 2, 2))
  A2 = all_alternatives(DList2, type = 'relative')
  expect_true(all_alternatives.post(A2, DList2, 'relative'))
  
  # A single binary decision
  D3 = makeDList(2)
  A3 = all_alternatives(D3, 'relative')
  expect_true(all_alternatives.post(A3, D3, 'relative'))
  
  
})
