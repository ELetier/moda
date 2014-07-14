context("Testing DList.R")

test_that("DList attributes work",{
  
  DList = list(
      Decision1 = c("A", "B", "C"),
      Decision2 = c("X", "Y")
    )
  expect_equal(n_decisions(DList), 2)
  expect_equal(n_options(DList), 5)
  expect_equal(n_alternatives(DList), 6)
  
  expect_equivalent(options_names(DList), c("A", "B", "C", "X", "Y"))
  
  expect_equivalent(dSpec(DList), c(3, 2))
  expect_equal(names(dSpec(DList)), c("Decision1", "Decision2"))  
  
})

test_that("relative2absolute works",{
  
  DSpec = c(3, 2)
  
  rd1 = c(1, 1)
  rd2 = c(2, 1)
  rd3 = c(3, 2)
  
  expect_equal(relative2absolute(rd1, DSpec), c(1, 4))
  expect_equal(relative2absolute(rd2, DSpec), c(2, 4))
  expect_equal(relative2absolute(rd3, DSpec), c(3, 5))
  
  A = rbind(rd1, rd2, rd3)
  expect_equal(relative2absolute(A, DSpec)[1, ], c(1, 4))
  
})

test_that("absolute2relative works",{
  
  DSpec = c(3, 2)
  
  d1 = c(1, 4)
  d2 = c(2, 4)
  d3 = c(3, 5)
  
  expect_equal(absolute2relative(d1, DSpec), c(1, 1))
  expect_equal(absolute2relative(d2, DSpec), c(2, 1))
  expect_equal(absolute2relative(d3, DSpec), c(3, 2))
  
  A = rbind(d1, d2, d3)
  expect_equal(absolute2relative(A, DSpec)[1, ], c(1, 1))
  
})

test_that("decision vector validation checks are correct",{
  
  DList = list(
    Decision1 = c("A", "B", "C"),
    Decision2 = c("X", "Y")
  )
   
  d1 = c(1, 1)
  expect_true(valid_relative_decision_vector(d1, DList))
  expect_false(valid_absolute_decision_vector(d1, DList))
  
  d2 = c(1, 4)
  expect_false(valid_relative_decision_vector(d2, DList))
  expect_true(valid_absolute_decision_vector(d2, DList))  
  
})

test_that("relative2named works with relative decision vectors",{
  
  DList = list(
    Decision1 = c("A", "B", "C"),
    Decision2 = c("X", "Y")
  )
  
  d1 = c(1, 1)
  
  n1 = c("A", "X")
  names(n1) = names(DList)
  
  expect_equal(relative2named(d1, DList), n1)
  
  
  d2 = c(2, 1)
  d3 = c(3, 2)
  A = rbind(d1, d2, d3)
  colnames(A) = names(DList)
  
  n2 = c("B", "X")
  n3 = c("C", "Y")
  N = rbind(n1, n2, n3)
  colnames(N) = names(DList)
  rownames(N) = rownames(A)
  
  expect_equal(relative2named(A, DList), N)
  
})