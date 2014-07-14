context("Testing pareto.R")

test_that("pareto is correct", {
  
  # Some goals:
  G1 = c(4, 5, 0, 3, 2)
  G2 = c(0, 3, 3, 5, 2)
  G3 = c(0, 3, -1, 4, 3)
  G4 = c(1, 2, 1, 2, 1)
  
  A_names = c("A1", "A2", "A3", "A4", "A5")
  names(G1) = A_names
  names(G2) = A_names
  
  # Testing with 2 objectives
  P2 = cbind(G1, G2)
  o2 = pareto(P2)
  expected_o2 = c(FALSE, TRUE, FALSE, TRUE, FALSE)
  names(expected_o2) = A_names
  
  expect_equal(o2, expected_o2)
  expect_true(pareto.postcondition(o2, P2))
  
  # Testing margins
  
  # If margin is small
  # Then pareto set stays the same
  o2m = pareto(P2, margin = 0.01)
  expect_equal(o2m, expected_o2)
  expect_true(pareto.postcondition(o2m, P2, margin = 0.01))
  
  # If the margins are set to the max value for each goal
  # Then all solutions should be pareto optimal
  big_margins = apply(P2, 2, max)
  expect_true(all(pareto(P2, big_margins)))
  
  # checking modes
  
  o2min = pareto(P2, mode = "min")
  expected_o2min = c(TRUE, FALSE, TRUE, FALSE, TRUE)
  names(expected_o2min) = A_names
  expect_equal(o2min, expected_o2min)
  
  o2min_max = pareto(P2, mode = c("min", "max"))
  expected_o2min_max = c(FALSE, FALSE, TRUE, TRUE, FALSE)
  names(expected_o2min_max) = A_names
  expect_equal(o2min_max, expected_o2min_max)
  
  o2max_min = pareto(P2, mode = c("max", "min"))
  expected_o2max_min = c(TRUE, TRUE, FALSE, FALSE, FALSE)
  names(expected_o2max_min) = A_names
  expect_equal(o2max_min, expected_o2max_min)
  
  # Checking problems with more than 2 goals
  
  P3 = cbind(G1, G2, G3)
  o3 = pareto(P3)
  expect_true(pareto.postcondition(o3, P2))
  
  P4 = cbind(G1, G2, G3, G4)
  o4 = pareto(P4)
  expect_true(pareto.postcondition(o4, P2))
  
  # No goals generates an exception
  P0 = c()  
  expect_error(pareto(P0))
  
  P1 = G1   # Generates exception because P1 is not a matrix
  expect_error(pareto(G1))
  
  # How to create a one-objective problem
  P1 = matrix(G1, ncol = 1)
  rownames(P1) = A_names
  o1 = pareto(P1)
  expected_o1 = c(FALSE, TRUE, FALSE, FALSE, FALSE)
  names(expected_o1) = A_names
  expect_equal(o1, expected_o1)
  
})

test_that("pareto works when one obj has same value for all alternatives",{
  # reproduces a bug found when playing with SAS case study
  # the bug was in definition of dominates when using margin
  # incorrect: all (S1 > S2 + margin) && any(S1 > S2 + margin)
  # correct: all(S1 > S2) && any(S1 > S2 + margin)
  
  N = 100
  G1 = rep(1, 100)
  G2 = 1:100
  A = cbind(G1, G2)
  
  shortlist = pareto(A, mode = c("min", "max"))
  expect_true(shortlist[N])
  expect_false(any(shortlist[1:(N-1)]))
  
  shortlist = pareto(A, margin = c(0.01, 1.5))
  expect_true(all(shortlist[(N-1): N]))
  expect_false(any(shortlist[1:(N-2)]))
  
  shortlist = pareto(A, mode = c("min", "max"), margin = c(0.01, 1.5))
  expect_true(all(shortlist[(N-1): N]))
  expect_false(any(shortlist[1:(N-2)]))
 
})


test_that("dominates is correct",{

  S0 = c(1, 1)
  S1 = c(2, 1)
  S2 = c(1, 2)

  expect_true(dominates(S1, S0))
  expect_false(dominates(S0, S1))

  expect_false(dominates(S1, S2))
  expect_false(dominates(S2, S1))

  expect_false(dominates(S1, S1))

  expect_true(dominates(S2, S0))

})
