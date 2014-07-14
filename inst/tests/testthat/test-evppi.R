context("Testing evppi")

# Sourcing evppi.seg.R (the orginal implementation)
tryCatch({
  source("evppi.seg.R")  
}, warning = function(warn){
  source("inst/tests/testthat/evppi.seg.R")
}, error = function(err){
  source("inst/tests/testthat/evppi.seg.R")
})  

test_that("our evppi is same as original on small cost-benefit model", {
  
  # Set up: cost-benefit analysis of two alternatives
  N = 10^3
  
  Cost = matrix(nrow = N, ncol = 2)
  Cost[ , 1] = rnorm(N, 3, 1)
  Cost[ , 2] = rnorm(N, 1, 0.1)
  
  Benefit = matrix(nrow = N, ncol = 2)
  Benefit[ , 1] = rnorm(N, 5, 2)
  Benefit[ , 2] = rnorm(N, 1, 0.2)
  
  NB = Benefit - Cost
  
  # Testing
  evppi.cost1 = evppi(Cost[ , 1], NB)
  evppi.cost2 = evppi(Cost[ , 2], NB) 
  evppi.benefit1 = evppi(Benefit[ , 1], NB)
  evppi.benefit2 = evppi(Benefit[ , 2], NB) 
  
  expected.evppi.cost1 = evppi.seg(Cost[ , 1], NB)$evppi
  expected.evppi.cost2 = evppi.seg(Cost[ , 2], NB)$evppi
  expected.evppi.benefit1 = evppi.seg(Benefit[ , 1], NB)$evppi
  expected.evppi.benefit2 = evppi.seg(Benefit[ , 2], NB)$evppi
  
  expect_equal(evppi.cost1, expected.evppi.cost1)
  expect_equal(evppi.cost2, expected.evppi.cost2)
  expect_equal(evppi.benefit1, expected.evppi.benefit1)
  expect_equal(evppi.benefit2, expected.evppi.benefit2) 
  
  
})

test_that("evtpi_and_risk works", {
  
  # Set up: cost-benefit analysis of two alternatives
  N = 10^3
  
  Cost = matrix(nrow = N, ncol = 2)
  Cost[ , 1] = rnorm(N, 3, 1)
  Cost[ , 2] = rnorm(N, 1, 0.1)
  
  Benefit = matrix(nrow = N, ncol = 2)
  Benefit[ , 1] = rnorm(N, 5, 2)
  Benefit[ , 2] = rnorm(N, 1, 0.2)
  
  NB = Benefit - Cost
  Loss = NB < 0
  
  # Testing same result as orginal implementation
  
  evppi.cost1 = evppi_and_risk(Cost[ , 1], NB, Loss)
  evppi.cost2 = evppi_and_risk(Cost[ , 2], NB, Loss) 
  evppi.benefit1 = evppi_and_risk(Benefit[ , 1], NB, Loss)
  evppi.benefit2 = evppi_and_risk(Benefit[ , 2], NB, Loss) 
  
#   print(evppi.cost1)
#   print(evppi.cost2)
#   print(evppi.benefit1)
#   print(evppi.benefit2)
  
  expected.evppi.cost1 = evppi.seg(Cost[ , 1], NB)$evppi
  expected.evppi.cost2 = evppi.seg(Cost[ , 2], NB)$evppi
  expected.evppi.benefit1 = evppi.seg(Benefit[ , 1], NB)$evppi
  expected.evppi.benefit2 = evppi.seg(Benefit[ , 2], NB)$evppi
  
  expect_equal(evppi.cost1[[1]], expected.evppi.cost1)
  expect_equal(evppi.cost2[[1]], expected.evppi.cost2)
  expect_equal(evppi.benefit1[[1]], expected.evppi.benefit1)
  expect_equal(evppi.benefit2[[1]], expected.evppi.benefit2) 

  
})
