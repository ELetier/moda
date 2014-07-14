context("Testing evtpi.R")

test_that("evtpi works",{
  # A simple MC simuation of the value of the two alternatives
  N = 10^4
  Value = matrix(ncol = 2, nrow = N)
  Value[ , 1] = rnorm(N, mean = 100, sd = 50)
  Value[ , 2] = rnorm(N, mean = 80, sd = 10)
  
  evtpi = evtpi(Value)
  
  expect_true(11 <= evtpi && evtpi <= 13)
  
})

test_that("evtpi_and_risk works", {
  # A simple MC simuation of the value of the two alternatives
  N = 10^4
  Value = matrix(ncol = 2, nrow = N)
  Value[ , 1] = rnorm(N, mean = 100, sd = 50)
  Value[ , 2] = rnorm(N, mean = 80, sd = 10)
  
  Failure = (Value < 0)
  
  x = evtpi_and_risk(Value, Failure)
  evtpi = x['evtpi']
  delta_risk = x['delta_risk']
  
  expect_true(11 <= evtpi && evtpi <= 13)
  expect_true(-0.03 <= delta_risk && delta_risk < -0.02)
  
})


