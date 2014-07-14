context("Testing sumproduct")

test_that("sumproduct works",{
  
  # 2 vectors
  w = 1:5
  a = 1:5
  
  expect_equal(sumproduct(w, a), sum(w * a))
  
  # 1 vector, 1 matrix
  A = matrix(1:50, ncol = 5)
  
  expect_equal(sumproduct(w, A)[1], sum(w * A[1, ]))
  expect_equal(sumproduct(w, A)[nrow(A)], sum(w * A[nrow(A), ]))
  
  expect_equal(sumproduct(A, w)[1], sum(w * A[1, ]))
  expect_equal(sumproduct(A, w)[nrow(A)], sum(w * A[nrow(A), ]))
  
  # 2 matrices
  B = matrix(50:1, ncol = 5)
    
  expect_equal(sumproduct(A, B)[1], sum(A[1, ] * B[1, ]))
  expect_equal(sumproduct(A, B)[nrow(A)], sum(A[nrow(A), ] * B[nrow(A), ]))
  
  
})


