test_that("errors are thrown when they should be", {

  x <- rnorm(100)
  y <- rnorm(100, 2)

  expect_error(mi_confidence(x, y, alpha = 1))
  expect_error(tr_confidence(x, y, alpha = 0))

})
