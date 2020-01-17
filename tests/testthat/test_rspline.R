
require(testthat)

context("random splines")


test_that("generate random spline based vector", {

  control_values <- c(0, 0.1, 0.4, 0.6, 0.7, 0.9, 2)
  x <- rspline(100, control_values, 0.1)

  expect_equal(length(x), 100)
  
  expect_true(min(x) >= 0)
})


test_that("generate non-random spline based vector", {
  
  control_values <- c(0, 0.1, 0.4, 0.6, 0.7, 0.9, 2)
  x <- synthetic::sspline(control_values, 0:6 / 6)

  expect_equal(length(x), 7)

  expect_lt(max((control_values - x) * (control_values - x)), 0.01)
})
