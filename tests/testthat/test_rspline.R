
require(testthat)

context("random splines")


test_that("generate random vector", {

  x <- rspline(100, c(0, 0.1, 0.4, 0.6, 0.7), 0.1)

  expect_equal(length(x), 100)
})
