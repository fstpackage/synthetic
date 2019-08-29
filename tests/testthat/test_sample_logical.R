
require(testthat)

context("sample logical")


test_that("basic sample logical", {
  x <- sample_logical(10)

  expect_equal(typeof(x), "logical")
  expect_equal(length(x), 10)
})


test_that("probabilistic sample logical", {
  x <- sample_logical(10)
  expect_equal(typeof(x), "logical")
  expect_equal(length(x), 10)

  x <- sample_logical(10, c(1, 0, 0))
  expect_equal(x, rep(TRUE, 10))

  x <- sample_logical(10, c(1, 0, 1))
  expect_true(sum(x == FALSE, na.rm = TRUE) == 0)
})
