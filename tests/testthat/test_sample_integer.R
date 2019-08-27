
require(testthat)

context("sample integer")


test_that("sample integer", {
  x <- sample_integer(10)

  expect_equal(length(x), 10)
})
