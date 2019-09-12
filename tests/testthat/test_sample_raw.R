
require(testthat)

context("sample raw")


test_that("sample raw", {
  x <- sample_raw(10)

  expect_equal(length(x), 10)
  expect_type(x, "raw")

  # set min
  x <- sample_raw(10, min_value = 250)
  expect_true(sum(as.integer(x) < 250) == 0)

  # set max
  x <- sample_raw(10, max_value = 2)
  expect_true(sum(as.integer(x) > 2) == 0)

  # set min and max
  x <- sample_raw(10, 10, 12)
  expect_true(sum(as.integer(x) < 10) == 0)
  expect_true(sum(as.integer(x) > 12) == 0)

  # set distinct values
  x <- sample_raw(10, max_distict_values = 2)
  expect_true(length(unique(x)) <= 2)
})
