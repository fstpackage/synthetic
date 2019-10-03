
require(testthat)

context("sample numerical")


test_that("sample numerical uniform", {
  x <- sample_numerical_uniform(10)
  expect_type(x, "double")
  expect_equal(length(x), 10)
  expect_equal(sum(x < 0), 0)
  expect_equal(sum(x > 1), 0)

  x <- sample_numerical_uniform(10, 2, 10)
  expect_equal(length(x), 10)
  expect_equal(sum(x < 2), 0)
  expect_equal(sum(x > 10), 0)

  x <- sample_numerical_uniform(10, 2, 10, 2)
  expect_equal(length(x), 10)
  expect_lte(length(unique(x)), 2)
})


test_that("sample numerical normal", {
  x <- sample_numerical_normal(10)
  expect_type(x, "double")
  expect_equal(length(x), 10)

  x <- sample_numerical_normal(10, max_distict_values = 2)
  expect_lte(length(unique(x)), 2)
})
