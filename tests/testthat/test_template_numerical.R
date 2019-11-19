
require(testthat)

context("sample numerical")


test_that("sample numerical uniform", {
  y <- template_numerical_uniform()
  x <- generate(y, 10)

  expect_type(x, "double")
  expect_equal(length(x), 10)
  expect_equal(sum(x < 0), 0)
  expect_equal(sum(x > 1), 0)

  y <- template_numerical_uniform(2, 10)
  x <- generate(y, 10)

  expect_equal(length(x), 10)
  expect_equal(sum(x < 2), 0)
  expect_equal(sum(x > 10), 0)

  y <- template_numerical_uniform(2, 10, 2)
  x <- generate(y, 10)

  expect_equal(length(x), 10)
  expect_lte(length(unique(x)), 2)
})


test_that("sample numerical normal", {
  y <- template_numerical_normal()
  x <- generate(y, 10)

  expect_type(x, "double")
  expect_equal(length(x), 10)

  y <- template_numerical_normal(max_distict_values = 2)
  x <- generate(y, 10)

  expect_lte(length(unique(x)), 2)
})
