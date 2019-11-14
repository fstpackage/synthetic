
require(testthat)

context("sample raw")


test_that("sample raw", {
  y <- template_raw()
  x <- generate(y, 10)

  expect_equal(length(x), 10)
  expect_type(x, "raw")

  # set min
  y <- template_raw(200)
  x <- generate(y, 10)

  expect_true(sum(as.integer(x) < 200) == 0)

  # set max
  y <- template_raw(max_value = 200)
  x <- generate(y, 10)

  expect_true(sum(as.integer(x) > 200) == 0)

  # set min and max
  y <- template_raw(130, 200)
  x <- generate(y, 10)

  expect_true(sum(as.integer(x) > 200) == 0)
  expect_true(sum(as.integer(x) < 130) == 0)

  # set distinct values
  y <- template_raw(max_distict_values = 2)
  x <- generate(y, 10)
  expect_true(length(unique(x)) <= 2)
})
