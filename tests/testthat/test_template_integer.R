
require(testthat)

context("sample integer")


test_that("sample integer", {
  x <- template_integer(4, 10)
  y <- generate(x, 20)

  expect_gte(min(y), 4)
  expect_lte(min(y), 10)
})
