
require(testthat)

context("sample logical")


test_that("basic sample logical", {
  x <- template_logical()

  expect_equal(names(x), c("metadata", "generator", "printer"))
  expect_equal(x$metadata, list(true_false_na_ratio = c(1, 1, 0)))

  y <- generate(x, 10)

  expect_equal(typeof(y), "logical")
  expect_equal(length(y), 10)
  expect_equal(sum(is.na(y)), 0)
})


test_that("probabilistic sample logical", {
  x <- template_logical(c(1, 0, 0))
  y <- generate(x, 10)
  expect_equal(y, rep(TRUE, 10))

  x <- template_logical(c(0, 10, 0))
  y <- generate(x, 10)
  expect_equal(y, rep(FALSE, 10))

  x <- template_logical(c(0, 0, 20))
  y <- generate(x, 10)
  expect_equal(y, rep(as.logical(NA), 10))

  x <- template_logical(c(20, 10, 0))
  y <- generate(x, 10)
  expect_equal(sum(is.na(y)), 0)
})
