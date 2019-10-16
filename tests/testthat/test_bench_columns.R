
require(testthat)
require(fst)
require(arrow)
require(dplyr)


context("bench columns")


# defines several generators
source("generators.R")


test_that("column mode", {

  x <- synthetic_bench(progress = FALSE)

  x %>%
    bench_columns("all")
})
