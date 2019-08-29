
require(testthat)
require(fst)
require(arrow)


context("synthetic bench")


# defines streamers for fst, feather, parguet and rds
source("streamers.R")


# test table with sparse integers
generator <- table_generator(
  "integer sparse",
  function(nr_of_rows) {
  data.frame(
    Integers = sample_integer(nr_of_rows, 1, nr_of_rows)
  )
})

test_that("benchmark single streamer", {

  # single streamer
  x <- synthetic_bench(generator, list(
    fst_streamer
  ), 10, 1, 1, 1)

  expect_equal(x$ID, c("fst", "fst"))
  expect_equal(x$Mode, c("write", "read"))
})


test_that("benchmark multiple streamers", {

  x <- synthetic_bench(generator, list(
    rds_streamer,
    fst_streamer,
    parguet_streamer,
    feather_streamer
    ), 100, 1, 2, 5)

  expect_equal(sort(unique(x$ID)), c("feather", "fst", "parguet", "rds"))
  expect_equal(x$Mode, c(rep("write", 20), rep("read", 20), rep("write", 20), rep("read", 20)))
})
