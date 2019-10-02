
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
  x <- synthetic_bench(generator, fst_streamer, 10, 1, 1, 1)

  expect_equal(x$ID, c("fst", "fst"))
  expect_equal(x$Mode, c("write", "read"))
})


test_that("benchmark multiple streamers", {

  x <- synthetic_bench(generator, list(
    rds_streamer,
    fst_streamer,
    parguet_streamer,
    feather_streamer
    ), 10, 2, 2, 1)

  expect_equal(sort(unique(x$ID)), c("feather", "fst", "parguet", "rds"))
  expect_equal(x$Mode, c(rep("write", 8), rep("read", 8), rep("write", 8), rep("read", 8)))
})


test_that("benchmark multiple nr_of_rows", {

  # multiple sizes
  x <- synthetic_bench(generator, fst_streamer, c(10, 20), 2, 2, 1)

  expect_equal(unique(x$ID), "fst")
  expect_equal(x$Mode, rep(c(rep("write", 2), rep("read", 2)), 4))
})


test_that("benchmark compression", {

  # empty compression argument
  x <- synthetic_bench(generator, fst_streamer, 10, 2, 2, c(1, 50))

  # multiple compression
  x <- synthetic_bench(generator, fst_streamer, 10, 2, 2, c(1, 50))

  expect_equal(unique(x$ID), "fst")
  expect_equal(x$Mode, rep(c(rep("write", 2), rep("read", 2)), 4))
})


test_that("benchmark multiple compression, nr_of_rows and streamers", {

  # multiple everything
  x <- synthetic_bench(
    generator,  # single generator
    list(
      rds_streamer,
      fst_streamer,
      parguet_streamer,
      feather_streamer),  # multiple streamers
    c(10, 20),  # multiple nr_of_rows
    2,
    2,
    c(1, 50)  # multiple compression
  )

  expect_equal(sort(unique(x$ID)), c("feather", "fst", "parguet", "rds"))
  expect_equal(sum(x$Mode == "write"), 40)
  expect_equal(sum(x$Mode == "read"), 40)
})
