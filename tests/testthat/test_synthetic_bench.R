
require(testthat)
require(fst)
require(arrow)


context("synthetic bench")


# defines streamers for fst, feather, parguet and rds
source("streamers.R")

# defines several generators
source("generators.R")


test_that("benchmark single streamer", {

  # single streamer
  x <- synthetic_bench(sparse_generator, fst_streamer, 10, 1, 1, 1)

  expect_equal(x$ID, c("fst", "fst"))
  expect_equal(x$Mode, c("write", "read"))
})


test_that("benchmark multiple streamers", {

  expect_error(
    synthetic_bench(sparse_generator, "incorrect streamer", 100, 1, 2, 5),
    "Expected a single tablestreamer object"
  )

  x <- synthetic_bench(sparse_generator, list(
    rds_streamer,
    fst_streamer,
    parguet_streamer,
    feather_streamer
    ), 10, 2, 2, 1)

  expect_equal(sort(unique(x$ID)), c("feather", "fst", "parguet", "rds"))
  expect_equal(x$Mode, c(rep("write", 8), rep("read", 8), rep("write", 8), rep("read", 8)))
})


test_that("benchmark multiple datasets", {

  expect_error(
    synthetic_bench("incorrect argument", fst_streamer, 10, 1, 2, 5),
    "generators should be a single generator or a list of generators"
  )

  x <- synthetic_bench(list(
    sparse_generator,
    random_generator),
    fst_streamer, 10, 2, 2, 1)

  expect_equal(unique(x$ID), "fst")
  expect_equal(x$Mode, rep(c(rep("write", 4), rep("read", 4)), 2))
})


test_that("benchmark multiple nr_of_rows", {

  # multiple sizes
  x <- synthetic_bench(sparse_generator, fst_streamer, c(10, 20), 2, 2, 1)

  expect_equal(unique(x$ID), "fst")
  expect_equal(x$Mode, rep(c(rep("write", 2), rep("read", 2)), 4))
})


test_that("benchmark multiple compression", {

  # empty compression argument
  x <- synthetic_bench(sparse_generator, fst_streamer, 10, 2, 2, c(1, 50))

  # multiple compression
  x <- synthetic_bench(sparse_generator, fst_streamer, 10, 2, 2, c(1, 50))

  expect_equal(unique(x$ID), "fst")
  expect_equal(x$Mode, rep(c(rep("write", 2), rep("read", 2)), 4))
})


test_that("multiple datasets, streamers, compression and number of rows", {

  x <- synthetic_bench(list(
      sparse_generator,
      random_generator
    ),
    list(
      rds_streamer,
      fst_streamer
    ),
    c(10, 20),  # number of rows
    1,  # number of runs
    2,  # cycle size
    c(1, 50)  # multiple compression
  )

  expect_equal(sort(unique(x$ID)), c("fst", "rds"))
  expect_equal(sum(x$Mode == "write"), 24)
})
