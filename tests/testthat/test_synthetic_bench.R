
require(testthat)
require(fst)
require(arrow)


context("synthetic bench")


# defines streamers for fst, feather, parguet and rds
source("streamers.R")


# test table with sparse integers
sparse_generator <- table_generator(
  "integer sparse",
  function(nr_of_rows) {
  data.frame(
    Integers = sample_integer(nr_of_rows, 1, 10)
    )
  }
)

# test table with sparse integers
random_generator <- table_generator(
  "integer random",
  function(nr_of_rows) {
    data.frame(
      Integers = sample_integer(nr_of_rows, 1, nr_of_rows)
    )
  }
)

test_that("benchmark single streamer", {

  # single streamer
  x <- synthetic_bench(sparse_generator, list(
    fst_streamer
  ), 10, 1, 1, 1)

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
    ), 100, 1, 2, 5)

  expect_equal(sort(unique(x$ID)), c("feather", "fst", "parguet", "rds"))
  expect_equal(x$Mode, c(rep("write", 20), rep("read", 20), rep("write", 20), rep("read", 20)))
})


test_that("benchmark multiple datasets", {

  expect_error(
    synthetic_bench("incorrect argument", fst_streamer, 100, 1, 2, 5),
    "generators should be a single generator or a list of generators"
  )

  x <- synthetic_bench(list(
    sparse_generator,
    random_generator),
    fst_streamer, 100, 1, 2, 5)

  expect_equal(unique(x$ID), "fst")
  expect_equal(x$Mode, rep(c(rep("write", 10), rep("read", 10)), 2))
})


test_that("multiple datasets and multiple streamers", {

  x <- synthetic_bench(list(
      sparse_generator,
      random_generator),
    list(
      rds_streamer,
      fst_streamer
    ), 100, 1, 2, 5)

  expect_equal(sort(unique(x$ID)), c("fst", "rds"))
  expect_equal(x$Mode, rep(c(rep("write", 20), rep("read", 20)), 2))
})
