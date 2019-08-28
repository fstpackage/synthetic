
require(testthat)
require(fst)
require(arrow)

context("synthetic bench")


# streamers

# rds streamer
rds_streamer <- table_streamer(
  table_writer = function(x, file_name, compress) {
    if (compress == 0) {
      saveRDS(x, file_name, compress = FALSE)
      return()
    }

    saveRDS(x, file_name)
  },
  table_reader = function(x) readRDS(x),
  can_select_threads = FALSE,
  can_select_compression = FALSE
)

# fst streamer
fst_streamer <- table_streamer(
  table_writer = function(x, file_name, compress) {
    fst::write_fst(x, file_name, compress)
  },
  table_reader = function(x) read_fst(x),
  can_select_threads = TRUE,
  can_select_compression = TRUE
)

# parguet streamer
parguet_streamer <- table_streamer(
  table_writer = function(x, file_name, compress) {
    arrow::write_parquet(x, file_name)
  },
  table_reader = function(x) read_parquet(x),
  can_select_threads = FALSE,
  can_select_compression = FALSE
)

# feather streamer
feather_streamer <- table_streamer(
  table_writer = function(x, file_name, compress) {
    arrow::write_feather(x, file_name)
  },
  table_reader = function(x) read_feather(x),
  can_select_threads = FALSE,
  can_select_compression = FALSE
)

table_generator <- function(nr_of_rows) {
  data.frame(
    Integers = sample_integer(nr_of_rows, 1, nr_of_rows)
  )
}


test_that("rds benchmark", {
  x <- synthetic_bench("rds", table_generator, rds_streamer, 100, 1, 2, 10)

  expect_equal(x$ID[1], "rds")
  expect_equal(nrow(x), 40)
  expect_equal(x$Mode, c(rep("write", 10), rep("read", 10), rep("write", 10), rep("read", 10)))
})


test_that("fst benchmark", {
  x <- synthetic_bench("fst", table_generator, fst_streamer, 100, 1, 2, 10)

  expect_equal(x$ID[1], "fst")
  expect_equal(nrow(x), 40)
  expect_equal(x$Mode, c(rep("write", 10), rep("read", 10), rep("write", 10), rep("read", 10)))
})


test_that("parguet benchmark", {
  x <- synthetic_bench("parguet", table_generator, parguet_streamer, 100, 1, 2, 10)

  expect_equal(x$ID[1], "parguet")
  expect_equal(nrow(x), 40)
  expect_equal(x$Mode, c(rep("write", 10), rep("read", 10), rep("write", 10), rep("read", 10)))
})


test_that("feather benchmark", {
  x <- synthetic_bench("feather", table_generator, feather_streamer, 100, 1, 2, 10)

  expect_equal(x$ID[1], "feather")
  expect_equal(nrow(x), 40)
  expect_equal(x$Mode, c(rep("write", 10), rep("read", 10), rep("write", 10), rep("read", 10)))
})
