
require(testthat)
require(fst)
require(arrow)

context("synthetic bench")


# streamers

# rds streamer
rds_streamer <- table_streamer(
  id = "rds",
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
  id = "fst",
  table_writer = function(x, file_name, compress) {
    fst::write_fst(x, file_name, compress)
  },
  table_reader = function(x) read_fst(x),
  can_select_threads = TRUE,
  can_select_compression = TRUE
)

# parguet streamer
parguet_streamer <- table_streamer(
  id = "parguet",
  table_writer = function(x, file_name, compress) {
    arrow::write_parquet(x, file_name)
  },
  table_reader = function(x) read_parquet(x),
  can_select_threads = FALSE,
  can_select_compression = FALSE
)

# feather streamer
feather_streamer <- table_streamer(
  id = "feather",
  table_writer = function(x, file_name, compress) {
    arrow::write_feather(x, file_name)
  },
  table_reader = function(x) read_feather(x),
  can_select_threads = FALSE,
  can_select_compression = FALSE
)

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
