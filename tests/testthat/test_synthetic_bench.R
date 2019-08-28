
require(testthat)
require(fst)
require(arrow)

context("synthetic bench")


# writers

# rds writer
rds_table_writer <- function(x, file_name, compress) {
  if (compress == 0) {
    saveRDS(x, file_name, compress = FALSE)
    return()
  }

  saveRDS(x, file_name)
}

# fst writer
fst_table_writer <- function(x, file_name, compress) {
  fst::write_fst(x, file_name, compress)
}

# parguet writer
parguet_table_writer <- function(x, file_name, compress) {
  arrow::write_parquet(x, file_name)
}

# feather writer
feather_table_writer <- function(x, file_name, compress) {
  arrow::write_feather(x, file_name)
}

# readers

rds_table_reader <- function(x) readRDS(x)  # rds
fst_table_reader <- function(x) read_fst(x)  # fst
parguet_table_reader <- function(x) read_parquet(x)  # parguet
feather_table_reader <- function(x) read_feather(x)  # parguet


table_generator <- function(nr_of_rows) {
  data.frame(
    Integers = sample_integer(nr_of_rows, 1, nr_of_rows)
  )
}


test_that("rds benchmark", {
  x <- synthetic_bench("rds", table_generator, rds_table_writer, rds_table_reader, 100, 1, 2, 10)

  expect_equal(x$ID[1], "rds")
  expect_equal(nrow(x), 40)
  expect_equal(x$Mode, c(rep("write", 10), rep("read", 10), rep("write", 10), rep("read", 10)))
})


test_that("fst benchmark", {
  x <- synthetic_bench("fst", table_generator, fst_table_writer, fst_table_reader, 100, 1, 2, 10)

  expect_equal(x$ID[1], "fst")
  expect_equal(nrow(x), 40)
  expect_equal(x$Mode, c(rep("write", 10), rep("read", 10), rep("write", 10), rep("read", 10)))
})


test_that("parguet benchmark", {
  x <- synthetic_bench("parguet", table_generator, parguet_table_writer, parguet_table_reader, 100, 1, 2, 10)

  expect_equal(x$ID[1], "parguet")
  expect_equal(nrow(x), 40)
  expect_equal(x$Mode, c(rep("write", 10), rep("read", 10), rep("write", 10), rep("read", 10)))
})


test_that("feather benchmark", {
  x <- synthetic_bench("feather", table_generator, feather_table_writer, feather_table_reader, 100, 1, 2, 10)

  expect_equal(x$ID[1], "feather")
  expect_equal(nrow(x), 40)
  expect_equal(x$Mode, c(rep("write", 10), rep("read", 10), rep("write", 10), rep("read", 10)))
})
