
require(testthat)

context("synthetic bench")


table_writer <- function(x, file_name, compress) {
  if (compress == 0) {
    saveRDS(x, file_name, compress = FALSE)
    return()
  }

  saveRDS(x, file_name)
}

table_reader <- function(x) {
  readRDS(x)
}


table_generator <- function(nr_of_rows) {
  data.frame(
    Integers = sample_integer(nr_of_rows, 1, nr_of_rows)
  )
}


test_that("rds benchmark", {
  x <- synthetic_bench(table_generator, table_writer, table_reader, 100, 1, 2, 10)

  expect_equal(nrow(x), 40)
  expect_equal(x$Mode, c(rep("write", 10), rep("read", 10), rep("write", 10), rep("read", 10)))
})
