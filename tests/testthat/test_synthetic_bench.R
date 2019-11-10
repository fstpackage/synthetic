
require(testthat)
require(fst)
require(arrow)
require(dplyr)


context("synth bench class")


# defines several table definitions
source("table_definitions.R")


test_that("constructor", {

  x <- synthetic_bench()
  expect_equal(names(x), c("nr_of_runs", "column_mode", "cycle_size", "result_folder", "progress"))
})


test_that("generators", {

  x <- synthetic_bench(1, 1)

  # check class
  expect_error(x %>% bench_tables("incorrect class"), "Incorrectly defined synthetic tables")

  # single
  x <- x %>% bench_tables(sparse_ints)
  expect_equal(names(x), c("nr_of_runs", "column_mode", "cycle_size", "result_folder", "progress", "generators"))

  # single streamer
  x <- x %>%
    bench_streamers(streamer_fst()) %>%
    bench_rows(10)

  res <- x %>%
    collect()

  expect_equal(res$ID, c("fst", "fst"))
  expect_equal(res$Mode, c("write", "read"))

  # multiple
  res <- x %>%
    bench_tables(random_ints, sparse_ints) %>%
    collect()

  expect_equal(names(x), c("nr_of_runs", "column_mode", "cycle_size", "result_folder", "progress", "generators",
    "streamers", "nr_of_rows"))
  expect_equal(res$ID, rep("fst", 4))
  expect_equal(res$Mode, c("write", "write", "read", "read"))
})


test_that("streamers", {

  x <- synthetic_bench(1, 1) %>%
    bench_tables(sparse_ints) %>%
    bench_rows(10)

  # check class
  expect_error(x %>% bench_streamers("incorrect class"), "Incorrectly defined streamer")

  # multiple
  x <- x %>%
    bench_streamers(streamer_fst(), streamer_arrow(), streamer_parguet(), streamer_feather())

  res <- x %>%
    collect()

  expect_equal(names(x), c("nr_of_runs", "column_mode", "cycle_size", "result_folder", "progress", "generators",
    "nr_of_rows", "streamers"))
  expect_equal(sort(unique(res$ID)), c("arrow", "feather", "fst", "parguet"))
  expect_equal(res$Mode, c(rep("write", 4), rep("read", 4)))
})


test_that("compression", {

  x <- synthetic_bench(1, 1) %>%
    bench_tables(sparse_ints)

  # compression error
  expect_error(x %>% bench_compression("incorrect type"), "Incorrectly defined compression")

  # single value
  x <- x %>% bench_compression(1)
  expect_equal(x$compression, 1)

  # vector
  x <- x %>% bench_compression(1:5)
  expect_equal(x$compression, 1:5)

  # multiple vectors
  x <- x %>% bench_compression(1:5, 7)
  expect_equal(x$compression, c(1:5, 7))

  # duplicate elements
  x <- x %>% bench_compression(5:1, 7, 3)
  expect_equal(x$compression, c(5:1, 7))

  x <- x %>% bench_compression(1, 50)
  expect_equal(names(x), c("nr_of_runs", "column_mode", "cycle_size", "result_folder",
    "progress", "generators", "compression"))

  res <- x %>%
    bench_streamers(streamer_fst()) %>%
    bench_rows(10) %>%
    collect()

  expect_equal(res$ID, rep("fst", 4))
  expect_equal(res$Mode, c("write", "read", "write", "read"))
  expect_equal(res$Compression, c(1, 1, 50, 50))
})


test_that("threads", {

  x <- synthetic_bench(1, 1) %>%
    bench_tables(sparse_ints)

  # number of threads
  x <- x %>% bench_threads(5:1, 7, 3)
  expect_equal(x$threads, c(5:1, 7))
  expect_error(x %>% bench_threads("incorrect type"), "Incorrectly defined number of threads")
})


test_that("rows", {

  x <- synthetic_bench(1, 1) %>%
    bench_tables(sparse_ints)

  # number of rows
  x <- x %>% bench_rows(5:1, 7, 3)
  expect_equal(x$nr_of_rows, c(5:1, 7))
  expect_error(x %>% bench_rows("incorrect type"), "Incorrectly defined number of rows")
})


test_that("collect", {

  x <- synthetic_bench(1, 1) %>%
    bench_tables(random_ints, sparse_ints) %>%
    bench_streamers(streamer_fst(), streamer_arrow()) %>%
    bench_compression(1, 50) %>%
    bench_rows(10, 20) %>%
    bench_threads(2, 4, 6) %>%
    collect()

  expect_equal(colnames(x),
    c("Mode", "ID", "DataID", "Compression", "Size", "Time", "NrOfRows", "OrigSize", "SpeedMBs"))
})
