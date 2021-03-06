
require(testthat)
require(fst)
require(arrow)
require(dplyr)


context("streamers")


# defines several table definitions
source("table_definitions.R")


test_that("test all streamers", {

  x <- synthetic_bench(1, 1) %>%
    bench_tables(random_ints) %>%
    bench_streamers(streamer_fst(), streamer_arrow(), streamer_feather(), streamer_parguet(), streamer_rds()) %>%
    bench_rows(10) %>%
    collect()

  expect_equal(colnames(x), c("Mode", "ID", "DataID", "Compression", "Size", "Time", "NrOfRows",
    "OrigSize", "SpeedMBs"))
})
