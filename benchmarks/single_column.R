
#######################################################################################
# Single column benchmark of fst, feather and parguet for various column types        #
#######################################################################################

# required packages
library(syntheticbench)  # benchmark infrastructure
library(fst)
library(arrow)


# define streamers

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


# single integer column with 100 distinct values in the range 1 - 10000
integer_100 <- function(nr_of_rows) {
  data.frame(
    Integers = sample_integer(nr_of_rows, 1, 10000, 100)
  )
}

x <- synthetic_bench(integer_100, list(
  rds_streamer,
  fst_streamer,
  parguet_streamer,
  feather_streamer
  ), 1e8, 100, 1, 10)


library(dplyr)

x %>%
  group_by(ID, Mode) %>%
  summarise(Time = 1e-9 * median(Time))


