
#######################################################################################
# Single column benchmark of fst, feather and parguet for various column types        #
#######################################################################################

# required packages
library(syntheticbench)  # benchmark infrastructure
library(fst)
library(arrow)
library(dplyr)
library(ggplot2)

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
  variable_compression = FALSE
)

# fst streamer
fst_streamer <- table_streamer(
  id = "fst",
  table_writer = function(x, file_name, compress) {
    fst::write_fst(x, file_name, compress)
  },
  table_reader = function(x) read_fst(x, as.data.table = TRUE),
  can_select_threads = TRUE,
  variable_compression = TRUE
)

# parguet streamer
parguet_streamer <- table_streamer(
  id = "parguet",
  table_writer = function(x, file_name, compress) {
    arrow::write_parquet(x, file_name)
  },
  table_reader = function(x) read_parquet(x),
  can_select_threads = FALSE,
  variable_compression = FALSE
)

# feather streamer
feather_streamer <- table_streamer(
  id = "feather",
  table_writer = function(x, file_name, compress) {
    arrow::write_feather(x, file_name)
  },
  table_reader = function(x) read_feather(x),
  can_select_threads = FALSE,
  variable_compression = FALSE
)


# single integer column with 100 distinct values in the range 1 - 10000
integer_50 <- table_generator(
  id = "integer sparse",
  generator = function(nr_of_rows) {
    data.frame(
      Integers = sample_integer(nr_of_rows, 1, 10000, 50)
    )
  }
)

# single integer column with 100 distinct values in the range 1 - 10000
integer_10 <- table_generator(
  id = "integer sparse",
  generator = function(nr_of_rows) {
    data.frame(
      Integers = sample_integer(nr_of_rows, 1, 10000, 10)
    )
  }
)

# single integer column with 100 distinct values in the range 1 - 10000
integer_random <- table_generator(
  id = "integer sparse",
  generator = function(nr_of_rows) {
    data.frame(
      Integers = sample_integer(nr_of_rows)
    )
  }
)

# run benchmark for multiple streamers and compression settings
x <- synthetic_bench(integer_50, list(
  fst_streamer,
  parguet_streamer,
  feather_streamer
  ), 25e7, c(1, 25, 50, 75, 100), 1, 10)


# run benchmark for multiple streamers and compression settings
y <- synthetic_bench(integer_10, list(
  fst_streamer,
  parguet_streamer,
  feather_streamer
), 25e7, c(1, 25, 50, 75, 100), 1, 10)


# run benchmark for multiple streamers and compression settings
z <- synthetic_bench(integer_random, list(
  fst_streamer,
  parguet_streamer,
  feather_streamer
), 25e7, c(1, 25, 50, 75, 100), 1, 10)


# report results

x %>%
  mutate(Speed = OrigSize / Time) %>%
  group_by(ID, Mode, Compression) %>%
  summarise(Speed = median(Speed))

y %>%
  group_by(ID, Mode, Compression) %>%
  summarise(Time = 1e-9 * median(Time))

z %>%
  group_by(ID, Mode, Compression) %>%
  summarise(Time = 1e-9 * median(Time))
