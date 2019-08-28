
#######################################################################################
# Single column benchmark of fst, feather and parguet for various column types        #
#######################################################################################

# required packages
library(syntheticbench)  # benchmark infrastructure
library(fst)
library(arrow)


# define stream writers

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


# define stream readers

rds_table_reader <- function(x) readRDS(x)  # rds
fst_table_reader <- function(x) read_fst(x)  # fst
parguet_table_reader <- function(x) read_parquet(x)  # parguet
feather_table_reader <- function(x) read_feather(x)  # feather



# single integer column with 100 distinct values in the range 1 - 10000
integer_100 <- function(nr_of_rows) {
  data.frame(
    Integers = sample_integer(nr_of_rows, 1, 10000, 100)
  )
}


x <- synthetic_bench("fst", integer_100, fst_table_writer, fst_table_reader, 1e8, 90, 1, 10, "res_fst")
y <- synthetic_bench("parguet", integer_100, parguet_table_writer, parguet_table_reader, 1e8, 50, 1, 10)


library(dplyr)

x %>%
  group_by(Mode) %>%
  summarise(Time = 1e-9 * median(Time))


