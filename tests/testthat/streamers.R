
require(fst)
require(arrow)

# define streamers used in tests

# baseR streamer
rds_streamer <- table_streamer(
  id = "rds",
  table_writer = function(x, file_name, compress) {
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
    if (is.null(compress)) {
      return(fst::write_fst(x, file_name))
    }
    fst::write_fst(x, file_name, compress)
  },
  table_reader = function(x) read_fst(x),
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
