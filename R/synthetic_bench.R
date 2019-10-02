#  syntheticbench - R package for benchmarking of dataset serialization
#
#  Copyright (C) 2019-present, Mark AJ Klik
#
#  This file is part of the lazyvec R package.
#
#  The lazyvec R package is free software: you can redistribute it and/or modify it
#  under the terms of the GNU Affero General Public License version 3 as
#  published by the Free Software Foundation.
#
#  The lazyvec R package is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
#  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
#  for more details.
#
#  You should have received a copy of the GNU Affero General Public License along
#  with the lazyvec R package. If not, see <http://www.gnu.org/licenses/>.
#
#  You can contact the author at:
#  - syntheticbench R package source repository : https://github.com/fstpackage/syntheticbench


# Add a single observation to benchmark
observation <- function(bench, mode, format_id, data_id, compression, size, time, nr_of_rows, orig_size) {
  rbindlist(list(bench, data.table(
    Mode = mode,
    ID = format_id,
    DataID = data_id,
    Compression = compression,
    Size = size,
    Time = time,
    NrOfRows = nr_of_rows,
    OrigSize = orig_size)))
}


#' Run serialization benchmarks
#'
#' @param generator function f(nr_of_rows) that generates the data.frame
#' @param table_streamers a single tablestreamer object generated with table_streamer().
#' Could also be a list of tablestreamer objects to benchmark various streamers.
#' @param nr_of_rows vector of number of rows values to use in the benchmark
#' @param nr_of_runs repeat the benchmark for statistics
#' @param cycle_size create cycly_size files before overwriting
#' @param compression vector of compression values to use for benchmarking
#' @param result_folder folder to use for temporal storage of results
#' @param bench_id 
#'
#' @return benchmarks results
#' @export
synthetic_bench <- function(generator, table_streamers, nr_of_rows,
  nr_of_runs = 10, cycle_size = 10, compression = NULL, result_folder = "results", progress = TRUE) {

  # verify table streamers
  if (class(table_streamers) == "tablestreamer") {
    table_streamers <- list(table_streamers)
  } else {
    if (!is.list(table_streamers)) stop("Expected a single tablestreamer object or a list",
    " of tablestreamer objects")

    lapply(table_streamers, function(x) {
      if (class(x) != "tablestreamer") stop("One or more of the tablestreamer objects was not",
      " of the correct class")
    })
  }

  results <- NULL

  # define progress bar
  if (progress) {

    compression_steps <- length(table_streamers)

    if (!is.null(compression)) {
      compressors <- 0
      for (table_streamer in table_streamers) {
        if (table_streamer$variable_compression) compressors <- compressors + 1
      }

      compression_steps <- compressors * length(compression) + length(table_streamers) - compressors
    }

    nr_of_measurements <- 2 * compression_steps * nr_of_runs * cycle_size * length(nr_of_rows)
    row_weights <- length(nr_of_rows) * nr_of_rows / sum(nr_of_rows)
    measurement_count <- 0

    pb <- progress_bar$new("[:bar] :percent :eta", total = 100)
  }

  # create a length 1 vector
  if (is.null(compression)) {
    compression <- -1
  }

  for (nr_of_rows_index in 1:length(nr_of_rows)) {

    cur_nr_of_rows <- nr_of_rows[nr_of_rows_index]

    for (compress_count in 1:length(compression)) {

      write_compression <- compression[compress_count]
      if (write_compression == -1) write_compression <- NULL

      for (run_id in 1:nr_of_runs) {

        # write cycle_size files
        for (id in 1:cycle_size) {

          # generate dataset once for all generators
          x <- generator$generator(cur_nr_of_rows)

          # disk warmup (to avoid a sleeping disk after data creation)
          saveRDS("warmup disk", paste0(result_folder, "/", "warmup.rds"))

          # iterate
          for (table_streamer in table_streamers[sample(1:length(table_streamers))]) {

            # don't repeat identical measurements
            if (!table_streamer$variable_compression && compress_count > 1) next

            file_name <- paste0(result_folder, "/", "dataset_", table_streamer$id, "_", id)

            # Only a single iteration is used to avoid disk caching effects
            # Due to caching measured speeds are higher and create a unrealistic benchmark
            res <- microbenchmark({
              table_streamer$table_writer(x, file_name, write_compression)
            },
            times = 1)

            results <- observation(results, "write", table_streamer$id, generator$id,
              write_compression, file.info(file_name)$size, res$time, cur_nr_of_rows, object.size(x))

            if (progress) {
              measurement_count <- measurement_count + row_weights[nr_of_rows_index]
              pb$update(measurement_count / nr_of_measurements)
            }
          }
        }

        # read from disk
        for (id in 1:cycle_size) {

          # iterate
          for (table_streamer in table_streamers[sample(1:length(table_streamers))]) {

            # don't repeat identical measurements
            if (!table_streamer$variable_compression && compress_count > 1) next

            file_name <- paste0(result_folder, "/", "dataset_", table_streamer$id, "_", id)

            res <- microbenchmark({
                y <- table_streamer$table_reader(file_name)
              },
              times = 1)

            results <- observation(results, "read", table_streamer$id, generator$id,
              write_compression, file.info(file_name)$size, res$time, cur_nr_of_rows, object.size(y))

            if (progress) {
              measurement_count <- measurement_count + row_weights[nr_of_rows_index]
              pb$update(measurement_count / nr_of_measurements)
            }
          }
        }
      }
    }
  }

  results
}
