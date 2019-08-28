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
observation <- function(bench, mode, id, compression, size, time, orig_size) {
  rbindlist(list(bench, data.table(
    Mode = mode,
    ID = id,
    Compression = compression,
    Size = size,
    Time = time,
    OrigSize = orig_size)))
}


#' Runs benchmarks
#'
#' @param id benchmark id (e.g. 'fst', 'rds', 'arrow')
#' @param table_streamer object of class tablestreamer that defines the streaming
#' methods, generated with table_streamer().
#' @param table_reader function f(file_name) that reads a file into a data.frame
#' @param nr_of_runs repeat the benchmark for statistics
#' @param cycle_size create cycly_size files before overwriting
#' @param table_generator function f(nr_of_rows) that generates the data.frame
#' @param nr_of_rows number of rows to use in the benchmark
#' @param compression vector of compression values to use for benchmarking
#' @param result_folder folder to use for temporal storage of results
#'
#' @return benchmarks results
#' @export
synthetic_bench <- function(bench_id, table_generator, table_streamer, nr_of_rows,
  compression, nr_of_runs = 100, cycle_size = 10, result_folder = "results") {

  results <- NULL

  for (compress in compression) {

    for (run_id in 1:nr_of_runs) {

      cat(".")

      # write to disk

      # write cycle_size files
      for (id in 1:cycle_size) {
        # generate dataset
        x <- table_generator(nr_of_rows)

        file_name <- paste0(result_folder, "/", "dataset_", id)

        # disk warmup (to avoid a sleeping disk after datacreation)
        saveRDS("warmup disk", paste0(result_folder, "/", "warmup.rds"))

        # Only a single iteration is used to avoid disk caching effects
        # Due to caching measured speeds are higher and create a unrealistic benchmark
        res <- microbenchmark({
          table_streamer$table_writer(x, file_name, compress)
          },
          times = 1)

        results <- observation(results, "write", bench_id, compress, file.info(file_name)$size,
          res$time, object.size(x))
      }

      # read from disk

      for (id in 1:cycle_size) {
        file_name <- paste(result_folder, "/", "dataset_", id, sep = "")

        res <- microbenchmark({
            y <- table_streamer$table_reader(file_name)
          },
          times = 1)

        results <- observation(results, "read", bench_id, compress, file.info(file_name)$size, res$time,
          object.size(y))
      }
    }
  }

  results
}
