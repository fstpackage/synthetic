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
observation <- function(bench, mode, id, compression, size, time) {
  rbindlist(list(bench, data.table(
    Mode = mode,
    ID = id,
    Compression = compression,
    Size = size,
    Time = res$time)))
}


#' Runs benchmarks
#'
#' @param table_writer function f(x, file_name, compress) that writes a data.frame x to file file_name with compression compress
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
synthetic_bench <- function(table_generator, table_writer, table_reader, nr_of_rows,
  compression, nr_of_runs = 100, cycle_size = 10, result_folder = "results") {

  results <- NULL

  for (compress in compression) {

    for (run_id in 1:nr_of_runs) {

      cat(".")

      # write to disk

      # write cycle_size files
      for (id in 1:cycle_size)
      {
        # generate dataset
        x <- table_generator(nr_of_rows)

        file_name <- paste(result_folder, "/", "dataset_", id, sep = "")

        # disk warmup (to avoid a sleeping disk)
        saveRDS("warmup disk", "warmup.rds")

        # Only a single iteration is used to avoid disk caching effects
        # Due to caching measured speeds are higher and create a unrealistic benchmark
        res <- microbenchmark({
            table_writer(x, file_name, compress)
          }, times = 1)

        results <- observation(results, "write", "rds", compress, file.info(file_name)$size, res$time)
      }

      # read from disk

      for (id in 1:cycle_size)
      {
        file_name <- paste(result_folder, "/", "dataset_", id, sep = "")

        res <- microbenchmark({
            table_reader(file_name)
          }, times = 1)

        results <- observation(results, "read", "rds", compress, file.info(file_name)$size, res$time)
      }
    }
  }

  results
}
