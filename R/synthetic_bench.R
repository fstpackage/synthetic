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
observation <- function(bench, mode, format_id, data_id, compression, size, time, orig_size) {
  rbindlist(list(bench, data.table(
    Mode = mode,
    ID = format_id,
    DataID = data_id,
    Compression = compression,
    Size = size,
    Time = time,
    OrigSize = orig_size)))
}


#' Runs benchmarks
#'
#' @param nr_of_runs repeat the benchmark for statistics
#' @param cycle_size create cycly_size files before overwriting
#' @param generator function f(nr_of_rows) that generates the data.frame
#' @param nr_of_rows number of rows to use in the benchmark
#' @param compression vector of compression values to use for benchmarking
#' @param result_folder folder to use for temporal storage of results
#' @param bench_id 
#' @param table_streamers a single tablestreamer object generated with table_streamer().
#' Could also be a list of tablestreamer objects to benchmark various streamers.
#'
#' @return benchmarks results
#' @export
synthetic_bench <- function(generator, table_streamers, nr_of_rows,
  compression, nr_of_runs = 100, cycle_size = 10, result_folder = "results") {

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

  for (compress in compression) {

    for (run_id in 1:nr_of_runs) {

      cat("writing ...")

      # write to disk

      # write cycle_size files
      for (id in 1:cycle_size) {

        cat(".")

        # generate dataset once for all generators
        x <- generator$generator(nr_of_rows)

        # disk warmup (to avoid a sleeping disk after data creation)
        saveRDS("warmup disk", paste0(result_folder, "/", "warmup.rds"))

        # iterate
        for (table_streamer in table_streamers[sample(1:length(table_streamers))]) {
          file_name <- paste0(result_folder, "/", "dataset_", table_streamer$id, "_", id)

          # Only a single iteration is used to avoid disk caching effects
          # Due to caching measured speeds are higher and create a unrealistic benchmark
          res <- microbenchmark({
            table_streamer$table_writer(x, file_name, compress)
          },
          times = 1)

          results <- observation(results, "write", table_streamer$id, generator$id, compress,
            file.info(file_name)$size, res$time, object.size(x))
        }
      }

      # read from disk
      cat("\nreading ...")

      for (id in 1:cycle_size) {

        cat(".")

        # iterate
        for (table_streamer in table_streamers[sample(1:length(table_streamers))]) {
          file_name <- paste0(result_folder, "/", "dataset_", table_streamer$id, "_", id)

          res <- microbenchmark({
              y <- table_streamer$table_reader(file_name)
            },
            times = 1)

          results <- observation(results, "read", table_streamer$id, generator$id, compress,
            file.info(file_name)$size, res$time, object.size(y))
        }
      }
    }
  }

  results
}
