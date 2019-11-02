#  synthetic - R package for synthetic dataset creation and serialization benchmarks
#
#  Copyright (C) 2019-present, Mark AJ Klik
#
#  This file is part of the synthetic R package.
#
#  The synthetic R package is free software: you can redistribute it and/or modify it
#  under the terms of the GNU Affero General Public License version 3 as
#  published by the Free Software Foundation.
#
#  The synthetic R package is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
#  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
#  for more details.
#
#  You should have received a copy of the GNU Affero General Public License along
#  with the synthetic R package. If not, see <http://www.gnu.org/licenses/>.
#
#  You can contact the author at:
#  - synthetic R package source repository : https://github.com/fstpackage/synthetic


# Add an observation to benchmark results
observation <- function(observations, mode, format_id, data_id, compression, size, time, nr_of_rows, orig_size) {
  rbindlist(list(observations, data.table(
    Mode = mode,
    ID = format_id,
    DataID = data_id,
    Compression = compression,
    Size = as.numeric(size),
    Time = time,
    NrOfRows = nr_of_rows,
    OrigSize = as.numeric(orig_size))))
}


#' Define a benchmark
#'
#' @param nr_of_runs repeat the benchmark for statistics
#' @param cycle_size create cycle_size files before overwriting
#' @param result_folder folder to use for temporal storage of results
#' @param progress if TRUE, a progress bar is displayed with the progress and estimated
#' remaining time for the benchmark
#'
#' @return A synth_bench class that can be used in pipes
#' @export
synthetic_bench <- function(nr_of_runs = 3, cycle_size = 3, result_folder = "results", progress = TRUE) {

  if (!dir.exists(result_folder)) {
    dir.create(result_folder)
  }

  bench_obj <- list(
    nr_of_runs = nr_of_runs,
    column_mode = "all",
    cycle_size = cycle_size,
    result_folder = normalizePath(result_folder),
    progress = TRUE
  )

  class(bench_obj) <- "benchmark_definition"

  bench_obj
}


#' Print details of synthetic benchmark
#'
#' @param ... other arguments to print
#' @param x synt_bench object
#'
#' @export
print.benchmark_definition <- function(x, ...) {

  cat(cyan("Synthetic benchmark"), "\n")
  cat(cyan("number of runs    : "), x$nr_of_runs, "\n", sep = "")
  cat(cyan("cycle size        : "), x$cycle_size, "\n", sep = "")
  cat(cyan("result folder     : "), x$result_folder, "\n", sep = "")
  cat(cyan("show progress     : "), x$progress, "\n", sep = "")

  # datasets

  cat(cyan("datasets          : "))

  if (!is.null(x$generators)) {
    cat(paste0(sapply(x$generators, function(generator) {
        generator$id
      }), collapse = "', '"), "'", sep = "")
  } else {
    cat(red("not defined yet"), "\n")
  }

  # columns

  if (length(x$nr_of_columns) != 0) {
    nr_of_cols_str <- paste0(head(sort(x$nr_of_columns), 5), collapse = ", ")
    if (length(x$nr_of_columns) > 5) {
      nr_of_cols_str <- paste(nr_of_cols_str, "...")
    }

    cat(cyan("number of columns : "), nr_of_cols_str, "\n", sep = "")
  }

  # column mode

  cat(cyan("column mode       : "), x$column_mode, "\n", sep = "")

  # number of rows

  if (!is.null(x$nr_of_rows)) {
    nr_of_rows_str <- paste0(head(sort(x$nr_of_rows), 5), collapse = ", ")
    if (length(x$nr_of_rows) > 5) {
      nr_of_rows_str <- paste(nr_of_rows_str, "...")
    }

    cat(cyan("number of rows    : "), nr_of_rows_str, "\n", sep = "")
  } else {
    cat(cyan("number of rows    : "), red("not defined yet"), "\n", sep = "")
  }

  # compression

  if (length(x$compression) != 0) {
    compression_str <- paste0(head(sort(x$compression), 5), collapse = ", ")
    if (length(x$compression) > 5) {
      compression_str <- paste(compression_str, "...")
    }

    cat(cyan("compression       : "), compression_str, "\n", sep = "")
  } else {
    cat(cyan("compression       : "), "default", "\n", sep = "")
  }

  # threads

  if (length(x$threads) != 0) {
    threads_str <- paste0(head(sort(x$threads), 5), collapse = ", ")
    if (length(x$threads) > 5) {
      threads_str <- paste(threads_str, "...")
    }

    cat(cyan("number of threads : "), threads_str, "\n", sep = "")
  } else {
    cat(cyan("number of threads : "), "default", "\n", sep = "")
  }
}


#' Add datasets or dataset generators to the benchmark
#'
#' @param bench_obj A benchmark definition created with synthetic_bench()
#' @param ... One or more synthetic_table or data.frame objects. A synthetic_table can be created
#' using method synthetic_table().
#'
#' @return An updated benchmark definition object
#' @export
bench_tables <- function(bench_obj, ...) {

  generators <- list(...)

  # check each element for correct generator class
  lapply(generators, function(x) {
    if (class(x) != "tabledefinition") stop("Incorrectly defined synthetic tables, please use method",
      " table_definition() to create table definition objects")
  })

  # add to definition
  bench_obj[["generators"]] <- generators

  bench_obj
}


#' Add table size to the benchmark
#'
#' @param bench_obj A benchmark definition created with synthetic_bench()
#' @param ... One or more numerical vectors defining the number of rows to use for benchmarking
#'
#' @return An updated benchmark definition object
#' @export
bench_rows <- function(bench_obj, ...) {

  row_vectors <- list(...)

  # verify table streamers
  lapply(row_vectors, function(x) {
    if (!is.numeric(x)) stop("Incorrectly defined number of rows, please use one or more numerical",
      " vectors to define the number of rows you need for benchmarking")
  })

  bench_obj[["nr_of_rows"]] <- unique(as.integer(unlist(row_vectors)))

  bench_obj
}


#' Add table size to the benchmark
#'
#' @param bench_obj A benchmark definition created with synthetic_bench()
#' @param column_mode Mode to use for benchmarking columns. The default 'all' just used the complete
#' table, 'single' benchmarks each column separately and 'type' separates the columns in groups of
#' a single type (integer, double, etc.)
#' @param ... One or more numerical vectors defining the number of columns to use in the benchmarks.
#' For values larger than the number of columns in the dataset, columns are recycled using the
#' appropriate column definition (generator).
#' @return An updated benchmark definition object
#' @export
bench_columns <- function(bench_obj, column_mode = "all", ...) {

  if (!(column_mode %in% c("all", "single", "type"))) {
    stop("Parameter column_mode should be one of 'all' (default), 'single' or 'type'")
  }

  col_vectors <- list(...)

  # verify table streamers
  lapply(col_vectors, function(x) {
    if (!is.numeric(x)) stop("Incorrectly defined number of columns, please use one or more numerical",
      " vectors to define the number of columns you need for benchmarking")
  })

  bench_obj[["columns_mode"]] <- column_mode
  bench_obj[["nr_of_columns"]] <- unique(as.integer(unlist(col_vectors)))

  bench_obj
}


#' Add streamers to the benchmark
#'
#' @param bench_obj A benchmark definition created with synthetic_bench()
#' @param ... One or more table streamers created with table_streamer()
#'
#' @return An updated benchmark definition object
#' @export
bench_streamers <- function(bench_obj, ...) {

  table_streamers <- list(...)

  # verify table streamers
  lapply(table_streamers, function(x) {
    if (class(x) != "tablestreamer") stop("Incorrectly defined streamer, please use method",
      " table_streamer() to create streamers")
  })

  bench_obj[["streamers"]] <- table_streamers

  bench_obj
}


#' Add compression to the benchmark
#'
#' @param bench_obj A benchmark definition created with synthetic_bench()
#' @param ... One or more numerical vectors defining the compression percentage to use for benchmarking
#'
#' @return An updated benchmark definition object
#' @export
bench_compression <- function(bench_obj, ...) {

  compress_vectors <- list(...)

  # verify table streamers
  lapply(compress_vectors, function(x) {
    if (!is.numeric(x)) stop("Incorrectly defined compression, please use one or more numerical",
      " vectors to define the compression you need for benchmarking")
  })

  bench_obj[["compression"]] <- unique(unlist(compress_vectors))

  bench_obj
}


#' Add thread count to the benchmark
#'
#' @param bench_obj A benchmark definition created with synthetic_bench()
#' @param ... One or more numerical vectors defining the number of threads to use for benchmarking
#'
#' @return An updated benchmark definition object
#' @export
bench_threads <- function(bench_obj, ...) {

  thread_vectors <- list(...)

  # verify table streamers
  lapply(thread_vectors, function(x) {
    if (!is.numeric(x)) stop("Incorrectly defined number of threads, please use one or more numerical",
      " vectors to define the number of threads you need for benchmarking")
  })

  bench_obj[["threads"]] <- unique(unlist(thread_vectors))

  bench_obj
}


#' Compute generic function to start computing a benchmark
#'
#' @param x A benchmark definition created with synthetic_bench()
#' @param ... other arguments passed on to method
#'
#' @return Benchmark resuls
#' @export
collect.benchmark_definition <- function(x, ...) {  # nolint

  if (is.null(x$streamers)) {
    stop("You need to define at least one streamer to benchmark")
  }

  # define progress bar
  if (x$progress) {

    compression_steps <- length(x$streamers)

    if (!is.null(x$compression)) {
      compressors <- 0
      for (table_streamer in x$streamers) {
        if (table_streamer$variable_compression) compressors <- compressors + 1
      }

      compression_steps <- compressors * length(x$compression) + length(x$streamers) - compressors
    }

    nr_of_measurements <- 2 * compression_steps * x$nr_of_runs * x$cycle_size *
      length(x$nr_of_rows) * length(x$generators)
    row_weights <- length(x$nr_of_rows) * x$nr_of_rows / sum(x$nr_of_rows)
    measurement_count <- 0

    pb <- progress_bar$new("[:bar] :percent remaining: :eta", total = 100)
  }

  # create a length 1 vector
  compression <- x$compression
  if (is.null(compression)) {
    compression <- -1
  }

  results <- NULL

  for (nr_of_rows_index in seq_len(length(x$nr_of_rows))) {

    cur_nr_of_rows <- x$nr_of_rows[nr_of_rows_index]

    for (run_id in seq_len(x$nr_of_runs)) {

      # loop over compression settings
      for (compress_count in seq_len(length(compression))) {

        write_compression <- compression[compress_count]
        if (write_compression == -1) write_compression <- NULL

        # write cycle_size files
        for (id in seq_len(x$cycle_size)) {

          # loop over datasets
          for (generator_count in seq_len(length(x$generators))) {

            generator <- x$generators[[generator_count]]

            # generate dataset once for all generators
            dt <- generator$generator(cur_nr_of_rows)

            # disk warmup (to avoid a sleeping disk after data creation)
            saveRDS("warmup disk", paste0(x$result_folder, "/", "warmup.rds"))

            # iterate
            for (table_streamer in x$streamers[sample(seq_len(length(x$streamers)))]) {

              # don't repeat identical measurements
              if (!table_streamer$variable_compression && compress_count > 1) next

              file_name <- paste0(x$result_folder, "/", "dataset_", table_streamer$id, "_",
                generator_count, "_", id)

              # Only a single iteration is used to avoid disk caching effects
              # Due to caching measured speeds are higher and create a unrealistic benchmark
              res <- microbenchmark({
                table_streamer$table_writer(dt, file_name, write_compression)
              },
              times = 1)

              results <- observation(results, "write", table_streamer$id, generator$id,
                compression[compress_count], file.info(file_name)$size, res$time, cur_nr_of_rows, object.size(x))

              if (x$progress) {
                measurement_count <- measurement_count + row_weights[nr_of_rows_index]
                pb$update(measurement_count / nr_of_measurements - 0.0001)
              }
            }
          }
        }

        for (id in seq_len(x$cycle_size)) {

          # loop over datasets
          for (generator_count in seq_len(length(x$generators))) {

            generator <- x$generators[[generator_count]]

            # iterate
            for (table_streamer in x$streamers[sample(seq_len(length(x$streamers)))]) {

              # don't repeat identical measurements
              if (!table_streamer$variable_compression && compress_count > 1) next

              file_name <- paste0(x$result_folder, "/", "dataset_", table_streamer$id, "_",
                generator_count, "_", id)

              res <- microbenchmark({
                y <- table_streamer$table_reader(file_name)
              },
              times = 1)

              results <- observation(results, "read", table_streamer$id, generator$id,
                compression[compress_count], file.info(file_name)$size, res$time, cur_nr_of_rows, object.size(y))

              if (x$progress) {
                measurement_count <- measurement_count + row_weights[nr_of_rows_index]
                pb$update(measurement_count / nr_of_measurements - 0.0001)
              }
            }
          }
        }
      }
    }
  }

  if (x$progress) {
    pb$update(1)
  }

  # avoid note in R CMD check
  OrigSize <- Time <- 0  # nolint

  results %>%
    mutate(SpeedMBs = 1000 * OrigSize / Time)
}
