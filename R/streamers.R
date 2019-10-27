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


#' baseR streamer
#'
#' @param id Identifier to mark the streamer in benchmarks (default: 'rds')
#'
#' @return A table streamer that uses the rds format
#' @export
streamer_rds <- function(id = "rds") {
  table_streamer(
    id = id,
    table_writer = function(x, file_name, compress, custom_parameters) {
      saveRDS(x, file_name)
    },
    table_reader = function(x, custom_parameters) readRDS(x)
  )
}


#' fst streamer
#'
#' @param id Identifier to mark the streamer in benchmarks (default: 'fst')
#'
#' @return A table streamer that uses the fst format
#' @export
streamer_fst <- function(id = "fst") {
  table_streamer(
    id = id,
    table_writer = function(x, file_name, compress, custom_parameters) {
      if (is.null(compress)) {
        return(write_fst(x, file_name))
      }
      write_fst(x, file_name, compress)
    },
    table_reader = function(x, custom_parameters) read_fst(x),
    set_threads = function(nr_of_threads, custom_parameters) {threads_fst(nr_of_threads)},
    can_select_threads = TRUE,
    variable_compression = TRUE
  )
}


#' parguet streamer
#'
#' @param id Identifier to mark the streamer in benchmarks (default: 'parguet')
#'
#' @return A table streamer that uses the parguet format
#' @export
streamer_parguet <- function(id = "parguet") {

  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Please install package arrow to use the parguet streamer")
  }

  table_streamer(
    id = id,
    table_writer = function(x, file_name, compress, custom_parameters) {
      arrow::write_parquet(x, file_name)
    },
    table_reader = function(x, custom_parameters) arrow::read_parquet(x),
    can_select_threads = FALSE,
    variable_compression = FALSE
  )
}


#' arrow streamer
#'
#' @param id Identifier to mark the streamer in benchmarks (default: 'arrow')
#'
#' @return A table streamer that uses the arrow format
#' @export
streamer_arrow <- function(id = "arrow") {

  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Please install package arrow to use the arrow streamer")
  }

  table_streamer(
    id = id,
    table_writer = function(x, file_name, compress, custom_parameters) {
      arrow::write_arrow(x, file_name)
    },
    table_reader = function(x, custom_parameters) arrow::read_arrow(x),
    can_select_threads = FALSE,
    variable_compression = FALSE
  )
}


#' feather streamer
#'
#' @param id Identifier to mark the streamer in benchmarks (default: 'feather')
#'
#' @return A table streamer that uses the feather format
#' @export
streamer_feather <- function(id = "feather") {

  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Please install package arrow to use the feather streamer")
  }

  table_streamer(
    id = id,
    table_writer = function(x, file_name, compress, custom_parameters) {
      arrow::write_feather(x, file_name)
    },
    table_reader = function(x, custom_parameters) {
      arrow::read_feather(x)
    },
    can_select_threads = FALSE,
    variable_compression = FALSE
  )
}


#' fread/fwrite streamer
#'
#' @param id Identifier to mark the streamer in benchmarks (default: 'data.table')
#'
#' @return A table streamer that uses data.table's fread/fwrite to read and write the csv format
#' @export
streamer_datatable <- function(id = "data.table") {
  
  table_streamer(
    id = id,
    table_writer = function(x, file_name, compress, custom_parameters) {
      fwrite(x, file_name)
    },
    table_reader = function(x, custom_parameters) fread(x),
    set_threads = function(nr_of_threads, custom_parameters) data.table::setDTthreads(nr_of_threads),
    can_select_threads = TRUE,
    variable_compression = FALSE
  )
}


#' vroom streamer
#'
#' @param id Identifier to mark the streamer in benchmarks (default: 'vroom')
#' @param altrep_opts Set ALTREP options for method `vroom::vroom()`. See that
#' functions description of the meaning of this parameter. Beware that enabling
#' ALTREP functionality for vroom can lead to incomparable benchmarks as the
#' dataset returned from `vroom` is not materialized in memory for types that use
#' ALTREP. In effect, part of the deserialization is postponed to a later time.
#'
#' @return A table streamer that uses vroom to read and write the csv format
#' @export
streamer_vroom <- function(id = "vroom", altrep_opts = FALSE) {
  
  if (!requireNamespace("vroom", quietly = TRUE)) {
    stop("Please install package vroom to use the vroom streamer")
  }

  table_streamer(
    id = id,
    table_writer = function(x, file_name, compress) vroom::vroom_write(x, file_name),
    table_reader = function(x) {
      suppressMessages(vroom::vroom(x, altrep_opts = altrep_opts, progress = FALSE))
    },
    can_select_threads = FALSE,
    variable_compression = FALSE
  )
}
