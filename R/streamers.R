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
streamer_rds <- function(id = "rds") {
  table_streamer(
    id = id,
    table_writer = function(x, file_name, compress) {
      saveRDS(x, file_name)
    },
    table_reader = function(x) readRDS(x),
    can_select_threads = FALSE,
    variable_compression = FALSE
  )
}


#' fst streamer
#'
#' @param id Identifier to mark the streamer in benchmarks (default: 'fst')
#'
#' @return A table streamer that uses the fst format
streamer_fst <- function(id = "fst") {
  table_streamer(
    id = id,
    table_writer = function(x, file_name, compress) {
      if (is.null(compress)) {
        return(write_fst(x, file_name))
      }
      write_fst(x, file_name, compress)
    },
    table_reader = function(x) read_fst(x),
    can_select_threads = TRUE,
    variable_compression = TRUE
  )
}


#' parguet streamer
#'
#' @param id Identifier to mark the streamer in benchmarks (default: 'parguet')
#'
#' @return A table streamer that uses the parguet format
streamer_parguet <- function(id = "parguet") {

  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Please install package arrow to use the parguet streamer")
  }

  table_streamer(
    id = id,
    table_writer = function(x, file_name, compress) {
      arrow::write_parquet(x, file_name)
    },
    table_reader = function(x) arrow::read_parquet(x),
    can_select_threads = FALSE,
    variable_compression = FALSE
  )
}


#' arrow streamer
#'
#' @param id Identifier to mark the streamer in benchmarks (default: 'arrow')
#'
#' @return A table streamer that uses the arrow format
streamer_arrow <- function(id = "arrow") {

  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Please install package arrow to use the arrow streamer")
  }

  table_streamer(
    id = id,
    table_writer = function(x, file_name, compress) {
      arrow::write_arrow(x, file_name)
    },
    table_reader = function(x) arrow::read_arrow(x),
    can_select_threads = FALSE,
    variable_compression = FALSE
  )
}


#' feather streamer
#'
#' @param id Identifier to mark the streamer in benchmarks (default: 'feather')
#'
#' @return A table streamer that uses the feather format
streamer_feather <- function(id = "feather") {

  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Please install package arrow to use the feather streamer")
  }

  table_streamer(
    id = id,
    table_writer = function(x, file_name, compress) {
      arrow::write_feather(x, file_name)
    },
    table_reader = function(x) arrow::read_feather(x),
    can_select_threads = FALSE,
    variable_compression = FALSE
  )
}