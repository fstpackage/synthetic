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


#' Define an object of class 'tablestreamer' that defines specific methods for
#' serializing of datasets
#'
#' @param id ID of the table streamer (e.g. 'fst' or 'parguet')
#' @param table_writer method for writing with signature f(x, file_name, compress, threads). Parameter
#' compression should be a percentage (0.0 to 100.0).
#' @param table_reader method for reading with signature f(file_name, threads)
#' @param can_select_threads TRUE of FALSE depending on the ability to select the number of threads
#' @param variable_compression TRUE of FALSE depending on the ability to select compression
#'
#' @return a tablestreamer object
#' @export
table_streamer <- function(id, table_writer, table_reader, can_select_threads, variable_compression) {
  x <- list(
    id = id,
    table_writer = table_writer,
    table_reader = table_reader,
    can_select_threads = can_select_threads,
    variable_compression = variable_compression
  )

  class(x) <- "tablestreamer"

  x
}
