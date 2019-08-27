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


# predefined allowed characters
char_pool <- c(LETTERS, letters, 0:9)


generate_string <- function(size) {
  paste0(sample(char_pool, size), collapse = "")
}


#' Generate a character vector with certain distribution of string lengths
#'
#' @param length length of the vector
#' @param max_distict_values maximum number of disctict values in the vector
#' @param min_str_size minimum string length
#' @param max_str_size maximum string length
#'
#' @return character vector
#' @export
sample_string <- function(length, min_str_size = 1, max_str_size = 10, max_distict_values = NULL) {

  if (is.null(max_distict_values)) {
    sizes <- sample(min_str_size:max_str_size, length, replace = TRUE)
    return(sapply(sizes, generate_string))
  }

  sizes <- sample(min_str_size:max_str_size, max_distict_values, replace = TRUE)
  x <- sapply(sizes, generate_string)  # unique values
  sample(x, length, replace = TRUE)
}
