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


#' Generate a integer vector with certain distribution
#'
#' @param length length of the vector
#' @param max_distict_values maximum number of disctict values in the vector
#'
#' @return integer vector
#' @export
sample_integer <- function(length, max_distict_values = NULL, min_value = 1 - .Machine$integer.max, max_value = .Machine$integer.max) {
  
  if (is.null(max_distict_values)) {
    return(as.integer(runif(length, min_value, max_value)))
  }
  
  x <- as.integer(runif(max_distict_values, min_value, max_value))
  sample(x, length, replace = TRUE)
}
