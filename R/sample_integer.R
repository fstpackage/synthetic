#  synthetic - R package for synthetic dataset creation and serialization benchmarks
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
#  - synthetic R package source repository : https://github.com/fstpackage/synthetic


#' Generate a integer vector with certain distribution
#'
#' @param length length of the vector
#' @param max_distict_values maximum number of disctict values in the vector
#' @param min_value minimum value in the vector
#' @param max_value maximum value in the vector
#'
#' @return integer vector
#' @export
sample_integer <- function(length, min_value = 1 - .Machine$integer.max, max_value = .Machine$integer.max,
  max_distict_values = NULL) {

  if (is.null(max_distict_values)) {
    return(sample(min_value : max_value, length, replace = TRUE))
  }

  x <- sample(min_value : max_value, max_distict_values)  # unique values
  sample(x, length, replace = TRUE)
}
