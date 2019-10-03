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


#' Generate a numerical vector with uniform distribution
#'
#' @param length length of the vector
#' @param min_value minimum value in the vector
#' @param max_value maximum value in the vector
#' @param max_distict_values maximum number of disctict values in the vector
#'
#' @return numerical vector
#' @export
sample_numerical_uniform <- function(length, min_value = 0.0, max_value = 1.0, max_distict_values = NULL) {

  if (is.null(max_distict_values)) {
    return(runif(length, min_value, max_value))
  }

  x <- runif(max_distict_values, min_value, max_value)
  sample(x, length, replace = TRUE)
}


#' Generate a numerical vector with normal distribution
#'
#' @param length length of the vector
#' @param max_distict_values maximum number of disctict values in the vector
#' @param mean mean of the distribution
#' @param sd standard deviation of the distribution
#'
#' @return numerical vector
#' @export
sample_numerical_normal <- function(length, mean = 0, sd = 1, max_distict_values = NULL) {

  if (is.null(max_distict_values)) {
    return(rnorm(length, mean, sd))
  }

  x <- rnorm(max_distict_values, mean, sd)
  sample(x, length, replace = TRUE)
}
