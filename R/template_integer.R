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


#' Define a template for integer vectors with custom distribution and characteristics
#'
#' @param length length of the vector
#' @param max_distict_values maximum number of disctict values in the vector
#' @param min_value minimum value in the vector
#' @param max_value maximum value in the vector
#'
#' @return integer vector
#' @export
template_integer <- function(min_value = 1 - .Machine$integer.max,
  max_value = .Machine$integer.max, max_distict_values = NULL) {

  metadata <- list(
    min_value = min_value,
    max_value = max_value,
    max_distict_values = max_distict_values
  )

  printer <- function(metadata) {
    cat(italic(cyan("integer vector template")), "\n")
    cat(cyan("- values between "), metadata$min_value, cyan(" and "), metadata$max_value, "\n", sep = "")

    if (!is.null(metadata$max_distict_values)) {
      cat(cyan("- max number of distinct values : "), metadata$max_distict_values, "\n", sep = "")
    }
  }

  if (is.null(max_distict_values)) {
    generator <- function(metadata, length) {
      sample(metadata$min_value:metadata$max_value, length, replace = TRUE)
    }
    return(vector_template(metadata, generator, printer))
  }

  # use distinct values
  generator <- function(metadata, length) {
    x <- sample(metadata$min_value:metadata$max_value, metadata$max_distict_values)  # unique values
    sample(x, length, replace = TRUE)
  }

  vector_template(metadata, generator, printer)
}
