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


raw_printer <- function(metadata) {

  cat(italic(cyan("raw vector template")), "\n")
  cat(cyan("- values between "), delayed_to_str(metadata$min_value), cyan(" and "),
      delayed_to_str(metadata$max_value), "\n", sep = "")

  if (!is.null(metadata$max_distict_values)) {
    cat(cyan("- max number of distinct values : "),
        delayed_to_str(metadata$max_distict_values), "\n", sep = "")
  }
}


#' Generate a raw vector with custom distribution
#'
#' @param min_value minimum value in the returned raw vector
#' @param max_value maximum value in the returned raw vector
#' @param max_distict_values maximum number of disctict values in the vector
#'
#' @return raw vector
#' @export
template_raw <- function(min_value = 0, max_value = 255, max_distict_values = NULL) {

  if (min_value < 0) {
    stop("Parameter min_value should be equal to or larger than 0")
  }

  if (max_value > 255) {
    stop("Parameter max_value should be equal to or smaller than 255")
  }

  if (!is.null(max_distict_values) && max_distict_values > 256) {
    stop("Parameter max_distinct_values should be in the range 1 to 256 or NULL")
  }

  metadata <- list(
    min_value = min_value,
    max_value = max_value,
    max_distict_values = max_distict_values
  )

  if (is.null(max_distict_values)) {
    generator <- function(metadata, length) {
      as.raw(sample(metadata$min_value:metadata$max_value, length, replace = TRUE))
    }
  } else {
    generator <- function(metadata, length) {
      max_distict_values <- min(metadata$max_distict_values, 1 + metadata$max_value - metadata$min_value)
      x <- sample(metadata$min_value:metadata$max_value, max_distict_values)  # unique values
      as.raw(sample(x, length, replace = TRUE))
    }
  }

  return(vector_template(metadata, generator, raw_printer))
}
