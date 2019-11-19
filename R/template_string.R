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


string_printer <- function(metadata) {

  cat(italic(cyan("string vector template")), "\n")
  cat(cyan("- sizes between "), delayed_to_str(metadata$min_str_size), cyan(" and "),
      delayed_to_str(metadata$max_str_size), "\n", sep = "")

  if (!is.null(metadata$max_distict_values)) {
    cat(cyan("- max number of distinct values : "),
        delayed_to_str(metadata$max_distict_values), "\n", sep = "")
  }
}


#' Generate a character vector with certain distribution of string lengths
#'
#' @param max_distict_values maximum number of disctict values in the vector
#' @param min_str_size minimum string length
#' @param max_str_size maximum string length
#'
#' @return character vector
#' @export
template_string_random <- function(min_str_size = 1, max_str_size = 10, max_distict_values = NULL) {

  metadata <- list(
    min_str_size = min_str_size,
    max_str_size = max_str_size,
    max_distict_values = max_distict_values
  )

  # fully random vector
  if (is.null(max_distict_values)) {

    # only a sinlge size of string
    if (min_str_size == max_str_size) {
      generator <- function(metadata, length) {
        stringi::stri_rand_strings(length, metadata$min_str_size)
      }
    } else {
      generator <- function(metadata, length) {
        sizes <- sample(metadata$min_str_size:metadata$max_str_size, length, replace = TRUE)
        stringi::stri_rand_strings(length, sizes)
      }
    }

    return(vector_template(metadata, generator, string_printer))
  }

  # limit number of unique values
  if (min_str_size == max_str_size) {
    generator <- function(metadata, length) {
      max_distict_values <- min(metadata$max_distict_values, 1 + metadata$max_str_size - metadata$min_str_size)
      x <- stringi::stri_rand_strings(max_distict_values, metadata$min_str_size)
      sample(x, length, replace = TRUE)
    }
  } else {
    generator <- function(metadata, length) {
      max_distict_values <- min(metadata$max_distict_values, 1 + metadata$max_str_size - metadata$min_str_size)
      sizes <- sample(metadata$min_str_size:metadata$max_str_size, max_distict_values, replace = TRUE)
      x <- stringi::stri_rand_strings(max_distict_values, sizes)
      sample(x, length, replace = TRUE)
    }
  }

  vector_template(metadata, generator, string_printer)
}
