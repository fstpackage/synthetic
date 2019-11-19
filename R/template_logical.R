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


template_logical_printer <- function(metadata) {

  cat(italic(cyan("logical vector template")), "\n")
  cat(
    cyan("- TRUE/FALSE/NA ratio: "),
    paste(delayed_to_str(metadata$true_false_na_ratio), collapse = "/"),
    "\n", sep = "")
}


#' Define a template for logical vectors with custom distribution of TRUE, FALSE and NA
#'
#' @param true_false_na_ratio relative ratio of values TRUE, FALSE and NA
#'
#' @return logical vector
#' @export
template_logical <- function(true_false_na_ratio = c(1, 1, 0)) {

  # test input
  if (length(true_false_na_ratio) != 3 || !is.numeric(true_false_na_ratio)) {
    stop("Parameter true_false_na_ratio must be a numerical vector of length 3, for example c(2, 9, 4)")
  }

  metadata <- list(
    true_false_na_ratio = true_false_na_ratio
  )

  generator <- function(metadata, length) {
    # normalize
    true_false_na_ratio <- true_false_na_ratio / sum(true_false_na_ratio)
    choice_set <- c(TRUE, FALSE, NA)[true_false_na_ratio > 0]
    true_false_na_ratio <- true_false_na_ratio[true_false_na_ratio > 0]

    # sample
    sample(choice_set, length, prob = true_false_na_ratio, replace = TRUE)
  }

  return(vector_template(metadata, generator, template_logical_printer))
}
