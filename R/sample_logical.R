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


#' Generate a logical vector with certain distribution of TRUE, FALSE and NA
#'
#' @param length length of the vector
#' @param true_false_na_ratio relative ratio of values TRUE, FALSE and NA
#'
#' @return logical vector
#' @export
sample_logical <- function(length, true_false_na_ratio = c(7, 3, 0)) {

  # normalize
  true_false_na_ratio <- true_false_na_ratio / sum(true_false_na_ratio)

  choice_set <- c(TRUE, FALSE, NA)[true_false_na_ratio > 0]
  true_false_na_ratio <- true_false_na_ratio[true_false_na_ratio > 0]

  sample(choice_set, length, prob = true_false_na_ratio, replace = TRUE)
}
