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


#' Define an object of class 'tablegenerator' that defines specific methods for
#' creation of a dataset
#'
#' @param id ID of the table generator (e.g. 'single column sparse')
#' @param generator method with signature f(nr_of_rows) that generates a data.frame
#'
#' @return a tablestreamer object
#' @export
table_generator <- function(id, generator) {
  x <- list(
    id = id,
    generator = generator
  )

  class(x) <- "tablegenerator"

  x
}
