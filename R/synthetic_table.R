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


#' Create the blueprints of a synthetic table
#'
#' @param id ID of the synthetic table (e.g. 'fst homepage dataset')
#' @param ... column templates created with the various template_ methods
#'
#' @return a synthetic table template that can be used to generate synthetic data using generate()
#' @export
synthetic_table <- function(..., id = "synthetic_table", return_type = options("synthetic_return_type")) {

  column_definitions <- list(...)

  if (length(column_definitions) == 0) {
    stop("Please specify at least one column to define the table. Alternatively, you ",
    "can specify a table that will be used as the source for generating synthetic data.")
  }

  if (length(column_definitions) == 1) {
    if (inherits(column_definitions[[1]], "data.frame")) {
      setDT(column_definitions[[1]])
      x <- list(
        id = id,
        columns = NULL,
        source_table = column_definitions[[1]]
      )

      class(x) <- "tabletemplate"

      return(x)
    }
  }

  lapply(column_definitions, function(col_def) {
    if (!("vectortemplate") %in% class(col_def)) {
      stop("Incorrect column definitions, columns must be vector templates",
        " generated with one of the template_ methods")
    }
  })

  x <- list(
    id = id,
    columns = column_definitions,
    source_table = NULL
  )

  class(x) <- "tabletemplate"

  x
}
