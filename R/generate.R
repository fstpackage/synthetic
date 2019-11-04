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


#' Generate a vector or table from a custom template
#'
#' @param template column or table template that you want to use to create your vector or 
#' table
#' @param size desired size of the resulting vector or table
#' @param columns used to select specific columns from a table template. Can be a character
#' vector with column names or an integer vector with the column index
#'
#' @return A vector or table generated from the specified template
#' @export
generate <- function(template, size, columns = NULL) {
  
  # column template
  if (inherits(template, "vectortemplate")) {
    if (!is.null(columns)) {
      stop("Parameter columns cannot be used with column templates")
    }

    if (!is.numeric(size) || size < 0) {
      stop("Please use a numeric value larger than 0 to specify the size")
    }

    return(template$generate(template$metadata, size))
  }

  if (!inherits(template, "tabletemplate"))  {
    stop("Parameter template must be a vector or table template. Vector templates can be created ",
      "with one of the template_ methods. Table templates can be created with method synthetic_table().")
  }

  table_type <- getOption("synthetic_default_table_class")

  # table template with a source table
  if (!is.null(template$source_table)) {
    rows <- sample(1:nrow(template$source_table), size, replace = TRUE)

    x <- template$source_table  # stored as a data.table

    # with column selection
    if (!is.null(columns)) {
      x <- x[, columns, with = FALSE]
    }

    if (!is.null(table_type) && table_type == "data.table") {
      return(x[rows])
    }

    x[rows] %>%
      as_tibble(rows) %>%
      return()
  }

  # table template with vector templates
  x <- lapply(template$columns, function(vec_template) {
    generate(vec_template, size)
  })

  if (table_type == "data.table") {
    setDT(x)
    return(x)
  }

  return(as_tibble(x))
}
