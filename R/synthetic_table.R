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


numerical_model_printer <- function(metadata) {

  cat(italic(cyan("numerical model based vector template")), "\n")
  cat(cyan("- values between "), metadata$values[1] + 0.5 * metadata$derivatives[1],
      cyan(" and "), tail(metadata$values, 1) + 0.5 * tail(metadata$derivatives, 1),
      "\n", sep = "")
}


template_from_column <- function(column) {

  # implement this method in C for speed

  # numeric columns
  if (typeof(column) == "double") {
    return(dbl_template_from_column(column))
  }
  
  stop("Unknown column format")
}

  
# library(ggplot2)
# 
# length = 10000
# dist <- sort(runif(length))
# distr <- data.frame(X = 1:10000, Y = dist)
# ggplot(distr) +
#   geom_line(aes(x = X, y = Y))
# 
# distr <- data.frame(X = 0.5 + 0:2339, Y = column)
# means <- data.frame(X = 0.5 * block_size + block_size * 0:99, Y = x)
# 
# ggplot(distr[1:100, ]) +
#   geom_line(aes(x = X, y = Y)) +
#   geom_point(data = means[1:5, ], aes(x = X, y = Y))
# 
# ggplot(distr) +
#   geom_line(aes(x = X, y = Y)) +
#   geom_point(data = means, aes(x = X, y = Y))
# 
# template_from_table <- function(dt) {
#     
# }

#' Create the blueprints of a synthetic table
#'
#' @param id ID of the synthetic table (e.g. 'fst homepage dataset')
#' @param ... column templates created with the various template_ methods or a single dataset
#' @param construct_model If parameter ... is a table, this parameter is used to determine how
#' the table model is generated. When `construct_model = TRUE`, the columns from the table
#' are used to construct an approximation of the column's distribution. This approximation
#' is used to generate the actual data. When `construct_model = FALSE`, the whole table is
#' stored and generation of new columns is done by taking random samples from the original
#' table.
#'
#' @return a synthetic table template that can be used to generate synthetic data using generate()
#' @export
synthetic_table <- function(..., id = "synthetic table", construct_model = FALSE) {

  column_definitions <- list(...)

  if (length(column_definitions) == 0) {
    stop("Please specify at least one column template to define the table. Alternatively, you ",
    "can specify a table that will be used as the source for generating synthetic data.")
  }

  if (length(column_definitions) == 1) {
    if (inherits(column_definitions[[1]], "data.frame")) {
      setDT(column_definitions[[1]])

      # small tables just get stored      
      if (!construct_model) {
        x <- structure(
          list(
            id = id,
            columns = NULL,
            source_table = column_definitions[[1]]
          ), 
          class = "tabletemplate"
        )
        
        return(x)
      }

      x <- structure(
        list(
          id = id,
          columns = template_from_table(column_definitions[[1]]),
          source_table = NULL
        ), 
        class = "tabletemplate"
      )
      
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
