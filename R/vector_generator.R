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
  if ("column_template" %in% class(template)) {
    if (!is.null(columns)) {
      stop("Parameter columns cannot be used with column templates")
    }

    return(template$generate(template$metadata, size))
  }

  # table template
  template$generate(template$metadata, size)
}


#' Create a template for creation of a vector with custom distribution
#'
#' @param metadata data needed by the generator function to create a synthetic vector
#' @param generator function with signature f(metadata, length) that generates a
#' vector from the metadata and the size
#' @param print_method  function with signature f(metadata) that prints the vector
#' template description. Will be used when  print() is called.
#'
#' @return an object of class 'columntemplate' that can be used with generate() to generate
#' vectors according to the characteristics stored in metadata
#' @export
vector_template <- function(metadata, generator, print_method = NULL) {
  if (!is.function(generator)) {
    stop("generator is expected to be a function(metadata, size) that can generate a vector")
  }

  x <- list(
    metadata = metadata,
    generator = generator,
    printer = print_method
  )

  class(x) <- "vectortemplate"

  x
}


#' Print a vector template description
#'
#' @param x vector template
#' @param ... additional parameters (will be discarded)
#' @export
print.vectortemplate <- function(x, ...) {
  if (is.null(x$printer)) {
    print(x)
    return()
  }

  x$printer(x$metadata)
}
