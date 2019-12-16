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


numerical_uniform_printer <- function(metadata) {

  cat(italic(cyan("uniform numerical vector template")), "\n")
  cat(cyan("- values between "), delayed_to_str(metadata$min_value), cyan(" and "),
      delayed_to_str(metadata$max_value), "\n", sep = "")

  if (!is.null(metadata$max_distict_values)) {
    cat(cyan("- max number of distinct values : "),
        delayed_to_str(metadata$max_distict_values), "\n", sep = "")
  }
}


numerical_normal_printer <- function(metadata) {

  cat(italic(cyan("normal numerical vector template")), "\n")
  cat(cyan("mean                  : "), delayed_to_str(metadata$mean), "\n")
  cat(cyan("sd                    : "), delayed_to_str(metadata$sd), "\n")

  if (!is.null(metadata$max_distict_values)) {
    cat(cyan("nr of distinct values : "),
        delayed_to_str(metadata$max_distict_values), "\n", sep = "")
  }
}


#' Generate a numerical vector with uniform distribution
#'
#' @param min_value minimum value in the vector
#' @param max_value maximum value in the vector
#' @param max_distict_values maximum number of disctict values in the vector
#'
#' @return numerical vector
#' @export
template_numerical_uniform <- function(min_value = 0.0, max_value = 1.0, max_distict_values = NULL) {

  metadata <- list(
    min_value = min_value,
    max_value = max_value,
    max_distict_values = max_distict_values
  )

  if (is.null(max_distict_values)) {
    generator <- function(metadata, length) {
      runif(length, metadata$min_value, metadata$max_value)
    }
  } else {
    generator <- function(metadata, length) {
      max_distict_values <- min(length, metadata$max_distict_values)
      x <- runif(max_distict_values, metadata$min_value, metadata$max_value)
      sample(x, length, replace = TRUE)
    }
  }

  vector_template(metadata, generator, numerical_uniform_printer)
}


#' Generate a numerical vector with normal distribution
#'
#' @param mean mean of the distribution
#' @param sd standard deviation of the distribution
#' @param max_distict_values maximum number of disctict values in the vector
#'
#' @return numerical vector
#' @export
template_numerical_normal <- function(mean = 0, sd = 1, max_distict_values = NULL) {

  metadata <- list(
    mean = mean,
    sd = sd,
    max_distict_values = max_distict_values
  )

  if (is.null(max_distict_values)) {
    generator <- function(metadata, length) {
      rnorm(length, metadata$mean, metadata$sd)
    }
  } else {
    generator <- function(metadata, length) {
      max_distict_values <- min(length, metadata$max_distict_values)
      x <- rnorm(max_distict_values, metadata$mean, metadata$sd)
      sample(x, length, replace = TRUE)
    }
  }

  vector_template(metadata, generator, numerical_normal_printer)
}


#' Create a numerical column template from a source vector
#'
#' @param column numerical vector
#'
#' @return a numerical column template
dbl_template_from_column <- function(column) {

  model_size <- 100
  size <- length(column)
  block_size <- size / model_size

  # implement this part in C for speed

  # numeric columns
  column <- sort(column)

  # no last element
  x <- sapply(0:(model_size - 2), function(group) {
    
    from <- group * block_size
    to <- (group + 1) * block_size
    
    # parts
    pre_remainder  <- 1 + floor(from) - from
    post_remainder <- to - floor(to)
    before <- pre_remainder  * column[1 + floor(from)]
    after  <- post_remainder * column[1 + floor(to)]
    
    tot <- 0
    if (floor(to) > (1 + floor(from))) {
      tot <- sum(column[(2 + floor(from)):floor(to)])
    }
    
    return ((tot + before + after) / block_size)
  })

  # last element    
  from <- (model_size - 1) * block_size

  pre_remainder  <- 1 + floor(from) - from
  before <- pre_remainder  * column[1 + floor(from)]

  tot <- 0
  if (size > (1 + floor(from))) {
    tot <- sum(column[(2 + floor(from)):size])
  }

  # estimate mean at p = -0.005
  first <- 2 * x[1] - x[2]
  last <- 2 * ((tot + before) / block_size) - x[model_size - 1]

  x <- c(first, x, (tot + before) / block_size, last)
  d <- x[2:(model_size + 2)] - x[1:(model_size + 1)]

  metadata <- list(
    values = x,
    derivatives = d,
    model_size = model_size
  )

  generator <- function(metadata, length) {
    dist <- runif(length, 0, metadata$model_size)
    metadata$values[floor(dist + 0.5) + 1] +
      ((dist + 0.5) %% 1) * metadata$derivatives[floor(dist + 0.5) + 1]
  }
  
  vector_template(metadata, generator, numerical_model_printer)
}
