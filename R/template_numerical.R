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

  if (length(column) < 10) stop("Too few rows")

  model_size <- min(1000L, length(column) - 1)
  bin_size <- (length(column) - 1) / model_size
  bins <- floor(1 + 0:model_size * bin_size)

  sorted <- sort(column)[bins]  # include first and last value
  
  fit <- splinefun(sorted, method = "hyman")
  
  nr_of_sim_points <- 100
  sim_size <- (length(bins) + 1 / bin_size - 1.0) / nr_of_sim_points
  points <- 1 - (0.5 / bin_size) + 0:nr_of_sim_points * sim_size

  metadata <- list(
    fit = fit(points)
  )

  generator <- function(metadata, length) {
    dist <- runif(length, 0, metadata$model_size)
    metadata$values[floor(dist + 0.5) + 1] +
      ((dist + 0.5) %% 1) * metadata$derivatives[floor(dist + 0.5) + 1]
  }

  vector_template(metadata, generator, numerical_model_printer)
}
