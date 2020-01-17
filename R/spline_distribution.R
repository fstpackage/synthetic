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

#' Random number generator with custom distribution
#'
#' Random numbers are taken from a distribution which is defined by control points.
#'
#' @param n number of observations
#' @param control_points values describing the ordered distribution
#' @param seed single numeric to specify the seed
#'
#' @return a vector of random numbers generated from the spline interpolated custom distribution.
#' @export
rspline <- function(n, control_points, seed) {
  random_spline(as.double(control_points), as.integer(n), as.double(seed[1]))
}


#' Random number generator with custom distribution
#'
#' Random numbers are taken from a distribution which is defined by control points.
#'
#' @param control_points values describing the ordered distribution
#' @param values values in the distribution to simulate
#'
#' @return a vector of numbers generated from the spline interpolated custom distribution.
#' @export
sspline <- function(control_points, values) {
  simulate_spline(as.double(control_points), as.double(values))
}
