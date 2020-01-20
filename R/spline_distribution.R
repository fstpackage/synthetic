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
#' Random numbers are taken from a distribution which is defined by control points. The control
#' points should be an ordered vector of values which are asumed to be the vertical component of
#' a curve in the range [-delta, 1 + delta]. So the first control point is the value at `x = -delta` and
#' the last control point contains the value at `1 + delta`. The other control points have an `x` value
#' located on equally spaced locations between these two `x` values.
#' Random draws from the distribution will be done by generating a random value between 0 and 1 and
#' simulating the corresponding value a modelled by the control points. For modelling, splines are used.
#'
#' @param n number of observations
#' @param control_points values describing the ordered distribution
#' @param seed single numeric to specify the seed
#' @param delta Determines the range [-delta, 1 + delta] for the `x` values of the control points.
#'
#' @return a vector of random numbers generated from the spline interpolated custom distribution.
#' @export
rspline <- function(n, control_points, seed, delta = 0.0) {
  random_spline(as.double(control_points), as.integer(n), as.double(seed[1]), as.double(delta))
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
