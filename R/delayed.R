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



#' A wrapped expression that can be used with operators to generate new expressions 
#'
#' @param expr an expression
#'
#' @return A delayed_expr object
#' @export
#'
#' @examples
#' # define a delayed expression
#' f_delay <- function() {
#'   delayed_expr(size)
#' }
#'
#' # create a new expression
#' y = (1 + f_delay()) == 5
#'
#' # generates an error, as size is not defined
#' delayed_eval(y)
#'
#' # calculates expression y using size
#' size = 0.1
#' delayed_eval(y)
delayed_expr <- function(expr) {
  x <- list(expr = enexpr(expr))
  class(x) <- "delayed_expr"
  x
}


#' Evaluate a delayed expression
#'
#' @param delayed_expr delayed expression generated with method `delayed_expr()``
#'
#' @return the evaluated expression
#' @export
#'
#' @examples
#' # define a delayed expression
#' x <- delayed_expr(size * 5 + 1)
#'
#' # create a new expression
#' y = 1 + x + 5 * x - x
#'
#' # generates an error, as size is not defined
#' delayed_eval(y)
#'
#' # calculates expression y using size
#' size = 0.1
#' delayed_eval(y)
delayed_eval <- function(delayed_expr) {
  
  # test appropriate class
  if (class(delayed_expr) != "delayed_expr") {
    stop("incorrect argument `delayed_expr`, please use method delayed_expr() ",
      "to generate an object of the appropriate class.")
  }
  
  eval(delayed_expr$expr)
}


print.delayed_expr <- function(x, ...) {
  print(x$expr)
}


delayed_operation <- function(f, x, y) {

  # both x and y are delayed expressions
  if (class(x) == "delayed_expr" && class(y) == "delayed_expr") {
    res <- list(expr = substitute(f(x, y), list(f = f, x = x$expr, y = y$expr)))
    class(res) <- "delayed_expr"
    return(res)
  }
  
  # x is a delayed expression
  if (class(x) == "delayed_expr") {
    res <- list(expr = substitute(f(x, y), list(f = f, x = x$expr, y = y)))
    class(res) <- "delayed_expr"
    return(res)
  }
  
  # y is a delayed expression
  res <- list(expr = substitute(f(x, y), list(f = f, x = x, y = y$expr)))
  class(res) <- "delayed_expr"
  res
}


# operator overrides

`+.delayed_expr` <- function(x, y) {
  delayed_operation(`+`, x, y)
}


`-.delayed_expr` <- function(x, y) {
  delayed_operation(`-`, x, y)
}


`*.delayed_expr` <- function(x, y) {
  delayed_operation(`*`, x, y)
}


`/.delayed_expr` <- function(x, y) {
  delayed_operation(`/`, x, y)
}


`|.delayed_expr` <- function(x, y) {
  delayed_operation(`|`, x, y)
}


`&.delayed_expr` <- function(x, y) {
  delayed_operation(`&`, x, y)
}


`&&.delayed_expr` <- function(x, y) {
  delayed_operation(`&&`, x, y)
}


`||.delayed_expr` <- function(x, y) {
  delayed_operation(`||`, x, y)
}


`==.delayed_expr` <- function(x, y) {
  delayed_operation(`==`, x, y)
}
