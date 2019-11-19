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
#' @param expr_str text that you want to show when this expression is printed. If NULL,
#' the expression supplied by parameter 'expr' is used for printing.
#'
#' @return A delayed_expr object
#' @export
#'
#' @examples
#' \dontrun{
#' # define a delayed expression
#' f_delay <- function() {
#'   delayed_expr(size)
#' }
#'
#' # create a new expression
#' y = (1 + f_delay()) == 5
#'
#' # print the unevaluated expression
#' y
#'
#' # evaluation generates an error, as size is not defined
#' delayed_eval(y)
#'
#' # define size and re-evaluate
#' size = 0.1
#' delayed_eval(y)
#'
#' # make the expression evaluate to TRUE
#' size = 4
#' delayed_eval(y)
#'
#'
#' # custom printing text can be added to the delayed expression
#' f_delay <- function() {
#'   delayed_expr(size, "f_delay()")
#' }
#'
#' # create a new expression
#' y = (1 + f_delay()) == 5
#'
#' # print with custom text
#' y
#' }
delayed_expr <- function(expr, expr_str = NULL) {
  x <- list(
    expr = enexpr(expr),
    expr_str = ifelse(is.null(expr_str), capture.output(print(enexpr(expr))), expr_str)
  )

  class(x) <- "delayed_expr"
  x
}


#' Textual representation of a delayed expression
#'
#' This is a convenience method to use when the delayed expression needs to be converted to
#' a string, for example in the print function of a vector template that allows for methods
#' like nr_of_rows().
#' @param delayed_expr delayed expression such as that generated with nr_of_rows()
#'
#' @return well formatted string representation of the delayed expression
#' @export
#'
#' @examples
#' delayed_to_str(nr_of_rows())
delayed_to_str <- function(delayed_expr) {
  if (class(delayed_expr) == "delayed_expr") return(delayed_expr$expr_str)
  as.character(delayed_expr)
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

  eval(delayed_expr$expr, parent.frame())
}


#' @export
print.delayed_expr <- function(x, ...) {
  print(x$expr_str)
}


delayed_operation <- function(f, x, y, operator_str) {

  # both x and y are delayed expressions
  if (class(x) == "delayed_expr" && class(y) == "delayed_expr") {
    res <- list(
      expr = substitute(f(x, y), list(f = f, x = x$expr, y = y$expr)),
      expr_str = paste0("(", x$expr_str, " ", operator_str, " ", y$expr_str, ")")
    )
    class(res) <- "delayed_expr"
    return(res)
  }

  # x is a delayed expression
  if (class(x) == "delayed_expr") {
    res <- list(
      expr = substitute(f(x, y), list(f = f, x = x$expr, y = y)),
      expr_str = paste0("(", x$expr_str, " ", operator_str, " ", y, ")")
    )
    class(res) <- "delayed_expr"
    return(res)
  }

  # y is a delayed expression
  res <- list(
    expr = substitute(f(x, y), list(f = f, x = x, y = y$expr)),
    expr_str = paste0("(", x, " ", operator_str, " ", y$expr_str, ")")
  )
  class(res) <- "delayed_expr"
  res
}


# operator overrides

#' @export
`+.delayed_expr` <- function(x, y) {
  delayed_operation(`+`, x, y, "+")
}


#' @export
`-.delayed_expr` <- function(x, y) {
  delayed_operation(`-`, x, y, "-")
}


#' @export
`*.delayed_expr` <- function(x, y) {
  delayed_operation(`*`, x, y, "*")
}


#' @export
`/.delayed_expr` <- function(x, y) {
  delayed_operation(`/`, x, y, "/")
}


#' @export
`|.delayed_expr` <- function(x, y) {
  delayed_operation(`|`, x, y, "|")
}


#' @export
`&.delayed_expr` <- function(x, y) {  # nolint
  delayed_operation(`&`, x, y, "&")
}


#' @export
`&&.delayed_expr` <- function(x, y) {  # nolint
  delayed_operation(`&&`, x, y, "&&")
}


#' @export
`||.delayed_expr` <- function(x, y) {
  delayed_operation(`||`, x, y, "||")
}


#' @export
`==.delayed_expr` <- function(x, y) {
  delayed_operation(`==`, x, y, "==")
}
