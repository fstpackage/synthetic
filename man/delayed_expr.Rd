% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/delayed.R
\name{delayed_expr}
\alias{delayed_expr}
\title{A wrapped expression that can be used with operators to generate new expressions}
\usage{
delayed_expr(expr, expr_str = NULL)
}
\arguments{
\item{expr}{an expression}

\item{expr_str}{text that you want to show when this expression is printed. If NULL,
the expression supplied by parameter 'expr' is used for printing.}
}
\value{
A delayed_expr object
}
\description{
A wrapped expression that can be used with operators to generate new expressions
}
\examples{
# define a delayed expression
f_delay <- function() {
  delayed_expr(size)
}

# create a new expression
y = (1 + f_delay()) == 5

# print the unevaluated expression
y

\dontrun{
# evaluation generates an error, as size is not defined
delayed_eval(y)
}

# define size and re-evaluate
size = 0.1
delayed_eval(y)

# make the expression evaluate to TRUE
size = 4
delayed_eval(y)


# custom printing text can be added to the delayed expression
f_delay <- function() {
  delayed_expr(size, "f_delay()")
}

# create a new expression
y = (1 + f_delay()) == 5

# print with custom text
y
}
