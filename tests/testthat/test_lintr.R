
context("code quality")

library(lintr)


test_that("Package Style", {
  lints <- with_defaults(line_length_linter = line_length_linter(120))

  code_files <- list.files(
    c("../../R", "../../tests", "../../prepare"), "R$",
    full.names = TRUE, recursive = TRUE)

  # manualy remove RcppExports file and few generated files (e.g. by codecov())
  code_files <- code_files[!(code_files %in%
    c("../../R/RcppExports.R"))]

  # Calculate lintr results for all code files
  lint_results <- lintr:::flatten_lints(lapply(code_files, function(file) {
      if (interactive()) {
          message(".", appendLF = FALSE)
      }
      lint(file, linters = lints, parse_settings = FALSE)
  }))

  # newline
  if (interactive()) {
      message()
  }

  lint_output <- NULL

  if (length(lint_results) > 0) {
    lint_results <- sapply(lint_results,
      function(lint_res) {
        paste(lint_res$filename, " (",
          lint_res$line_number, "): ", lint_res$message)
      })

    print(lint_results)
  }

  expect_true(length(lint_results) == 0,
    paste(lint_results, sep = "\n", collapse = "\n"))
})
