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


combine_text <- function(y, nchar1, nchar2, size) {
  v <- y[Length == nchar1, Text] %>%
    sample(size, replace = TRUE)
  
  w <- y[Length == nchar2, Text] %>%
    sample(size, replace = TRUE)
  
  paste0(v, w)
}


lorum_dictionary <- function() {

  x <- stri_rand_lipsum(1000000) %>%
    strsplit("[ .,?!]") %>%
    unlist(use.names = FALSE) %>%
    tolower()

  x <- x[x != ""] %>%
    unique() %>%
    c(letters)

  v <- c("a", "e", "i", "o", "u", "y")
  w <- letters[!(letters %in% c("a", "e", "i", "o", "u", "y"))]
  
  l2 <- unique(
    paste0(
      sample(v, 1000, replace = TRUE),
      sample(w, 1000, replace = TRUE))
    )

  l3 <- unique(
    paste0(
      sample(w, 1000, replace = TRUE),
      sample(v, 1000, replace = TRUE),
      sample(w, 1000, replace = TRUE))
  )
  
  s4 <- unique(
    paste0(
      "s",
      sample(w, 100, replace = TRUE),
      sample(v, 100, replace = TRUE),
      sample(w, 100, replace = TRUE))
  )

  l4 <- unique(
    paste0(
      sample(v, 1000, replace = TRUE),
      sample(w, 1000, replace = TRUE),
      sample(v, 1000, replace = TRUE),
      sample(w, 1000, replace = TRUE))
  )

  x <- unique(c(x, l2, l3, s4, l4))

  y <- data.table(Text = x, Length = nchar(x))

  x <- c(x,
        combine_text(y, 2, 3, 990),
        combine_text(y, 3, 2, 990)
      ) %>%
    unique()

  x <- c(x,
      combine_text(y, 2, 4, 665),
      combine_text(y, 4, 2, 665),
      combine_text(y, 3, 3, 665)
    ) %>%
    unique()

  x <- c(x,
      combine_text(y, 3, 4, 990),
      combine_text(y, 4, 3, 990)
    ) %>%
    unique()

  c(x,
     combine_text(y, 4, 4, 1980)
    ) %>%
    unique()
}
