#  synthetic - R package for synthetic dataset creation and serialization benchmarks
#
#  Copyright (C) 2019-present, Mark AJ Klik
#
#  This file is part of the lazyvec R package.
#
#  The lazyvec R package is free software: you can redistribute it and/or modify it
#  under the terms of the GNU Affero General Public License version 3 as
#  published by the Free Software Foundation.
#
#  The lazyvec R package is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
#  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
#  for more details.
#
#  You should have received a copy of the GNU Affero General Public License along
#  with the lazyvec R package. If not, see <http://www.gnu.org/licenses/>.
#
#  You can contact the author at:
#  - synthetic R package source repository : https://github.com/fstpackage/synthetic


#' @useDynLib synthetic, .registration = TRUE
#' @importFrom crayon cyan italic red
#' @importFrom data.table data.table fread fwrite rbindlist setDT setDTthreads :=
#' @importFrom dplyr %>% as_tibble collect mutate select
#' @importFrom fst read_fst threads_fst write_fst
#' @importFrom microbenchmark microbenchmark
#' @importFrom progress progress_bar
#' @importFrom qs qread qsave
#' @importFrom rlang enexpr
#' @importFrom stats rnorm runif
#' @importFrom stringi stri_rand_strings stri_rand_lipsum
#' @importFrom utils head packageVersion object.size capture.output
#' @importFrom vroom vroom vroom_write
#' @importFrom Rcpp evalCpp
NULL


#' @export
dplyr::`%>%`


#' @export
dplyr::collect
