
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- <img src="logo.png" align="right" /> -->

[![Linux/OSX Build
Status](https://travis-ci.org/fstpackage/syntheticbench.svg?branch=develop)](https://travis-ci.org/fstpackage/syntheticbench)
[![Windows Build
status](https://ci.appveyor.com/api/projects/status/rng88laj6o2fj2dy?svg=true)](https://ci.appveyor.com/project/fstpackage/syntheticbench)
[![License: AGPL
v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-blue.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![codecov](https://codecov.io/gh/fstpackage/syntheticbench/branch/develop/graph/badge.svg)](https://codecov.io/gh/fstpackage/syntheticbench)

## Overview

The `syntheticbenchmark` package provides tooling to greatly symplify
benchmarking of serialization solutions such as `fst`, `arrow`,
`feather`, `fread/fwrite` or `sqlite`. By using a standardized method of
benchmarking, results become more reliable and more easy to compare.

## Features

Benchmarks performed With `syntheticbench` have the following features:

  - Each measurement of serialization speed uses a unique dataset
    (*avoid disk caching*)
  - A read is not executed immediately after a write of the same dataset
    (*avoid disk caching*)
  - All (column-) data is generated on the fly using predefined
    generators (*no need to download large test sets*)
  - A wide range of data profiles can be used for the creation of
    synthetic data (*understand dependencies on data format and
    profile*)
  - Object- en file sizes are recorded and speeds automatically
    calculated (*reproducible results*)
  - A progress bar shows percentage done and time remaining (*know when
    to go and get a cup of coffee*)
  - Only the actual serialization speed is benchmarked (*measure only
    what must be measured*)
  - Multithreaded solutions are correctly measured (*unlike some
    benchmark techniques*)

But most importantly, with the use of `syntheticbench`, complex
benchmarks are reduced to a few simple statements, increasing your
productivity and reproducibility\!

## Walkthrough: setting up a benchmark

A lot of claims are made on the performance of serializers and
databases, but the truth is that all solutions have their own strenghts
and weaknesses.

*some more text here*

Define the template of a test dataset:

``` r
library(syntheticbench)
library(fst)
library(arrow)

# generator for 'fst benchmark' dataset
generator <- table_generator(
  "fst benchmark",
  function(nr_of_rows) {
    data.frame(
      Logical = sample_logical(nr_of_rows, true_false_na_ratio = c(85, 10, 5)),
      Integer = sample_integer(nr_of_rows, max_value = 100L),
      Real    = sample_integer(nr_of_rows, 1, 10000, max_distict_values = 20) / 100,
      Factor  = as.factor(sample(labels(UScitiesD), nr_of_rows, replace = TRUE))
    )}
)
```

Define a *streamer* for `fst`:

``` r
# fst streamer
fst_streamer <- table_streamer(
  id = "fst",
  table_writer = function(x, file_name, compress) {
    if (is.null(compress)) {
      return(fst::write_fst(x, file_name))
    }
    fst::write_fst(x, file_name, compress)
  },
  table_reader = function(x) read_fst(x),
  can_select_threads = TRUE,
  variable_compression = TRUE
)
```

Do some benchmarking:

``` r
library(dplyr)

synthetic_bench() %>%
  bench_generators(generator) %>%
  bench_streamers(fst_streamer) %>%
  bench_rows(1e5) %>%
  compute()
```

Congratulations, that’s your first structured benchmark :-)

Now, let´s add a second *streamer* and allow for two different sizes of
datasets:

``` r
# parguet streamer
parguet_streamer <- table_streamer(
  id = "parguet",
  table_writer = function(x, file_name, compress) {
    arrow::write_parquet(x, file_name)
  },
  table_reader = function(x) read_parquet(x),
  can_select_threads = FALSE,
  variable_compression = FALSE
)

synthetic_bench() %>%
  bench_generators(generator) %>%
  bench_streamers(fst_streamer, parguet_streamer) %>%  # two streamers
  bench_rows(1e5) %>%
  compute()
```

As you can see, although benchmarking two solutions at different sizes
is more complex than the single solution benchmark, with
`syntheticbench` it´s just a matter of expanding some of the arguments.

Let´s add two more *streamers* and add compression settings to the mix:

``` r
# baseR streamer
rds_streamer <- table_streamer(
  id = "rds",
  table_writer = function(x, file_name, compress) {
    saveRDS(x, file_name)
  },
  table_reader = function(x) readRDS(x),
  can_select_threads = FALSE,
  variable_compression = FALSE
)


# feather streamer
feather_streamer <- table_streamer(
  id = "feather",
  table_writer = function(x, file_name, compress) {
    arrow::write_feather(x, file_name)
  },
  table_reader = function(x) read_feather(x),
  can_select_threads = FALSE,
  variable_compression = FALSE
)


synthetic_bench() %>%
  bench_generators(generator) %>%
  bench_streamers(rds_streamer, fst_streamer, parguet_streamer, feather_streamer) %>%
  bench_rows(1e7, 5e7) %>%
  bench_compression(50, 80) %>%
  compute()
```
