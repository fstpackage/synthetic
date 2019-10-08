
require(testthat)
require(fst)
require(arrow)


context("synth bench class")


# defines streamers for fst, feather, parguet and rds
source("streamers.R")

# defines several generators
source("generators.R")
