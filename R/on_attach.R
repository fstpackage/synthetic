#  syntheticbench - R package for benchmarking of dataset serialization
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
#  - syntheticbench R package source repository : https://github.com/fstpackage/syntheticbench


.onAttach <- function(libname, pkgname) {

  # executed when attached to search() path such as by library() or require()
  if (!interactive()) return()

  v <- packageVersion("syntheticbench")
  d <- read.dcf(system.file("DESCRIPTION", package = "syntheticbench"), fields = c("Packaged", "Built"))

  if (is.na(d[1])) {
    if (is.na(d[2])) {
      return() # neither field exists
    } else {
      d <- unlist(strsplit(d[2], split = "; "))[3]
    }
  } else {
    d <- d[1]
  }

  # version number odd => dev
  dev <- as.integer(v[1, 3]) %% 2 == 1

  packageStartupMessage("syntheticbench package v", v, if (dev) paste0(" IN DEVELOPMENT built ", d))

  # check for old version
  if (dev && (Sys.Date() - as.Date(d)) > 28)
    packageStartupMessage("\n!!! This development version of the package is rather old, please update !!!")
}
