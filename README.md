
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spatialplanr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/MathMarEcol/spatialplanr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MathMarEcol/spatialplanr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

This repository is specific to the work of the [Mathematical Marine
Ecology Lab](https://mathmarecol.github.io) at the University of
Queensland. It is not intended to be used by anyone outside the MME Lab
without the permission of Professor Anthony Richardson.

This code has been written to simplify the process for running a
*prioritizr* analysis on a given region use the workflows and data of
the MME Lab. It is still a work in progress so feel free to submit pull
requests with new features and code improvements.

The code depends on `sf`, `terra`, `tidyverse`, `rnaturalearth`,
`prioritizr`, `stars`, `patchwork`.

To use this code, you will need to download and expand
`MME1DATA-Q1215/SpatialPlanning/SpatPlan_Data.zip`. To use the
`spatialplanr` package defaults, expand this file into your home
directory (e.g. `/Users/jason/SpatPlan_Data`). You can use any location
you want, but you will need to specify the location for some functions,
as per the function help. **Note:** The download is only 2GB, but the
expanded data is 35 GB in size. If you need help subsetting the data to
your region due to memory or HD space constraints, contact Jason.

If you run into memory problems, you can try increasing the amount of
the HD space gdal is allowed to use.
`Sys.setenv(GDAL_MAX_BAND_COUNT = 120000)`

## Installation

Be aware that this package is in the very early stages of development.
Functions and documentation are not complete so installing at the moment
is at your own risk. If you are still interested, you can install the
development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("https://github.com/MathMarEcol/spatialplanr")
```
