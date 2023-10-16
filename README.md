
<!-- README.md is generated from README.Rmd. Please edit that file -->

# proteoME

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/TomChupan/proteoME/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/TomChupan/proteoME/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This package will serve as a useful tool when working with proteomics
data. Its main feature is a shiny app that allows you to:

- import your dataset
- deal with missing values
- normalize data
- aggregate data
- analyze data (both univariate and multivariate analysis)

## Installation

You can install the development version of proteoME like so:

``` r
library(devtools)
install_github("TomChupan/proteoME")
```

## Run the app

You can run the app locally as below:

``` r
library(proteoME)
proteoME::run_app()
```
