
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fastTS

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/petersonR/fastTS/branch/main/graph/badge.svg)](https://app.codecov.io/gh/petersonR/fastTS?branch=main)
[![R-CMD-check](https://github.com/petersonR/fastTS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/petersonR/fastTS/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/fastTS)](https://CRAN.R-project.org/package=fastTS)
<!-- badges: end -->

## Overview

The `fastTS` package efficiently fits long, high-frequency time series
with complex seasonality, even with a high-dimensional exogenous feature
set. It implements the sparsity-ranked lasso (and similar methods) for
time series data.

Originally described in [Peterson and Cavanaugh
(2022)](https://doi.org/10.1007/s10182-021-00431-7) in the context of
variable selection with interactions and/or polynomials, *ranked
sparsity* is a philosophy of variable selection in the presence of prior
informational asymmetry.

In time series data with complex seasonality or exogenous features; see
[Peterson and Cavanaugh
(2023+)](https://doi.org/10.48550/arXiv.2211.01492), which also
describes this package in greater detail. The basic premise is to
utilize the sparsity-ranked lasso (or similar) to be less skeptical of
more recent lags, and suspected seasonal relationships.

## Installation

You can install the development version of `fastTS` like so:

``` r
# install.packages("remotes")
remotes::install_github("PetersonR/fastTS")
```

Or, install from CRAN with:

``` r
install.packages("fastTS")
```

## Example

This is a basic example with the [sunspot monthly
series](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/sunspot.month.html).

``` r
library(fastTS)

data("sunspot.month")
fit <- fastTS(sunspot.month)

fit
#> An endogenous PACF-based fastTS model.
#> 
#>  PF_gamma AICc_d  BIC_d
#>      0.00  24.92  38.93
#>      0.25   7.88    *0*
#>      0.50    *0*   0.48
#>      1.00  69.15   35.7
#>      2.00 221.33 131.01
#>      4.00 434.49 332.77
#>      8.00 434.49 332.77
#>     16.00 434.49 332.77
#> 
#> AICc_d and BIC_d are the difference from the minimum; *0* is best.
#> 
#> - Best AICc model: 23 active terms
#> - Best BIC  model: 14 active terms
#> 
#> Test-set prediction accuracy (20% held-out test set)
#>          rmse       rsq      mae
#> AICc 15.94153 0.8920102 11.85384
#> BIC  16.04978 0.8905385 11.99382
```

## Learn more

To learn more and to see this methodology in action, see:

- [Simple case studies
  vignette](https://petersonr.github.io/fastTS/articles/case_studies.html)
- [Modeling hourly ER arrival data with complex
  seasonality](https://petersonr.github.io/fastTS/articles/hourly_er_visits.html)
- [Did Denver’s 2022 ‘Zero Fare for Cleaner Air’ campaign actually
  work?](https://data-diction.com/posts/did-denver-zero-fare-policy-work/#modeling)
