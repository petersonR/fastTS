
<!-- README.md is generated from README.Rmd. Please edit that file -->

# srlTS

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/petersonR/srlTS/branch/main/graph/badge.svg)](https://app.codecov.io/gh/petersonR/srlTS?branch=main)
[![R-CMD-check](https://github.com/petersonR/srlTS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/petersonR/srlTS/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/srlTS)](https://CRAN.R-project.org/package=srlTS)
<!-- badges: end -->

## Overview

The Sparsity-Ranked Lasso (SRL) for Time Series implemented in `srlTS`
efficiently fits long, high-frequency time series with complex
seasonality, even with a high-dimensional exogenous feature set.

Originally described in [Peterson and Cavanaugh
(2022)](https://doi.org/10.1007/s10182-021-00431-7) in the context of
variable selection with interactions and/or polynomials, *ranked
sparsity* is a philosophy of variable selection in the presence of prior
informational asymmetry.

In time series data with complex seasonality or exogenous features; see
[Peterson and Cavanaugh
(2023+)](https://doi.org/10.48550/arXiv.2211.01492), which also
describes this package in greater detail. The basic premise is to
utilize the sparsity-ranked lasso to be less skeptical of more recent
lags, and suspected seasonal relationships.

## Installation

You can install the development version of `srlTS` like so:

``` r
# install.packages("remotes")
remotes::install_github("PetersonR/srlTS")
```

Or, install from CRAN with:

``` r
install.packages("srlTS")
```

## Example

This is a basic example.

``` r
library(srlTS)

y <- cumsum(rnorm(100))
fit <- srlTS(y, gamma = c(0, .5))

fit
#>  PF_gamma best_AICc best_BIC
#>       0.0  209.9610 216.3429
#>       0.5  208.1509 214.5327
#> 
#> Test-set prediction accuracy
#>         rmse       rsq      mae
#> AIC 1.518106 0.9478941 1.286608
#> BIC 1.518106 0.9478941 1.286608
```

## Learn more

To learn more and to see this methodology in action, see:

- [Simple case studies
  vignette](https://petersonr.github.io/srlTS/articles/case_studies.html)
- [Modeling hourly ER arrival data with complex
  seasonality](https://petersonr.github.io/srlTS/articles/hourly_er_visits.html)
- [Did Denver’s 2022 ‘Zero Fare for Cleaner Air’ campaign actually
  work?](https://data-diction.com/posts/did-denver-zero-fare-policy-work/#modeling)
