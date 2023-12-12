
<!-- README.md is generated from README.Rmd. Please edit that file -->

# srlTS

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/petersonR/srlTS/branch/main/graph/badge.svg)](https://app.codecov.io/gh/petersonR/srlTS?branch=main)
[![R-CMD-check](https://github.com/petersonR/srlTS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/petersonR/srlTS/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/srlTS)](https://CRAN.R-project.org/package=srlTS)
<!-- badges: end -->

The goal of srlTS is to fit the sparsity-ranked lasso to time series
data.

## Installation

You can install the development version of srlTS like so:

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
#>       0.0  209.4454 216.8027
#>       0.5  207.5736 213.9554
#> 
#> Test-set prediction accuracy
#>         rmse       rsq      mae
#> AIC 1.260535 0.7849037 1.041486
#> BIC 1.260535 0.7849037 1.041486
```
