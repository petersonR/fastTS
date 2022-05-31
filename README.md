
<!-- README.md is generated from README.Rmd. Please edit that file -->

# srlTS

<!-- badges: start -->

[![R-CMD-check](https://github.com/petersonR/srlTS/workflows/R-CMD-check/badge.svg)](https://github.com/petersonR/srlTS/actions)
[![Codecov test
coverage](https://codecov.io/gh/petersonR/srlTS/branch/main/graph/badge.svg)](https://app.codecov.io/gh/petersonR/srlTS?branch=main)
<!-- badges: end -->

The goal of srlTS is to fit the sparsity-ranked lasso to time series
data.

## Installation

You can install the development version of srlTS like so:

``` r
# install.packages("remotes")
remotes::install_github("PetersonR/srlTS")
```

## Example

This is a basic example.

``` r
library(srlTS)

y <- cumsum(rnorm(100))
fit <- srlTS(y, gamma = c(0, .5))

fit
#>  PF_gamma best_AICc best_BIC
#>       0.0  188.4958 194.8777
#>       0.5  188.1214 194.5033
#> 
#> Test-set prediction accuracy
#>         rmse      rsq       mae
#> AIC 1.005835 0.679604 0.8435354
#> BIC 1.005835 0.679604 0.8435354
```
