# fastTS

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

This package implements such methods for fast fitting of time series
data with complex seasonality or exogenous features. More information is
included in [Peterson and Cavanaugh
(2024)](https://doi.org/10.1177/1471082X231225307). The basic premise is
to utilize the sparsity-ranked lasso (or similar) to be less skeptical
of more recent lags, and suspected seasonal relationships.

Please cite `fastTS` as:

Peterson R. A. & Cavanaugh J. E. (2024). Fast, effective, and coherent
time series modelling using the sparsity-ranked lasso. *Statistical
Modelling*. <doi:10.1177/1471082X231225307>

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
#>      0.00  18.62  26.13
#>      0.25   5.77    3.5
#>      0.50    *0*    *0*
#>      1.00  55.68  30.84
#>      2.00 234.71 146.86
#>      4.00 460.98 361.64
#>      8.00 460.98 361.64
#>     16.00 460.98 361.64
#> 
#> AICc_d and BIC_d are the difference from the minimum; *0* is best.
#> 
#> - Best AICc model: 24 active terms
#> - Best BIC  model: 18 active terms
#> 
#> Test-set prediction accuracy (20% held-out test set)
#>          rmse       rsq      mae
#> AICc 22.04138 0.8969860 16.11950
#> BIC  22.05650 0.8968446 16.17492
```

## Learn more

To learn more and to see this methodology in action, see:

- [Simple case studies
  vignette](https://petersonr.github.io/fastTS/articles/case_studies.html)
- [Modeling hourly ER arrival data with complex
  seasonality](https://petersonr.github.io/fastTS/articles/hourly_er_visits.html)
- [Did Denver’s 2022 ‘Zero Fare for Cleaner Air’ campaign actually
  work?](https://data-diction.com/posts/did-denver-zero-fare-policy-work/#modeling)
