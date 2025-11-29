# internal AICc function for lasso models

internal AICc function for lasso models

Internal function for obtaining oos results

Internal function for converting time series into model matrix of lags

## Usage

``` r
AICc(fit, eps = 1)

get_oos_results(fits, ytest, Xtest)

get_model_matrix(y, X = NULL, n_lags_max)
```

## Arguments

- fit:

  an object with logLik method,

- eps:

  minimum df used in computation

- fits:

  a list of fits with different tuning parameters

- ytest:

  validation data

- Xtest:

  new X data, including lags

- y:

  time series vector

- X:

  Additional exogenous features

- n_lags_max:

  Maximum number of lags to add
