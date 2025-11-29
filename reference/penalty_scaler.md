# Penalty Scaling Function for parametric penalty weights

Penalty Scaling Function for parametric penalty weights

## Usage

``` r
penalty_scaler(lag, m, r, plot = TRUE, log = TRUE)
```

## Arguments

- lag:

  a vector of lags for which to calculate the penalty function

- m:

  a vector of seasonality modes

- r:

  a vector of dim (m + 1) for the factor penalties on c(m, time)

- plot:

  logical; whether to plot the penalty function

- log:

  logical; whether to return the log of the penalty function
