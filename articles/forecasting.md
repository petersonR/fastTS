# Forecasting with fastTS

``` r
library(fastTS)
library(tibble)
set.seed(123)
```

## Lake Huron data set

``` r

data("LakeHuron")
years <- time(LakeHuron)
fit <- fastTS(LakeHuron, n_lags_max = 3)
fit
#> An endogenous PACF-based fastTS model.
#> 
#>  PF_gamma AICc_d BIC_d
#>      0.00    *0*  0.54
#>      0.25  <0.01  0.54
#>      0.50   0.01  0.55
#>      1.00   0.05  0.28
#>      2.00   0.66   *0*
#>      4.00   4.46  0.89
#>      8.00   4.46  0.89
#>     16.00   4.46  0.89
#> 
#> AICc_d and BIC_d are the difference from the minimum; *0* is best.
#> 
#> - Best AICc model: 4 active terms
#> - Best BIC  model: 3 active terms
#> 
#> Test-set prediction accuracy (20% held-out test set)
#>           rmse       rsq       mae
#> AICc 0.7836646 0.5955089 0.6056737
#> BIC  0.7486619 0.6308355 0.6032140
```

### What does `predict` do?

Let $y_{t}$ refer to our outcome series, and ${\widehat{y}}_{t}^{(k)}$
refer to the $k$-step-ahead prediction for $y_{t}$.

The predicted value returned at any time point $t$ is the model’s
prediction for that point ${\widehat{y}}_{t}$, given the model and all
data up to $t -$`n_ahead`. This means that

- The 1-step prediction ${\widehat{y}}_{t}^{(1)}$ is computed by using
  lags of $y_{t}$ deemed important by the fitting process.

- The 2-step prediction ${\widehat{y}}_{t}^{(2)}$ is computed by using
  important lags of $y_{t}$, but replacing the first lag $y_{t - 1}$
  with ${\widehat{y}}_{t - 1}^{(1)}$.

- The 3-step prediction ${\widehat{y}}_{t}^{(3)}$ is computed by
  replacing the first lag $y_{t - 1}$ with ${\widehat{y}}_{t - 1}^{(2)}$
  and the second lag $y_{t - 2}$ with ${\widehat{y}}_{t - 2}^{(1)}$.

- And so on until the $k$-step prediction ${\widehat{y}}_{t}^{(k)}$ is
  similarly computed by replacing lags of $y_{t}$ with predicted values
  as necessary.

Here is an example with the `LakeHuron` data set.

``` r
p1 <- predict(fit, n_ahead = 1)
p7 <- predict(fit, n_ahead = 7)
predictions <- tibble(years, LakeHuron, p1, p7)
head(predictions, 10)
#> # A tibble: 10 × 4
#>    years LakeHuron    p1    p7
#>    <dbl>     <dbl> <dbl> <dbl>
#>  1  1875      580.   NA    NA 
#>  2  1876      582.   NA    NA 
#>  3  1877      581.   NA    NA 
#>  4  1878      581.  580.   NA 
#>  5  1879      580.  581.   NA 
#>  6  1880      580.  579.   NA 
#>  7  1881      580.  581.   NA 
#>  8  1882      581.  580.   NA 
#>  9  1883      581.  581.   NA 
#> 10  1884      581.  581.  579.
tail(predictions)
#> # A tibble: 6 × 4
#>   years LakeHuron    p1    p7
#>   <dbl>     <dbl> <dbl> <dbl>
#> 1  1967      578.  578.  579.
#> 2  1968      579.  579.  579.
#> 3  1969      580.  579.  579.
#> 4  1970      579.  580.  579.
#> 5  1971      580.  579.  578.
#> 6  1972      580.  580.  579.
```

- The `predict` function returns missing values for the first
  `n_lags_max` observations for 1-step ahead predictions. The prediction
  process back-fill real values when necessary for early predictions,
  but resets to NA before returning predictions.
- In 1884, the model’s 1-step prediction, the one that would be made in
  1883, is 581.1087408.
- The 7-step prediction for 1884, the one “made” in 1877, is
  579.4498549.

Note: there is a “burn-in” component to `fastTS` objects that means the
first `n_lags_max` observations are back-filled in.

### Forecasting

By default, the `predict` function does **not** produce forecasts. In
order to get forecasts, we need to set `forecast_ahead = TRUE`, which
will return forecasted values at the tail end of the returned vector.

``` r
p1 <- predict(fit, n_ahead = 1, forecast_ahead = TRUE) 
predictions <- tibble(time = c(1973), p1)


# For 7-step ahead forecasts
p7 <- predict(fit, n_ahead = 7, forecast_ahead = TRUE)
predictions <- tibble(time = c(1973:1979), p7)
predictions
#> # A tibble: 7 × 2
#>    time    p7
#>   <int> <dbl>
#> 1  1973  580.
#> 2  1974  580.
#> 3  1975  579.
#> 4  1976  579.
#> 5  1977  579.
#> 6  1978  579.
#> 7  1979  579.
```

Finally, the `return_intermediate` option allows users to collect all of
the step-ahead predictions up to $k$:

``` r
p1_p7 <- predict(fit, n_ahead = 7, return_intermediate = TRUE)

predictions <- tibble(years, LakeHuron, p1_p7)
tail(predictions)
#> # A tibble: 6 × 9
#>   years LakeHuron    p1    p2    p3    p4    p5    p6    p7
#>   <dbl>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  1967      578.  578.  578.  578.  578.  579.  579.  579.
#> 2  1968      579.  579.  578.  578.  578.  578.  579.  579.
#> 3  1969      580.  579.  579.  578.  578.  578.  578.  579.
#> 4  1970      579.  580.  579.  579.  578.  578.  578.  579.
#> 5  1971      580.  579.  580.  579.  579.  578.  578.  578.
#> 6  1972      580.  580.  579.  579.  579.  579.  579.  579.
```
