# Predict function for fastTS object

Predict function for fastTS object

## Usage

``` r
# S3 method for class 'fastTS'
predict(
  object,
  n_ahead = 1,
  X_test,
  y_test,
  cumulative = FALSE,
  forecast_ahead = FALSE,
  return_intermediate = FALSE,
  ...
)
```

## Arguments

- object:

  an fastTS object

- n_ahead:

  the look-ahead period for predictions

- X_test:

  a matrix exogenous features for future predictions (optional)

- y_test:

  the test series for future predictions (optional)

- cumulative:

  cumulative (rolling) sums of 1-, 2-, 3-, ..., k-step-ahead
  predictions.

- forecast_ahead:

  returns forecasted values for end of training series

- return_intermediate:

  if TRUE, returns the intermediate predictions between the 1st and
  n_ahead predictions, as data frame.

- ...:

  currently unused

## Value

a vector of predictions, or a matrix of 1- through n_ahead predictions.

## Details

The \`y_test\` argument must be supplied if predictions are desired or
if \`n_ahead\` \< \`nrow(X_test)\`. This is because in order to obtain
1-step forecast for, say, the 10th observation in the test data set, the
9th observation of \`y_test\` is required.

Forecasts for the first \`n_ahead\` observations after the training set
can be obtained by setting \`forecast_ahead\` to TRUE, which will return
the forecasted values at the end of the training data. it produces the
1-step-ahead prediction, the 2-step-ahead prediction, ... through the
\`n_ahead\`-step prediction. The \`cumulative\` argument is similar but
will return the cumulative (rolling) sums of 1-, 2-, 3=, ...,
\`n_ahead\`-step-ahead predictions.

## Examples

``` r
data("LakeHuron")
fit_LH <- fastTS(LakeHuron)
predict(fit_LH)
#>  [1]       NA       NA       NA       NA       NA       NA       NA       NA
#>  [9]       NA 581.0125 580.7219 580.9960 581.1889 580.5357 580.1159 579.7550
#> [17] 579.7761 578.8827 579.2512 579.5325 579.4836 578.1434 578.5270 579.3470
#> [25] 578.8931 579.3384 578.6304 579.4781 578.8051 579.0307 579.8600 579.5126
#> [33] 579.5211 579.7656 579.7974 579.0669 578.6431 578.2998 578.9424 579.5938
#> [41] 578.5703 578.1132 579.8013 579.8827 579.7640 579.2026 579.2054 578.5818
#> [49] 579.0279 577.9377 578.0709 576.9041 577.3496 578.3401 578.7360 580.6749
#> [57] 578.6937 577.2520 577.5200 577.4140 576.5386 577.5373 577.1386 577.2966
#> [65] 578.2541 578.2710 577.5091 577.5881 578.9097 579.6057 578.6401 579.3499
#> [73] 579.1059 579.3386 578.9476 577.8435 578.5245 580.0310 580.5006 579.7886
#> [81] 579.6993 579.4481 578.5976 578.2988 577.3532 577.6201 579.6514 577.7694
#> [89] 578.1514 577.0373 576.4356 577.6168 578.0205 578.5252 578.5060 579.9080
#> [97] 578.8826 579.9434
```
