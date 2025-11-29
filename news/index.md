# Changelog

## fastTS 1.0.3

- Fixing issue [\#4](https://github.com/petersonR/fastTS/issues/4)

## fastTS 1.0.2

CRAN release: 2024-12-01

- Fixed some tests that were very slightly sensitive to OS and were
  causing errors in R CMD CHECK on CRAN.

## fastTS 1.0.1

CRAN release: 2024-03-28

- Update citation with now published
  [paper](https://doi.org/10.1177/1471082X231225307)

## fastTS 1.0.0

CRAN release: 2024-03-07

- Improved handling of out-of-sample error estimation
- Using `rsq_trad_vec` instead of `rsq_vec`
- Improved print method for `fastTS` objects
- Improved documentation
- Improved unit tests, ensuring method fits at least decently well on a
  few time series example data sets
- Adding `fastTS` parametric penalty scaling functionality via
  `weight_type`, `m`, and `r`, and `penalty_scaler` function.
- Fixed bug(s) in prediction method for `fastTS` objects, no paper
  conclusions were harmed in the process.
- Added forecast option to `predict.fastTS` with option `forecast_ahead`

## fastTS 0.1.2

CRAN release: 2024-02-07

- Renaming package to `fastTS`, and functions
- Add unit tests for `srlTS` (`fastTS`) function
