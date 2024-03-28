# fastTS 1.0.1

- Update citation with now published [paper](https://doi.org/10.1177/1471082X231225307)

# fastTS 1.0.0

- Improved handling of out-of-sample error estimation
- Using `rsq_trad_vec` instead of `rsq_vec`
- Improved print method for `fastTS` objects
- Improved documentation
- Improved unit tests, ensuring method fits at least decently well on 
  a few time series example data sets
- Adding `fastTS` parametric penalty scaling functionality via `weight_type`, 
  `m`, and `r`, and `penalty_scaler` function. 
- Fixed bug(s) in prediction method for `fastTS` objects, 
  no paper conclusions were harmed in the process.
- Added forecast option to `predict.fastTS` with option `forecast_ahead`

# fastTS 0.1.2

- Renaming package to `fastTS`, and functions
- Add unit tests for `srlTS` (`fastTS`) function

# srlTS 0.1.1

- remove warning when `ptrain = 1`
- Improve `predict` functionality, documentation for forecasting
- UIHC Emergency Department arrivals data and vignette. 
- unit tests, improved coverage
- Getting started vignette
- Simple case studies vignette
- Adding minor examples to roxygen, improving documentation

# srlTS 0.1.0

- add basic building blocks, readme
- Added a `NEWS.md` file to track changes to the package.
