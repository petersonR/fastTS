
#' internal AICc function for lasso models
#'
#' @param fit an object with logLik method,
#' @param eps minimum df used in computation
#'
#' @rdname internal
AICc <- function(fit, eps = 1) {
  ll <- logLik(fit)
  k <- attr(ll, "df")
  n <- attr(ll, "n")
  k_star <- pmin(k, n - eps - 1)
  AIC(fit) + (2 * k^2 + 2*k) / (n - k_star - 1)
}

#' Internal function for obtaining oos results
#' @rdname internal
#' @param fits a list of fits with different tuning parameters
#' @param ytest validation data
#' @param Xtest new X data, including lags
#'
#' @importFrom dplyr summarize
#' @importFrom yardstick rmse_vec rsq_trad_vec mae_vec
#' @importFrom rlang .data
get_oos_results <- function(fits, ytest, Xtest) {

  best_fit_penalized_bic <- fits[[which.min(apply(sapply(fits, BIC), 2, min))]]
  best_fit_penalized_aicc <- fits[[which.min(apply(sapply(fits, AICc), 2, min))]]

  predictions <- data.frame(
    y = ytest,
    fc_srb = predict(best_fit_penalized_bic, X = Xtest,
                     which = which.min(BIC(best_fit_penalized_bic))
    ),
    fc_sra = predict(best_fit_penalized_aicc, X = Xtest,
                     which = which.min(AICc(best_fit_penalized_aicc)))
  )

  oos_results_aic <- summarize(
      predictions,
      rmse = rmse_vec(.data$y, .data$fc_sra),
      rsq = rsq_trad_vec(.data$y, .data$fc_sra),
      mae = mae_vec(.data$y, .data$fc_sra),
    )

  oos_results_bic <- summarize(
      predictions,
      rmse = rmse_vec(.data$y, .data$fc_srb),
      rsq = rsq_trad_vec(.data$y, .data$fc_srb),
      mae = mae_vec(.data$y, .data$fc_srb)
    )

  oos_results <- rbind("AICc" = oos_results_aic, "BIC" = oos_results_bic)
}

#' Internal function for converting time series into model matrix of lags
#' @rdname internal
#'
#' @param y time series vector
#' @param X Additional exogenous features
#' @param n_lags_max Maximum number of lags to add
#'
#' @importFrom dplyr lag
get_model_matrix <- function(y, X = NULL, n_lags_max) {

  ylags <- sapply(1:n_lags_max, function(i) lag(y, i))
  colnames(ylags) <- paste0('lag', 1:n_lags_max)

  cbind(ylags, X)
}
