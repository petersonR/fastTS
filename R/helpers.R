## AICc function for lasso models
AICc <- function(fit, eps = 1) {
  ll <- logLik(fit)
  k <- attr(ll, "df")
  n <- attr(ll, "n")
  k_star <- pmin(k, n - eps - 1)
  AIC(fit) + (2 * k^2 + 2*k) / (n - k_star - 1)
}

#' @importFrom dplyr summarize
#' @importFrom yardstick rmse_vec rsq_vec mae_vec
#' @importFrom rlang .data
get_oos_results <- function(fits, ytest, Xtest) {

  best_fit_penalized_bic <- fits[[which.min(apply(sapply(fits, BIC), 2, min))]]
  best_fit_penalized_aicc <- fits[[which.min(apply(sapply(fits, AICc), 2, min))]]

  predictions <- data.frame(
    y = ytest,
    fc_srb = predict(best_fit_penalized_bic, X = Xtest, which = which.min(BIC(best_fit_penalized_bic))),
    fc_sra = predict(best_fit_penalized_aicc, X = Xtest, which = which.min(AICc(best_fit_penalized_aicc)))
  )

  oos_results_aic <- predictions %>%
    summarize(
      rmse = rmse_vec(.data$y, .data$fc_sra),
      rsq = rsq_vec(.data$y, .data$fc_sra),
      mae = mae_vec(.data$y, .data$fc_sra),
    )

  oos_results_bic <- predictions %>%
    summarize(
      rmse = rmse_vec(.data$y, .data$fc_srb),
      rsq = rsq_vec(.data$y, .data$fc_srb),
      mae = mae_vec(.data$y, .data$fc_srb)
    )

  oos_results <- rbind("AIC" = oos_results_aic, "BIC" = oos_results_bic)
}
