
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

  best_fit_penalized_bic <- fits[[which.min(sapply(lapply(fits, BIC), min))]]
  best_fit_penalized_aicc <- fits[[which.min(sapply(lapply(fits, AICc), min))]]

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

#' Penalty Scaling Function for parametric penalty weights
#' @rdname penalty_scaler
#'
#' @param lag  a vector of lags for which to calculate the penalty function
#' @param m  a vector of seasonality modes
#' @param r  a vector of dim (m + 1) for the factor penalties on c(m, time)
#' @param plot  logical; whether to plot the penalty function
#' @param log  logical; whether to return the log of the penalty function
#'
#' @importFrom graphics lines
#'
#' @export
penalty_scaler <- function(lag, m, r, plot = TRUE, log = TRUE) {

  stopifnot("length(m) must match length(r) - 1" = length(m) == length(r) - 1)
  stopifnot("if specified, m must be a vector of positive integers" = all(m > 0))

  s <- NULL
  if(length(m))
    s <- sapply(1:length(m), function(i) -cos(2*pi*lag/m[i] + r[i]) * r[i])

  s <- cbind(s, r[length(r)] * lag)
  pen_vals <- apply(s, 1, sum)
  if(!log) pen_vals <- exp(pen_vals)
  if(plot) {
    plot(lag, pen_vals, type = "l", ylim = range(s, pen_vals), ylab = "log penalty factor", lwd = 3)
    pp <- sapply(1:length(r), function(i) lines(lag, s[,i], col = i+1))
  }

  pen_vals
}

