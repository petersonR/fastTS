#' Perform time series ranked sparsity methods
#'
#' @param y univariate time series outcome
#' @param X matrix of predictors (no intercept)
#' @param n_lags_max maximum number of lags to consider
#' @param gamma vector of exponent for weights
#' @param ptrain prop. to leave out for test data
#' @param pf_eps penalty factors below this will be set to zero
#' @param w_endo optional pre-specified weights for endogenous terms
#' @param w_exo optional pre-specified weights for exogenous terms
#' @param ncv_args additional args to pass through to ncvreg

sparseR_ts <- function(y, X = NULL, n_lags_max, gamma, ptrain = .8,
                       pf_eps = 0.01, w_endo, w_exo,
                       ncv_args = list(penalty = "lasso", returnX = FALSE, lambda.min = .001)) {
  n <- length(y)

  if(missing(n_lags_max))
     n_lags_max <- min(1000, n/10)

  if(missing(gamma))
    gamma <- c(0, .25, .5, 1, 2, 4, 8, 16)

  train_idx <- 1:floor(n*ptrain)

  ytest <- y[-train_idx]
  ytrain <- y[train_idx]

  ## SRL-pac
  ylags <- sapply(1:n_lags_max, function(i) dplyr::lag(y, i))
  colnames(ylags) <- paste0('lag', 1:n_lags_max)

  if(!is.null(X)) {
    if(is.null(colnames(X)))
      colnames(X) <- paste0("X", 1:ncol(X))
    Xtest <- X[-train_idx,]
    Xtrain <- X[train_idx,]

    Xfulltrain <- na.omit(cbind(ylags, X)[train_idx,])
    Xfulltest <- cbind(ylags, X)[-train_idx,]

    # Use simple OLS for weights for exogenous features
    if(missing(w_exo))
      w_exo <- abs(apply(Xtrain, 2, function(x) coef(lm((ytrain ~ x)))))

    if(w_exo[1] == "unpenalized")
      w_exo <- rep(Inf, ncol(Xtrain))

  } else {
    Xfulltrain <- na.omit(ylags[train_idx,])
    Xfulltest <- ylags[-train_idx,]
    w_exo <- NULL
  }

  y_cc_train <- ytrain[complete.cases(ylags)[train_idx]]

  if(missing(w_endo)) {
    pacfs <- pacf(ts(ytrain), lag.max = n_lags_max, plot = FALSE)

    w <- c(
      as.vector(abs(pacfs$acf)),
      w_exo
    )
  } else
    w <- w_endo

  pfs <- sapply(1:length(gamma), function(g) w^-gamma[g])
  pfs[pfs < pf_eps] <- 0
  pfs[is.infinite(w), 1] <- 0

  ncvreg_args$X <- Xfulltrain
  ncvreg_args$y <- y_cc_train


  srl_fits <- apply(pfs, 2, function(x) {
    ncvreg_args$penalty.factor <- x
    do.call(ncvreg::ncvreg, ncvreg_args)
  })

  best_fit_penalized_bic <- srl_fits[[which.min(apply(sapply(srl_fits, BIC), 2, min))]]
  best_fit_penalized_aicc <- srl_fits[[which.min(apply(sapply(srl_fits, AICc), 2, min))]]

  predictions <- data.frame(
    y = ytest,
    fc_srb = predict(best_fit_penalized_bic, X = Xfulltest, which = which.min(BIC(best_fit_penalized_bic))),
    fc_sra = predict(best_fit_penalized_aicc, X = Xfulltest, which = which.min(AICc(best_fit_penalized_aicc)))
  )

  oos_results_aic <- predictions %>%
    summarize(
      rmse = yardstick::rmse_vec(y, fc_sra),
      rsq = yardstick::rsq_vec(y, fc_sra),
      mae = yardstick::mae_vec(y, fc_sra),
    )

  oos_results_bic <- predictions %>%
    summarize(
      rmse = yardstick::rmse_vec(y, fc_srb),
      rsq = yardstick::rsq_vec(y, fc_srb),
      mae = yardstick::mae_vec(y, fc_srb)
    )
  b <- coef(best_fit_penalized_aicc, which = which.min(AICc(best_fit_penalized_aicc)))[-1]
  df <- data.frame(y=y_cc_train, Xfulltrain[,b!=0])
  relaxed_fit <- lm(y ~ ., data = df)

  results <- list(
    fits = srl_fits,
    ncvreg_args = ncvreg_args,
    gamma = gamma,
    oos_results = rbind("AIC" = oos_results_aic, "BIC" = oos_results_bic),
    relaxed_fit = broom::tidy(relaxed_fit)
  )

  class(results) <- "sparseR_ts"
  results
}

# try1 <- sparseR_ts(y2, n_lags_max = 24)

plot.sparseR_ts <- function(x, log.l = TRUE, ...){

  best_fit_penalized_bic <- x$fits[[which.min(apply(sapply(x$fits, BIC), 2, min))]]
  best_fit_penalized_aicc <- x$fits[[which.min(apply(sapply(x$fits, AICc), 2, min))]]

  tr_fn <- ifelse(log.l, log, I)

  plot(best_fit_penalized_aicc, log.l = log.l, ...)
  title("PFs selected by AICc")
  abline(v = tr_fn(best_fit_penalized_aicc$lambda[which.min(AICc(best_fit_penalized_aicc))]))
  abline(v = tr_fn(best_fit_penalized_aicc$lambda[which.min(BIC(best_fit_penalized_aicc))]))

  plot(best_fit_penalized_bic, log.l = log.l, ...)
  title("PFs selected by BIC")
  abline(v = tr_fn(best_fit_penalized_bic$lambda[which.min(AICc(best_fit_penalized_bic))]))
  abline(v = tr_fn(best_fit_penalized_bic$lambda[which.min(BIC(best_fit_penalized_bic))]))
  invisible(x)
}

# plot(try1)


coef.sparseR_ts <- function(object, choose = c("AICc", "BIC", "all"), ...) {

  choose <- match.arg(choose)

  if(choose != "AICc")
    stop("non-AICc options not yet supported")

  best_fit_penalized_aicc <- object$fits[[which.min(apply(sapply(object$fits, AICc), 2, min))]]
  predict(best_fit_penalized_aicc,  type = "coef", which = which.min(AICc(best_fit_penalized_aicc)))
}

# coef(try1)

print.sparseR_ts <- function(x, ...) {
  gamma_summary <- data.frame(
    "PF_gamma" = x$gamma,
    best_AICc = apply(sapply(x$fits, AICc), 2, min),
    best_BIC = apply(sapply(x$fits, BIC), 2, min)
  )

  print(gamma_summary, row.names = FALSE)

  cat("\nTest-set prediction accuracy\n")
  print(x$oos_results, row.names = TRUE)
}

# print(try1)
