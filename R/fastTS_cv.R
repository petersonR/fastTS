#' Use k-fold CV with fastTS
#'
#' @aliases plot.fastTS_cv coef.fastTS_cv print.fastTS_cv
#'
#' @param y univariate time series outcome
#' @param X matrix of predictors (no intercept)
#' @param n_lags_max maximum number of lags to consider
#' @param gamma vector of exponent for weights
#' @param ptrain prop. to leave out for test data
#' @param cv_folds number of cross-validation folds (defaults to 10)
#' @param pf_eps penalty factors below this will be set to zero
#' @param w_endo optional pre-specified weights for endogenous terms
#' @param w_exo optional pre-specified weights for exogenous terms (see details)
#' @param ncvreg_args additional args to pass through to ncvreg
#' @param ... passed to downstream functions
#'
#' @return A list of class \code{fastTS_cv} with elements
#'
#'   \item{fit}{a fit at the best CV estimate of gamma}
#'   \item{ncvreg_args}{arguments passed to ncvreg}
#'   \item{gamma}{the (negative) exponent on the penalty weights, one
#'   for each fit}
#'   \item{n_lags_max}{the maximum number of lags}
#'   \item{y}{the time series}
#'   \item{X}{the utilized matrix of exogenous features}
#'   \item{oos_results}{results on test data using best of fits}
#'   \item{train_idx}{index of observations used in training data}
#'   \item{cve_mean}{mean CV error for each gamma}
#'   \item{best_gamma}{best gamma}
#'   \item{best_lambda}{best lambda}
#'   \item{best_pf}{best penalty factors}
#'
#' @details This function performs similarly to \code{fastTS}, except is
#'   designed for use with cross-validation. The additional need to
#'   cross-validate makes the PACF function, used in determining the weights
#'   more difficult as this must be wrapped into the CV procedure, and the PACF
#'   needs to be recomputed each time. Therefore this function takes potentially
#'   considerably longer than \code{fastTS}.
#'
#'   The benefit of this function is that the user is not dependent on AICc or BIC for
#'   model tuning, and obtains extra-sample estimates of the model error which
#'   may be useful for forecasting.
#'
#'   This function is currently experimental. It is not recommended for use at this time.
#'
#' @seealso predict.fastTS_cv
#'
#' @importFrom ncvreg ncvreg
#' @importFrom stats coef complete.cases lm logLik na.omit pacf predict ts
#' @importFrom methods is
#' @importFrom yardstick mae_vec rmse_vec rsq_trad_vec
#' @importFrom dplyr summarize
#'
#' @examples
#' data("LakeHuron")
#' fit_LH <- fastTS_cv(LakeHuron)
#' fit_LH
#' coef(fit_LH)
#' plot(fit_LH)
#'
#' @export
fastTS_cv <- function(
  y, X = NULL, n_lags_max, gamma = c(0, 2^(-2:4)),
  ptrain = 1, cv_folds = 10, pf_eps = 0.01, w_endo, w_exo,
  ncvreg_args = list(penalty = "lasso", returnX = FALSE, lambda.min = .001, nlambda = 100)) {

  n <- length(y)

  if(missing(n_lags_max))
     n_lags_max <- min(1000, n/10)

  # Parameter checks
  parameter_checks(y, X, n_lags_max, gamma, ptrain)
  if(!missing(w_endo)) {
    stopifnot("if specified, w_endo must be nonmissing" = !any(is.na(w_endo)))
    stopifnot("w_endo must be [0, Inf)" = (all(w_endo >= 0)))
  }
  if(!missing(w_exo)) {
    stopifnot("if specified, w_exo must be nonmissing" = !any(is.na(w_exo)))
    stopifnot("w_exo must be [0, Inf) or 'unpenalized'" = all(w_exo == "unpenalized") || (all(w_exo >= 0)))
  }

  train_idx <- 1:floor(n*ptrain)

  if(any(is(y) == "ts"))
    y <- as.numeric(y)

  ytest <- y[-train_idx]
  ytrain <- y[train_idx]


  ## if exogenous features supplied...
  if(!is.null(X)) {
    X <- as.matrix(X)

    if(is.null(colnames(X)))
      colnames(X) <- paste0("X", 1:ncol(X))

    if(any(is_intercept <- apply(X, 2, function(x) all(x == 1)))) {
      warning("Detected intercept; dropping")
      X <- X[,!is_intercept]
    }

    Xfull <- get_model_matrix(y, X, n_lags_max)
    Xtrain <- X[train_idx,, drop = FALSE]

    Xfulltrain <- na.omit(Xfull[train_idx,])
    Xfulltest <- Xfull[-train_idx,]

    # Use simple OLS for weights for exogenous features
    if(missing(w_exo))
      w_exo <- abs(apply(Xtrain, 2, function(x) coef(lm((ytrain ~ x)))[2]))

    if(w_exo[1] == "unpenalized")
      w_exo <- rep(Inf, ncol(Xtrain))

  } else { # otherwise...
    X <- NULL
    Xfull <- get_model_matrix(y, n_lags_max = n_lags_max)
    Xfulltrain <- na.omit(Xfull[train_idx,])
    Xfulltest <- Xfull[-train_idx,]
    w_exo <- NULL
  }

  y_cc_train <- ytrain[complete.cases(Xfull)[train_idx]]

  if(missing(w_endo))
    w_endo <- NULL

  # This should be embedded into CV procedure
  # Build CV fold IDs for each observation
  cv_fold_ids <- sample(rep(1:cv_folds, length.out = length(y_cc_train)))

  # Perform CV in a for loop
  cve_at_best_lambda <- matrix(NA, nrow = length(gamma), ncol = cv_folds)
  best_lambda_grid <- matrix(NA, nrow = length(gamma), ncol = cv_folds)

  for(i in 1:cv_folds) {
    cv_idx <- which(cv_fold_ids == i)
    cv_y <- y_cc_train
    cv_y[cv_idx] <- NA # Need this for PACF calculation
    cv_y_cc <- na.omit(cv_y)

    cv_X <- Xfulltrain[-cv_idx,]
    cv_ytest <- y_cc_train[cv_idx]
    cv_Xtest <- Xfulltrain[cv_idx,]

    if(is.null(w_endo)) {

      # Impute missing
      imputed_cv_y <- imputeTS::na_kalman(cv_y)
      pacfs <- pacf(ts(imputed_cv_y), lag.max = n_lags_max, plot = FALSE)

      w <- c(
        as.vector(abs(pacfs$acf)),
        w_exo
      )
    } else
      w <- c(w_endo, w_exo)

    ncvreg_args$X <- cv_X
    ncvreg_args$y <- cv_y_cc

    gfits <- sapply(gamma, function(g) {
      pf <- w^-g
      pf[pf < pf_eps] <- 0

      ncvreg_args$penalty.factor <- pf

      fit <- do.call(ncvreg::ncvreg, ncvreg_args)
      cv_yhat <- predict(fit, X = cv_Xtest)
      cve_lambda <- apply(cv_yhat, 2, function(x) mean((x - cv_ytest)^2))
      cve_at_best_lambda_i <- min(cve_lambda)
      c(cve_at_best_lambda_i = cve_at_best_lambda_i, best_lambda = fit$lambda[which.min(cve_lambda)])
    })

    cve_at_best_lambda[,i] <- gfits[1,]
    best_lambda_grid[,i] <- gfits[2,]

  }

  # Average CV error over folds
  cve_mean <- apply(cve_at_best_lambda, 1, mean)

  # find best CV error indices
  best_gamma <- gamma[which.min(cve_mean)]
  best_lambda <- mean(best_lambda_grid[which.min(cve_mean),])
  best_ncvar_args <- ncvreg_args
  best_ncvar_args$penalty.factor <- w^-best_gamma

  # Refitting at optimal gamma
  best_fit_cv <- do.call(ncvreg::ncvreg, best_ncvar_args)

  # Truncate lambda to be within the range of the ncvreg grid
  best_fit_lambda_trunc <- max(min(best_lambda, max(best_fit_cv$lambda)), min(best_fit_cv$lambda))

  # Compute oos predictions
  oos_results <- data.frame(rmse = NA, rsq = NA, mae = NA)
  if(ptrain < 1) {
    predictions <- data.frame(
      y = ytest,
      fc = predict(best_fit_cv, X = Xfulltest, lambda = best_fit_lambda_trunc)
    )

    oos_results <- summarize(
      predictions,
      rmse = rmse_vec(.data$y, .data$fc),
      rsq = rsq_trad_vec(.data$y, .data$fc),
      mae = mae_vec(.data$y, .data$fc),
    )
  }

  results <- list(
    fit = best_fit_cv,
    best_gamma = best_gamma,
    best_lambda = best_fit_lambda_trunc,
    ncvreg_args = ncvreg_args,
    best_pf <- w^-best_gamma,
    gamma = gamma,
    n_lags_max = n_lags_max,
    y = y, X = X, y_cc_train = y_cc_train,
    Xfulltrain = Xfulltrain,
    oos_results = oos_results,
    train_idx = train_idx,
    cve_mean = cve_mean
  )

  class(results) <- "fastTS_cv"
  results
}

#' @rdname fastTS_cv
#'
#' @param x a fastTS_cv object
#' @param log.l Should the x-axis (lambda) be logged?
#' @method plot fastTS_cv
#' @importFrom graphics abline
#' @export
#'
#' @return x invisibly
#'
plot.fastTS_cv <- function(x, log.l = TRUE, ...){

  tr_fn <- ifelse(log.l, log, I)

  plot(x$fit, log.l = log.l, ...)
  abline(v = tr_fn(
    x$best_lambda
    ))
  invisible(x)
}

#' @rdname fastTS_cv
#' @param object a fastTS_cv object
#' @param choose which criterion to use for lambda selection (AICc, BIC, or all)
#' @method coef fastTS_cv
#' @return a vector of model coefficients
#'
#' @export
coef.fastTS_cv <- function(object, ...) {
  predict(object$fit, type = "coef", lambda = object$best_lambda)
}

#' @rdname fastTS_cv
#' @param x a fastTS_cv object
#' @method print fastTS_cv
#' @returns x (invisibly)
#' @export
print.fastTS_cv <- function(x, ...) {
  gamma_summary <- data.frame(
    "PF_gamma" = x$gamma,
    best_CV = x$cve_mean,
    " " = ifelse(x$gamma == x$best_gamma, "*", " "),
    check.names = FALSE
  )

  print(gamma_summary, row.names = FALSE)

  cat("\nTest-set prediction accuracy\n")
  print(x$oos_results, row.names = TRUE)
}

#' @rdname fastTS_cv
#' @method summary fastTS_cv
#' @returns the summary object produced by ncvreg
#'   evaluated at the best tuning parameter combination
#'   (best AICc).
#' @export
summary.fastTS_cv <- function(object, ...) {
  best_fit <- object$fit
  best_gamma <- object$best_gamma
  best_lambda <- object$best_lambda

  s <- summary(best_fit, lambda = best_lambda,
               X = object$Xfulltrain, y = object$y_cc_train, ...)
  cat("Model summary at optimal CV (lambda=", round(best_lambda, 4),
      "; gamma=", round(best_gamma, 4), ")\n\n", sep = "")
  s
}

parameter_checks <- function(y, X, n_lags_max, gamma, ptrain) {
  stopifnot("y must be numeric" = is.numeric(y))
  stopifnot("n_lags_max must be numeric" = is.numeric(n_lags_max))
  stopifnot("gamma must be numeric" = is.numeric(gamma))
  stopifnot("ptrain must be numeric" = is.numeric(ptrain))
  stopifnot("X must be NULL or numeric" = is.null(X) | is.numeric(X))

  stopifnot("y cannot have missing values; run imputation first?" = !any(is.na(y)))
  stopifnot("n_lags_max must be a single number" = length(n_lags_max) == 1)
  stopifnot("n_lags_max must be (1, length(y))" = n_lags_max > 1 & n_lags_max < length(y))
  stopifnot("ptrain must be a single number" = length(ptrain) == 1)
  stopifnot("ptrain must be (0, 1]" = ptrain > 0 & ptrain <= 1)

  if(!is.null(X)) {
    stopifnot("X cannot have missing values; run imputation first?" = !any(is.na(X)))
  }

  stopifnot("not enough training data, update n_lags_max or ptrain?" = length(y)*ptrain > n_lags_max+1)

}
