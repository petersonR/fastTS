#'  Fast time series modeling with ranked sparsity
#'
#' @description
#' Uses penalized regression to quickly fit time series models with
#' potentially complex seasonal patterns and exogenous variables.
#' Based on methods described in Peterson & Cavanaugh (2024).
#'
#' @aliases plot.fastTS coef.fastTS print.fastTS
#'
#' @param y univariate time series outcome
#' @param X matrix of predictors (no intercept)
#' @param n_lags_max maximum number of lags to consider
#' @param gamma vector of exponent for weights
#' @param ptrain prop. to leave out for test data
#' @param pf_eps penalty factors below this will be set to zero
#' @param w_endo optional pre-specified weights for endogenous terms
#' @param w_exo optional pre-specified weights for exogenous terms (details)
#' @param weight_type type of weights to use for endogenous terms
#' @param m mode(s) for seasonal lags (used if weight_type = "parametric")
#' @param r penalty factors for seasonal + local scaling functions (used if
#'   weight_type = "parametric")
#' @param plot logical; whether to plot the penalty functions
#' @param ncvreg_args additional args to pass through to ncvreg
#' @param ... passed to downstream functions
#'
#' @return A list of class \code{slrTS} with elements
#'
#'   \item{fits}{a list of lasso fits} \item{ncvreg_args}{arguments passed to
#'   ncvreg}
#'   \item{gamma}{the (negative) exponent on the penalty weights, one
#'   for each fit}
#'   \item{n_lags_max}{the maximum number of lags} \item{y}{the time series}
#'   \item{X}{the utilized matrix of exogenous features}
#'   \item{oos_results}{results on test data using best of fits}
#'   \item{train_idx}{index of observations used in training data}
#'   \item{weight_type}{the type of weights used for endogenous terms}
#'   \item{m}{the mode(s) for seasonal lags (used if weight_type =
#'   "parametric")} \item{r}{penalty factors for seasonal + local scaling
#'   functions}
#'
#' @details The default weights for exogenous features will be chosen based on a
#'   similar approach to the adaptive lasso (using bivariate OLS estimates). For
#'   lower dimensional X, it's advised to set \code{w_exo="unpenalized"},
#'   because this allows for statistical inference on exogenous variable
#'   coefficients via the \code{summary} function.
#'
#'   By default, a seasonal frequency \code{m} must not be specified and the
#'   PACF is used to estimate the weights for endogenous terms. A parametric
#'   version is also available, which allows for a penalty scaling function that
#'   penalizes seasonal and recent lags less according to the penalty scaling
#'   functions described in Peterson & Cavanaugh (2024). See the
#'   \code{penalty_scaler} function for more details, and to plot the penalty
#'   function for various values of \code{m} and \code{r}.
#'
#' @references Breheny, P. and Huang, J. (2011) Coordinate descent algorithms
#'   for nonconvex penalized regression, with applications to biological feature
#'   selection. Ann. Appl. Statist., 5: 232-253.
#'
#'   Peterson, R.A., Cavanaugh, J.E. (2022) Ranked sparsity: a cogent
#'   regularization framework for selecting and estimating feature interactions
#'   and polynomials. AStA Adv Stat Anal.
#'   https://doi.org/10.1007/s10182-021-00431-7
#'
#'   Peterson, R.A., Cavanaugh, J.E. (2024). Fast, effective, and coherent time
#'   series modeling using the sparsity-ranked lasso. Statistical Modelling
#'   (accepted). DOI: https://doi.org/10.48550/arXiv.2211.01492
#'
#' @seealso predict.fastTS
#'
#' @importFrom ncvreg ncvreg
#' @importFrom stats AIC BIC coef complete.cases lm logLik na.omit pacf predict
#'   ts
#' @importFrom methods is
#'
#' @examples
#' data("LakeHuron")
#' fit_LH <- fastTS(LakeHuron)
#' fit_LH
#' coef(fit_LH)
#' plot(fit_LH)
#'
#' @export
fastTS <- function(
  y, X = NULL, n_lags_max, gamma = c(0, 2^(-2:4)),
  ptrain = .8, pf_eps = 0.01, w_endo, w_exo,
  weight_type = c("pacf", "parametric"),
  m = NULL, r = c(rep(.1, length(m)), .01),
  plot = FALSE,
  ncvreg_args = list(penalty = "lasso", returnX = FALSE, lambda.min = .001)) {

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

  if(missing(w_endo)) {
    weight_type <- match.arg(weight_type)
    if(weight_type == "pacf") {
      pacfs <- pacf(ts(ytrain), lag.max = n_lags_max, plot = plot)
      w_endo <- abs(pacfs$acf)
      if(length(m))
        warning("m is ignored when weight_type = 'pacf'")
    } else if(weight_type == "parametric") {
      w_endo <- 1/exp(penalty_scaler(1:n_lags_max, m, r, plot = plot))
    }
  }

  w <- c(w_endo, w_exo)

  pfs <- sapply(1:length(gamma), function(g) w^-gamma[g])
  pfs[pfs < pf_eps] <- 0
  pfs[is.infinite(w), 1] <- 0

  ncvreg_args$X <- Xfulltrain
  ncvreg_args$y <- y_cc_train

  srl_fits <- apply(pfs, 2, function(x) {
    ncvreg_args$penalty.factor <- x
    do.call(ncvreg::ncvreg, ncvreg_args)
  })

  best_fit_penalized_bic <-
    srl_fits[[which.min(apply(sapply(srl_fits, BIC), 2, min))]]
  best_fit_penalized_aicc <-
    srl_fits[[which.min(apply(sapply(srl_fits, AICc), 2, min))]]

  oos_results <- data.frame(rmse = NA, rsq = NA, mae = NA)
  if(ptrain < 1)
    oos_results <- get_oos_results(srl_fits, ytest=ytest, Xtest=Xfulltest)

  results <- list(
    fits = srl_fits,
    ncvreg_args = ncvreg_args,
    gamma = gamma,
    n_lags_max = n_lags_max,
    y = y, X = X, y_cc_train = y_cc_train,
    Xfulltrain = Xfulltrain,
    oos_results = oos_results,
    train_idx = train_idx,
    weight_type = weight_type,
    m = m, r = r
  )

  class(results) <- "fastTS"
  results
}

#' @rdname fastTS
#'
#' @param x a fastTS object
#' @param log.l Should the x-axis (lambda) be logged?
#' @method plot fastTS
#' @importFrom graphics abline
#' @export
#'
#' @return x invisibly
#'
plot.fastTS <- function(x, log.l = TRUE, ...){

  best_fit_penalized_aicc <-
    x$fits[[which.min(apply(sapply(x$fits, AICc), 2, min))]]

  tr_fn <- ifelse(log.l, log, I)

  plot(best_fit_penalized_aicc, log.l = log.l, ...)
  abline(v = tr_fn(
    best_fit_penalized_aicc$lambda[which.min(AICc(best_fit_penalized_aicc))]
    ))
  abline(v = tr_fn(
    best_fit_penalized_aicc$lambda[which.min(BIC(best_fit_penalized_aicc))]
    ))
  invisible(x)
}

#' @rdname fastTS
#' @param object a fastTS object
#' @param choose which criterion to use for lambda selection (AICc or BIC)
#' @method coef fastTS
#' @return a vector of model coefficients
#'
#' @export
coef.fastTS <- function(object, choose = c("AICc", "BIC"), ...) {

  choose <- match.arg(choose)

  pen_fn <- AICc

  if(choose == "BIC")
    pen_fn <- BIC

  best_fit_penalized <-
    object$fits[[which.min(apply(sapply(object$fits, pen_fn), 2, min))]]
  predict(best_fit_penalized,
          type = "coef",
          which = which.min(pen_fn(best_fit_penalized)))
}

#' @rdname fastTS
#' @param x a fastTS object
#' @method print fastTS
#' @returns x (invisibly)
#' @export
print.fastTS <- function(x, ...) {

  best_AICc = apply(sapply(x$fits, AICc), 2, min)
  best_BIC = apply(sapply(x$fits, BIC), 2, min)

  # If there are ties, point out the one with lowest gamma
  best_AICc[which(best_AICc == min(best_AICc))[1]] <- min(best_AICc) - .0001
  best_BIC[which(best_BIC == min(best_BIC))[1]] <- min(best_BIC) - .0001

  AICc_d <- best_AICc - min(best_AICc)
  BIC_d <- best_BIC - min(best_BIC)

  aic_pretty <- round(AICc_d, 2)
  bic_pretty <- round(BIC_d, 2)

  aic_pretty[AICc_d < 0.01] <- "<0.01"
  bic_pretty[BIC_d < 0.01] <- "<0.01"

  aic_pretty[AICc_d == 0] <- "*0*"
  bic_pretty[BIC_d == 0] <- "*0*"


  gamma_summary <- data.frame(
    "PF_gamma" = x$gamma,
    AICc_d = aic_pretty,
    BIC_d = bic_pretty,
    check.names = FALSE
  )

  print(gamma_summary, row.names = FALSE)

  cat("\nNote: AICc_d and BIC_d are the difference from the minimum; *0* is best.\n")

  cat("\nTest-set prediction accuracy\n")
  print(x$oos_results, row.names = TRUE)
}

#' @rdname fastTS
#' @method summary fastTS
#' @returns the summary object produced by ncvreg
#'   evaluated at the best tuning parameter combination
#'   (best AICc).
#' @export
summary.fastTS <- function(object, choose = c("AICc", "BIC"), ...) {

  choose <- match.arg(choose)
  pen_fn <- AICc
  if(choose == "BIC")
    pen_fn <- BIC

  ics <- sapply(object$fits, pen_fn)
  best_idx <- which(ics == min(ics), arr.ind = TRUE)

  best_fit <- object$fits[[best_idx[2]]]
  best_gamma <- object$gamma[best_idx[2]]
  best_lambda <- best_fit$lambda[best_idx[1]]

  s <- summary(best_fit, which = which(pen_fn(best_fit) == min(ics)),
               X = object$Xfulltrain, y = object$y_cc_train, method = "kernel",
               ...)
  cat("Model summary (ncvreg) at optimal ", choose, " (lambda=",
      round(best_lambda, 4), "; gamma=", round(best_gamma, 4),
      ")\n\n", sep = "")
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
