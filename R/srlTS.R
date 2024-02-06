#' Perform time series ranked sparsity methods
#'
#' @aliases plot.srlTS coef.srlTS print.srlTS
#'
#' @param y univariate time series outcome
#' @param X matrix of predictors (no intercept)
#' @param n_lags_max maximum number of lags to consider
#' @param gamma vector of exponent for weights
#' @param ptrain prop. to leave out for test data
#' @param pf_eps penalty factors below this will be set to zero
#' @param w_endo optional pre-specified weights for endogenous terms
#' @param w_exo optional pre-specified weights for exogenous terms (see details)
#' @param ncvreg_args additional args to pass through to ncvreg
#' @param ... passed to downstream functions
#'
#' @return A list of class \code{slrTS} with elements
#'
#'   \item{fits}{a list of lasso fits}
#'   \item{ncvreg_args}{arguments passed to ncvreg}
#'   \item{gamma}{the (negative) exponent on the penalty weights, one
#'   for each fit}
#'   \item{n_lags_max}{the maximum number of lags}
#'   \item{y}{the time series}
#'   \item{X}{the utilized matrix of exogenous features}
#'   \item{oos_results}{results on test data using best of fits}
#'   \item{train_idx}{index of observations used in training data}
#'
#' @details The default weights for exogenous features will be chosen based on
#' a similar approach to the adaptive lasso (using bivariate OLS estimates). For
#' lower dimensional X, it's advised to set \code{w_exo="unpenalized"}, because
#' this allows for statistical inference on exogenous variable coefficients
#' via the \code{summary} function.
#'
#' @references Breheny, P. and Huang, J. (2011) Coordinate descent algorithms
#'   for nonconvex penalized regression, with applications to biological feature
#'   selection. Ann. Appl. Statist., 5: 232-253.
#'
#'   Peterson, R.A., Cavanaugh, J.E. Ranked sparsity: a cogent regularization
#'   framework for selecting and estimating feature interactions and
#'   polynomials. AStA Adv Stat Anal (2022).
#'   https://doi.org/10.1007/s10182-021-00431-7
#'
#' @seealso predict.srlTS
#'
#' @importFrom ncvreg ncvreg
#' @importFrom stats AIC BIC coef complete.cases lm logLik na.omit pacf predict
#'   ts
#' @importFrom methods is
#'
#' @examples
#' data("LakeHuron")
#' fit_LH <- srlTS(LakeHuron)
#' fit_LH
#' coef(fit_LH)
#' plot(fit_LH)
#'
#' @export
srlTS <- function(
  y, X = NULL, n_lags_max, gamma = c(0, 2^(-2:4)),
  ptrain = .8, pf_eps = 0.01, w_endo, w_exo,
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
    pacfs <- pacf(ts(ytrain), lag.max = n_lags_max, plot = FALSE)

    w <- c(
      as.vector(abs(pacfs$acf)),
      w_exo
    )
  } else
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

  oos_results <- get_oos_results(srl_fits, ytest=ytest, Xtest=Xfulltest)

  results <- list(
    fits = srl_fits,
    ncvreg_args = ncvreg_args,
    gamma = gamma,
    n_lags_max = n_lags_max,
    y = y, X = X, y_cc_train = y_cc_train,
    Xfulltrain = Xfulltrain,
    oos_results = oos_results,
    train_idx = train_idx
  )

  class(results) <- "srlTS"
  results
}

#' @rdname srlTS
#'
#' @param x a srlTS object
#' @param log.l Should the x-axis (lambda) be logged?
#' @method plot srlTS
#' @importFrom graphics abline
#' @export
#'
#' @return x invisibly
#'
plot.srlTS <- function(x, log.l = TRUE, ...){

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

#' @rdname srlTS
#' @param object a srlTS object
#' @param choose which criterion to use for lambda selection (AICc, BIC, or all)
#' @method coef srlTS
#' @return a vector of model coefficients
#'
#' @export
coef.srlTS <- function(object, choose = c("AICc", "BIC", "all"), ...) {

  choose <- match.arg(choose)

  if(!(choose %in% c("BIC", "AICc")))
    stop("choose option not yet supported")

  pen_fn <- AICc

  if(choose == "BIC")
    pen_fn <- BIC

  best_fit_penalized <-
    object$fits[[which.min(apply(sapply(object$fits, pen_fn), 2, min))]]
  predict(best_fit_penalized,
          type = "coef",
          which = which.min(pen_fn(best_fit_penalized)))
}

#' @rdname srlTS
#' @param x a srlTS object
#' @method print srlTS
#' @returns x (invisibly)
#' @export
print.srlTS <- function(x, ...) {
  gamma_summary <- data.frame(
    "PF_gamma" = x$gamma,
    best_AICc = apply(sapply(x$fits, AICc), 2, min),
    best_BIC = apply(sapply(x$fits, BIC), 2, min)
  )

  print(gamma_summary, row.names = FALSE)

  cat("\nTest-set prediction accuracy\n")
  print(x$oos_results, row.names = TRUE)
}

#' @rdname srlTS
#' @method summary srlTS
#' @returns the summary object produced by ncvreg
#'   evaluated at the best tuning parameter combination
#'   (best AICc).
#' @export
summary.srlTS <- function(object, ...) {
  aics <- sapply(object$fits, function(x) AICc(x))
  best_idx <- which(aics == min(aics),arr.ind = TRUE)

  best_fit <- object$fits[[best_idx[2]]]
  best_gamma <- object$gamma[best_idx[2]]
  best_lambda <- best_fit$lambda[best_idx[1]]

  s <- summary(best_fit, which = which(AICc(best_fit) == min(aics)),
               X = object$Xfulltrain, y = object$y_cc_train, ...)
  cat("Model summary at optimal AICc (lambda=", round(best_lambda, 4),
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
