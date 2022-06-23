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
#' a simliar approach to the adaptive lasso (using bivariate OLS estimates). For
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
#' @importFrom butcher axe_data
#' @importFrom stats AIC BIC coef complete.cases lm logLik na.omit pacf predict
#'   ts
#' @importFrom methods is
#'
#' @export
srlTS <- function(y, X = NULL, n_lags_max, gamma = c(0, 2^(-2:4)), ptrain = .8,
                       pf_eps = 0.01, w_endo, w_exo,
                  ncvreg_args = list(penalty = "lasso", returnX = FALSE, lambda.min = .001)) {
  n <- length(y)

  if(missing(n_lags_max))
     n_lags_max <- min(1000, n/10)

  train_idx <- 1:floor(n*ptrain)

  if(any(is.na(y)))
    stop("Cannot have missing values in outcome; run imputation first?")

  if(any(is(y) == "ts"))
    y <- as.numeric(y)

  ytest <- y[-train_idx]
  ytrain <- y[train_idx]


  ## if exogenous features supplied...
  if(!is.null(X)) {
    X <- as.matrix(X)

    stopifnot(is.numeric(X))

    if(is.null(colnames(X)))
      colnames(X) <- paste0("X", 1:ncol(X))

    if(any(is.na(X)))
      stop("Cannot have missing values in covariates; run imputation first?")

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

#' @importFrom graphics abline
#' @export
plot.srlTS <- function(x, log.l = TRUE, ...){

  best_fit_penalized_aicc <- x$fits[[which.min(apply(sapply(x$fits, AICc), 2, min))]]

  tr_fn <- ifelse(log.l, log, I)

  plot(best_fit_penalized_aicc, log.l = log.l, ...)
  abline(v = tr_fn(best_fit_penalized_aicc$lambda[which.min(AICc(best_fit_penalized_aicc))]))
  abline(v = tr_fn(best_fit_penalized_aicc$lambda[which.min(BIC(best_fit_penalized_aicc))]))

  invisible(x)
}


#' @export
coef.srlTS <- function(object, choose = c("AICc", "BIC", "all"), ...) {

  choose <- match.arg(choose)

  if(choose != "AICc")
    stop("non-AICc options not yet supported")

  best_fit_penalized_aicc <- object$fits[[which.min(apply(sapply(object$fits, AICc), 2, min))]]
  predict(best_fit_penalized_aicc,  type = "coef", which = which.min(AICc(best_fit_penalized_aicc)))
}

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

#' @export
summary.srlTS <- function(object, ...) {
  aics <- sapply(object$fits, function(x) min(AICc(x)))
  best_fit <- object$fits[[which.min(aics)]]

  cat("Model summary at optimal AICc\n")
  summary(best_fit, which = which(AICc(best_fit) == min(aics)), X = object$Xfulltrain, y = object$y_cc_train, ...)
}
