#' Predict function for srlTS object
#'
#' @param object an srlTS object
#' @param n_ahead number of times ahead to predict by iteration
#' @param X_test a matrix exogenous features
#' @param y_test the test series; needed for future predictions (optional; see
#'   details)
#' @param cumulative should cumulative (rolling) sums be returned (integer
#'   indicating number of times to sum)
#' @param ... currently unused
#'
#' @importFrom RcppRoll roll_sum
#' @importFrom utils tail
#'
#' @details
#'
#' The `y_test` argument must be supplied if forecasts are desired or if
#' `n_ahead` < `nrow(X_test)`. This is because in order to obtain 1-step
#' forecast for, say, the 10th observation in the test data set, the 9th
#' observation of `y_test` is required. The length of `y_test` will determine
#' how many forecasts to produce. In order to get true forecasts for the first
#' 30 observations after the training set, one must (currently) produce the set
#' of 1-step, 2-step, 3-step, ..., 30-step ahead predictions.
#'
#' @returns a vector of predictions
#'
#' @examples
#' data("LakeHuron")
#' fit_LH <- srlTS(LakeHuron)
#' predict(fit_LH)
#'
#' @export
predict.srlTS <- function(object, n_ahead = 1, X_test, y_test, cumulative = 0, ...) {

  aics <- sapply(object$fits, AICc)
  best_idx <- which(aics == min(aics), arr.ind = TRUE)[1,]

  fit <- object$fits[[best_idx[2]]]
  n <- length(object$y)

  X_orig <- get_model_matrix(object$y, X = object$X, n_lags_max = object$n_lags_max)

  if(missing(X_test) & missing(y_test)) {
    # get one-step predictions
    p <- unname(predict(fit, X = X_orig, which = best_idx[1]))

    if(n_ahead > 1) {
      X_new <- X_orig
      for(j in 2:n_ahead) {
        X_new <- cbind(
          c(NA, p[-length(p)]),
          X_new[,-1]
        )
        colnames(X_new) <- colnames(X_orig)
        p <- unname(predict(fit, X = X_new, which = best_idx[1]))
      }
    }

    if(cumulative <= 1)
      return(p)
    if(cumulative > 1)
      return(roll_sum(p, n = cumulative, align = "right", fill = NA))

  } else if (missing(X_test)) {
    # If y_test supplied but not X_test, must be endogenous model
    if(!is.null(object$X))
      stop("Detected exogenous model; for future predictions, you must supply X_test")

    full_y <- c(object$y, y_test)
    X_new <- get_model_matrix(full_y, n_lags_max = object$n_lags_max)

    # get one-step predictions
    p <- unname(predict(fit, X = X_new, which = best_idx[1]))

    if(n_ahead > 1) {
      for(j in 2:n_ahead) {
        X_new <- cbind(
          c(NA, p[-length(p)]),
          X_new[,-1]
        )
        colnames(X_new) <- colnames(X_orig)
        p <- unname(predict(fit, X = X_new, which = best_idx[1]))
      }
    }

    if(cumulative <= 1)
      return(p[-(1:n)])

    # Cumulatively add predictions
    psum <- roll_sum(p, n = cumulative, align = "right", fill = NA)
    return(psum[-(1:n)])

  } else {
    # If X_test provided
    X_test <- as.matrix(X_test)
    stopifnot(isTRUE(ncol(object$X) == ncol(X_test)))

    if(missing(y_test)) {
      warning("setting n_ahead to ", nrow(X_test), ";, otherwise supply y_test")
      n_ahead <- nrow(X_test)
      full_y <- c(object$y, rep(NA, n_ahead))
    } else {
      full_y <- c(object$y, y_test)
    }

    X_new <- get_model_matrix(
      full_y, X = rbind(object$X, X_test), n_lags_max = object$n_lags_max
    )
  }

  if(n_ahead > 1) {
    # Fill in lags for 1 through n_ahead with model predictions
    for(j in 1:(n_ahead-1)) {
      p_j <- unname(predict(fit, X = X_new[(n+1):(n+j),], which = best_idx[1]))
      if(j < object$n_lags_max) {
        X_new[n + j + 1, j:1] <- p_j
      } else {
        X_new[n + j + 1, object$n_lags_max:1] <- tail(p_j, object$n_lags_max)
      }
    }
  }

  p <- unname(predict(fit, X = X_new, which = best_idx[1]))

  if(cumulative <= 1)
    return(p[-(1:n)])

  # Cumulatively add predictions
  psum <- roll_sum(p, n = cumulative, align = "right", fill = NA)
  psum[-(1:n)]
}
