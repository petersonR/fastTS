#' Predict function for fastTS object
#'
#' @param object an fastTS object
#' @param n_ahead the look-ahead period for predictions
#' @param X_test a matrix exogenous features for future predictions (optional)
#' @param y_test the test series for future predictions (optional)
#' @param cumulative cumulative (rolling) sums of 1-, 2-, 3-, ..., k-step-ahead
#'   predictions.
#' @param forecast_ahead returns forecasted values for end of training series
#' @param return_intermediate if TRUE, returns the intermediate predictions between
#'  the 1st and n_ahead predictions, as data frame.
#' @param ... currently unused
#'
#' @importFrom RcppRoll roll_sum
#' @importFrom utils tail
#'
#' @details
#'
#' The `y_test` argument must be supplied if predictions are desired or if
#' `n_ahead` < `nrow(X_test)`. This is because in order to obtain 1-step
#' forecast for, say, the 10th observation in the test data set, the 9th
#' observation of `y_test` is required.
#'
#' Forecasts for the first
#' `n_ahead` observations after the training set can be obtained by setting
#' `forecast_ahead` to TRUE, which will return the forecasted values at
#' the end of the training data. it produces the 1-step-ahead prediction,
#' the 2-step-ahead prediction, ... through the `n_ahead`-step prediction.
#' The `cumulative` argument is similar but will return the cumulative (rolling)
#' sums of 1-, 2-, 3=, ..., `n_ahead`-step-ahead predictions.
#'
#' @returns a vector of predictions, or a matrix of 1- through n_ahead predictions.
#'
#' @examples
#' data("LakeHuron")
#' fit_LH <- fastTS(LakeHuron)
#' predict(fit_LH)
#'
#' @export
predict.fastTS <- function(object, n_ahead = 1, X_test, y_test, cumulative = FALSE,
                           forecast_ahead = FALSE, return_intermediate = FALSE,
                           ...) {

  aics <- sapply(object$fits, AICc)
  best_idx <- which(aics == min(aics), arr.ind = TRUE)[1,]

  fit <- object$fits[[best_idx[2]]]
  n <- length(object$y)

  y <- object$y

  if(forecast_ahead) {
    if(!is.null(object$X) & missing(X_test))
      stop("Detected exogenous model; for future predictions, you must supply X_test")

    if(!missing(y_test)) {
      stop("y_test not used when forecast_ahead = TRUE")
    } else {
      full_y <- c(object$y, rep(NA, n_ahead))
    }

    if(missing(X_test)) {
      X_new <- get_model_matrix(full_y, n_lags_max = object$n_lags_max)
    } else {
      X_test <- as.matrix(X_test)

      if(nrow(X_test) != n_ahead) {
        if(n_ahead > 1)
          warning("n_ahead set to match X_test")
        n_ahead <- nrow(X_test)
        full_y <- c(object$y, rep(NA, n_ahead))
      }

      stopifnot(isTRUE(ncol(object$X) == ncol(X_test)))
      exo <- rbind(object$X, X_test)
      X_new <- get_model_matrix(full_y, X = exo, n_lags_max = object$n_lags_max)
    }

    # This is for getting the best possible prediction of the next n_ahead values
    # Instead of n_ahead referring to the look-ahead period for all predictions,
    # it refers to the number of predictions to make into the "future" from the
    # last training observation
    if(n_ahead > 1) {
      # Fill in lags for 1 through n_ahead with model predictions
      for(j in 1:(n_ahead-1)) {
        # Predict the next j values following the last training observation
        p_j <- unname(predict(fit, X = X_new[(n+1):(n+j),], which = best_idx[1]))
        if(j < object$n_lags_max) {
          # Replace the last j values with the model predictions
          X_new[n + j + 1, j:1] <- p_j
        } else {
          X_new[n + j + 1, object$n_lags_max:1] <- tail(p_j, object$n_lags_max)
        }
      }
    }

    p <- unname(predict(fit, X = X_new, which = best_idx[1]))
    return(p[-(1:n)])

  }

  if(missing(X_test) & missing(y_test)) {
    y_test <- NULL

    exo <- object$X

    # get one-step predictions
    X_orig <- get_model_matrix(object$y, X = exo, n_lags_max = object$n_lags_max)

    p <- unname(predict(fit, X = X_orig, which = best_idx[1]))
    preds <- data.frame(p1 = p)
    preds_bf <- data.frame(p1 = c(c(y[1:object$n_lags_max], p[-1:-object$n_lags_max])))

    # get multi-step predictions
    if(n_ahead > 1) {
      X_new <- X_orig

      for(j in 2:n_ahead)  {

        # Replace X_new lags with correct predicted values
        for(lag in 1:(j-1)) {
          if(lag <= object$n_lags_max) {
               X_new[,lag] <- preds_bf[,j - lag]
          }
        }

        p <- unname(predict(fit, X = X_new, which = best_idx[1]))
        preds_bf[[j]] <- c(y[1:object$n_lags_max], p[-1:-object$n_lags_max])
        p[1:(object$n_lags_max + j - 1)] <- NA
        preds[[j]] <- p

      }
    }

    cut_train_later <- FALSE

  } else if (missing(X_test)) {
    # If y_test supplied but not X_test, must be endogenous model
    if(!is.null(object$X))
      stop("Detected exogenous model; for future predictions, you must supply X_test")

    full_y <- c(object$y, y_test)
    X_orig <- get_model_matrix(full_y, n_lags_max = object$n_lags_max)

    p <- unname(predict(fit, X = X_orig, which = best_idx[1]))
    preds <- data.frame(p1 = p)
    preds_bf <- data.frame(p1 = c(c(y[1:object$n_lags_max], p[-1:-object$n_lags_max])))

    if(n_ahead > 1) {
      X_new <- X_orig

      for(j in 2:n_ahead)  {
        # Replace X_new lags with correct predicted values
        for(lag in 1:(j-1)) {
          if(lag <= object$n_lags_max) {
            X_new[,lag] <- preds_bf[,j - lag]
          }
        }

        p <- unname(predict(fit, X = X_new, which = best_idx[1]))
        preds_bf[[j]] <- c(full_y[1:object$n_lags_max], p[-1:-object$n_lags_max])
        p[1:(object$n_lags_max + j - 1)] <- NA
        preds[[j]] <- p
      }
    }

    # Only return test data predictions
    if(!cumulative) {
      preds <- preds[-(1:n),, drop = FALSE]
      cut_train_later <- FALSE
    } else
      cut_train_later <- TRUE

  } else {
    X_test <- as.matrix(X_test)
    stopifnot(isTRUE(ncol(object$X) == ncol(X_test)))

    if(missing(y_test)) {
      stop("y_test required for future predictions, unless forecast_ahead=TRUE")
    } else {
      stopifnot(length(y_test) == nrow(X_test))
      full_y <- c(object$y, y_test)
    }

    exo <- rbind(object$X, X_test)
    X_orig <- get_model_matrix(full_y, X = exo, n_lags_max = object$n_lags_max)

    p <- unname(predict(fit, X = X_orig, which = best_idx[1]))
    preds <- data.frame(p1 = p)
    preds_bf <- data.frame(p1 = c(c(y[1:object$n_lags_max], p[-1:-object$n_lags_max])))

    if(n_ahead > 1) {
      X_new <- X_orig

      for(j in 2:n_ahead)  {
        # Replace X_new lags with correct predicted values
        for(lag in 1:(j-1)) {
          if(lag <= object$n_lags_max) {
            X_new[,lag] <- preds_bf[,j - lag]
          }
        }

        p <- unname(predict(fit, X = X_new, which = best_idx[1]))
        preds_bf[[j]] <- c(full_y[1:object$n_lags_max], p[-1:-object$n_lags_max])
        p[1:(object$n_lags_max + j - 1)] <- NA
        preds[[j]] <- p
      }
    }

    # Only return test data predictions
    if(!cumulative) {
      preds <- preds[-(1:n),, drop = FALSE]
      cut_train_later <- FALSE
    } else
      cut_train_later <- TRUE

  }

  names(preds) <- paste0("p", 1:ncol(preds))
  ret <- preds

  if(cumulative) {

    # Need to "shift" preds
    if(n_ahead > 1) {
      for(j in 2:n_ahead) {
        preds[,j] <- lag(preds[,j], j-1)
      }
    }
    ret <- rowSums(preds)

    if(cut_train_later)
      ret <- ret[-(1:n)]

  } else if(!return_intermediate) {
    ret <- ret[,ncol(ret)]
  }

  ret
}
