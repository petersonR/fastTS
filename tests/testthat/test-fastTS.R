
library(fastTS)

set.seed(1)
y <- cumsum(rnorm(100))

test_that("fastTS works as expected, endogenous", {

  expect_silent(fit <- fastTS(y, gamma = c(0, .5)))
  expect_silent(b <- coef(fit))

  expect_length(fit$fits, 2)

  expect_output(print(fit))
  expect_invisible(plot(fit))

})

data(iris)
X <- model.matrix(~., iris[sample(1:150, size = 100),])[,-1]

test_that("fastTS works as expected, exogenous", {

  expect_silent(fit <- fastTS(y, gamma = c(0, .5), X = X))
  expect_silent(b <- coef(fit))
  expect_length(b, 17)

  expect_output(print(fit))
  expect_invisible(plot(fit))

  expect_silent(fit <<- fastTS(y, gamma = c(0, .5), X = X, w_exo = "unpenalized"))

})


test_that("fastTS stops with missings", {
  y2 <- y; y2[c(1, 50)] <- NA
  X2 <- X; X2[1,4] <- NA
  expect_error(fit <- fastTS(y2, gamma = c(0, .5)))
  expect_error(fit <- fastTS(y, X=X2, gamma = c(0, .5)))
})

test_that("fastTS works with missing and nonstandard w_exo, w_endo", {
  expect_error(fit <- fastTS(y, gamma = c(0, .5), w_exo = c(NA, 1)))
  expect_error(fit <- fastTS(y, gamma = c(0, .5), w_endo = c(NA, 1)))
  expect_error(fit <- fastTS(y, gamma = c(0, .5), w_endo = c(NA, 1), w_exo = c(NA, 1)))

  expect_silent(fit <- fastTS(y, gamma = c(0, .5), X = X[,1:2],  w_exo = c(Inf, 1)))
  expect_silent(fit <- fastTS(y, gamma = c(0, .5), X = X[,1:2], w_exo = c(0, 1)))
  expect_error(fit <- fastTS(y, gamma = c(0, .5),  X = X[,1:2],  w_exo = c(-1, 1)))

  expect_silent(fit <- fastTS(y, gamma = c(0, .5), n_lags_max = 2, w_endo = c(Inf, 1)))
  expect_silent(fit <- fastTS(y, gamma = c(0, .5), n_lags_max = 2, w_endo = c(0, 1)))
  expect_error(fit <- fastTS(y, gamma = c(0, .5), n_lags_max = 2, w_endo = c(-1, 1)))
})

test_that("fastTS n_lags_max works", {
  expect_silent(fit <- fastTS(y, gamma = c(0, .5), n_lags_max = 10))
  expect_silent(b <- coef(fit))
  expect_length(fit$fits, 2)

  expect_error(fit <- fastTS(y, gamma = c(0, .5), n_lags_max = 100))
  expect_error(fit <- fastTS(y, gamma = c(0, .5), n_lags_max = 1))
  expect_silent(fit <- fastTS(y, gamma = c(0, .5), n_lags_max = 2))

  expect_error(fit <- fastTS(y, gamma = c(0, .5), n_lags_max = 1000))
  expect_error(fit <- fastTS(y, gamma = c(0, .5), n_lags_max = -1))
  expect_error(fit <- fastTS(y, gamma = c(0, .5), n_lags_max = NA))
  expect_error(fit <- fastTS(y, gamma = c(0, .5), n_lags_max = c(4, 5)))
})

test_that("fastTS ptrain works", {
  expect_silent(fit <- fastTS(y, gamma = c(0, .5), ptrain = .5))
  expect_silent(b <- coef(fit))
  expect_length(fit$fits, 2)

  expect_error(fit <- fastTS(y, gamma = c(0, .5), ptrain = .1))
  expect_silent(fit <- fastTS(y, gamma = c(0, .5), ptrain = .12))
  expect_silent(fit <- fastTS(y, gamma = c(0, .5), ptrain = 1))
  expect_error(fit <- fastTS(y, gamma = c(0, .5), ptrain = 0))
  expect_error(fit <- fastTS(y, gamma = c(0, .5), ptrain = NA))
  expect_error(fit <- fastTS(y, gamma = c(0, .5), ptrain = c(.5, .6)))
})

test_that("fastTS gamma works", {
  expect_silent(fit <- fastTS(y, gamma = c(0, .5)))
  expect_silent(b <- coef(fit))
  expect_length(fit$fits, 2)

  expect_silent(fit <- fastTS(y, gamma = c(0, 1)))
  expect_silent(b <- coef(fit))
  expect_length(fit$fits, 2)

  expect_silent(fit <- fastTS(y, gamma = c(0, 1, 2)))
  expect_silent(b <- coef(fit))
  expect_length(fit$fits, 3)

  expect_silent(fit <- fastTS(y, gamma = c(0, 1, 2, 3)))
  expect_silent(fit <- fastTS(y, gamma = c(0, 1, 2, -1)))
  expect_error(fit <- fastTS(y, gamma = c(0, 1, 2, NA)))
})

test_that("fastTS works with ts", {
  y2 <- ts(y)
  expect_silent(fit <- fastTS(y2, gamma = c(0, .5)))
  expect_silent(b <- coef(fit))
  expect_length(fit$fits, 2)
})

test_that("fastTS works with non-numeric y", {
  y2 <- as.character(y)
  expect_error(fit <- fastTS(y2, gamma = c(0, .5)))
  y2 <- as.factor(y)
  expect_error(fit <- fastTS(y2, gamma = c(0, .5)))
  y2 <- as.logical(y)
  expect_error(fit <- fastTS(y2, gamma = c(0, .5)))
})

test_that("fastTS works with non-numeric X", {
  X2 <- as.character(X)
  expect_error(fit <- fastTS(y, gamma = c(0, .5), X = X2))
  X2 <- as.factor(X)
  expect_error(fit <- fastTS(y, gamma = c(0, .5), X = X2))
  X2 <- as.logical(X)
  expect_error(fit <- fastTS(y, gamma = c(0, .5), X = X2))
})

test_that("fastTS works with ncvreg_args", {
  expect_silent(fit <- fastTS(y, gamma = c(0, .5), ncvreg_args = list(penalty = "MCP")))
  expect_silent(b <- coef(fit))
  expect_length(fit$fits, 2)

  expect_silent(fit <- fastTS(y, gamma = c(0, .5), ncvreg_args = list(penalty = "MCP", lambda.min = .1)))
  expect_silent(b <- coef(fit))
  expect_length(fit$fits, 2)
})

test_that("coef.fastTS parameter testing", {
  expect_silent(fit <- fastTS(y))
  expect_error(b <- coef(fit, choose = "EBIC"))
  expect_silent(b <- coef(fit, choose = "AICc"))
  expect_silent(b <- coef(fit, choose = "BIC"))
})

test_that("parametric penalty scaling", {
  m <- 12
  r <- c(0.1, .01)
  expect_silent(penalty_scaler(1:24, m, r))
  expect_silent(penalty_scaler(1:24, m, r, plot = FALSE))
  expect_silent(penalty_scaler(1:24, m, r, log = FALSE))
  expect_silent(penalty_scaler(1:24, m, r, plot = FALSE, log = FALSE))
  expect_error(penalty_scaler(1:24, m = 0, r))
  expect_error(penalty_scaler(1:24, m = 12, r = c(0.1, .01, .001)))
  expect_silent(penalty_scaler(1:12, m = 24, r = c(0.1, .01)))
})

test_that("fastTS parametric weighting", {
  expect_silent(fit <- fastTS(y, gamma = c(0, .5), weight_type = "parametric"))
  expect_silent(b <- coef(fit))
  expect_length(fit$fits, 2)

  expect_silent(fit <- fastTS(y, gamma = c(0, .5), X = X, weight_type = "parametric"))
  expect_silent(b <- coef(fit))
  expect_length(b, 17)

  expect_silent(fit <- fastTS(y, gamma = c(0, .5), X = X, weight_type = "parametric"))
  expect_silent(b <- coef(fit))
  expect_length(b, 17)

  expect_silent(fit <- fastTS(y, gamma = c(0, .5), weight_type = "parametric", m=12))
  expect_silent(fit <- fastTS(y, gamma = c(0, .5), X = X, weight_type = "parametric", m=12))
  expect_silent(fit <- fastTS(y, gamma = c(0, .5), weight_type = "parametric", m=12, r = c(0.1, .01)))

  expect_error(fit <- fastTS(y, gamma = c(0, .5), weight_type = "parametric", m=12, r = c(0.1, .01, .001)))

  expect_warning(fit <- fastTS(y, gamma = c(0, .5), m = 12))
})

test_that("fastTS fits Lake Huron decently well", {
  data("LakeHuron")
  expect_silent(fit_srl_pacf <- fastTS(LakeHuron))
  expect_gt(fit_srl_pacf$oos_results$rsq[1], 0.6)

  expect_silent(fit_srl_par <- fastTS(LakeHuron, weight_type = "parametric"))
  expect_gt(fit_srl_par$oos_results$rsq[1], 0.59)

  expect_silent(fit_srl_par <- fastTS(LakeHuron, weight_type = "parametric", m = 4))
  expect_gt(fit_srl_par$oos_results$rsq[1], .6)

  expect_silent(fit_srl_par <- fastTS(LakeHuron, weight_type = "parametric", m = 4, r = c(0.2, .01)))
  expect_gt(fit_srl_par$oos_results$rsq[1], .6)

  expect_silent(fit_srl_par <- fastTS(LakeHuron, weight_type = "parametric", r = .1, plot = T, n_lags_max = 24))
  expect_gt(fit_srl_par$oos_results$rsq[1], .6)

  # Penalty factors should be increasing when r > 0
  expect_true(all(diff(fit_srl_par$fits[[4]]$penalty.factor) > 0))

  # test with r < 0
  expect_silent(fit_srl_par2 <- fastTS(LakeHuron, weight_type = "parametric", r = -.1, plot = T, n_lags_max = 24))
  expect_gt(fit_srl_par2$oos_results$rsq[1], .6)

  # This should pick gamma = 0 (equal AR weight penalties)
  best_AICc = apply(sapply(fit_srl_par2$fits, AICc), 2, min)
  best_BIC = apply(sapply(fit_srl_par2$fits, BIC), 2, min)
  expect_equal(which.min(best_AICc), which.min(best_BIC))
  expect_equal(which.min(best_AICc), 1)

  # Penalty factors should be decreasing when r < 0
  expect_true(all(diff(fit_srl_par2$fits[[4]]$penalty.factor) < 0))

  # r > 0 model should have larger ar1 term than r < 0 model
  expect_lt(coef(fit_srl_par2)[2], coef(fit_srl_par)[2])

})


test_that("fastTS fits nottem decently well", {
  data("nottem")
  expect_silent(fit_srl_pacf <- fastTS(nottem, ptrain = .5))
  expect_gt(fit_srl_pacf$oos_results$rsq[1], .92)

  expect_silent(fit_srl_par <- fastTS(nottem, ptrain = .5, weight_type = "parametric"))
  expect_gt(fit_srl_par$oos_results$rsq[1], 0.924)

  expect_silent(fit_srl_par <- fastTS(nottem, ptrain = .5, weight_type = "parametric", m = 12))
  expect_gt(fit_srl_par$oos_results$rsq[1], .928)

  expect_silent(fit_srl_par <- fastTS(nottem, ptrain = .5, weight_type = "parametric", m = 12, r = c(0.2, .01)))
  expect_gt(fit_srl_par$oos_results$rsq[1], .929)

  expect_silent(fit_srl_par <- fastTS(nottem, ptrain = .5, weight_type = "parametric", r = .1, plot = T, n_lags_max = 24))
  expect_gt(fit_srl_par$oos_results$rsq[1], .924)

  # Penalty factors should be increasing when r > 0
  expect_true(all(diff(fit_srl_par$fits[[4]]$penalty.factor) > 0))

  # test with r < 0
  expect_silent(fit_srl_par2 <- fastTS(nottem, ptrain = .5, weight_type = "parametric",
                                       r = -.1, plot = T, n_lags_max = 24))
  expect_gt(fit_srl_par2$oos_results$rsq[1], .919)

  # This should pick gamma = 0 (equal AR weight penalties)
  best_AICc = apply(sapply(fit_srl_par2$fits, AICc), 2, min)
  best_BIC = apply(sapply(fit_srl_par2$fits, BIC), 2, min)
  expect_equal(which.min(best_AICc), which.min(best_BIC))
  expect_equal(which.min(best_AICc), 1)

  # Penalty factors should be decreasing when r < 0
  expect_true(all(diff(fit_srl_par2$fits[[4]]$penalty.factor) < 0))

  # r > 0 model should have larger ar1 term than r < 0 model
  expect_lt(coef(fit_srl_par2)[2], coef(fit_srl_par)[2])

})

test_that("fastTS fits UKDriverDeaths well", {
  data("UKDriverDeaths")
  expect_silent(fit_srl_pacf <- fastTS(UKDriverDeaths, ptrain = .5))
  expect_gt(fit_srl_pacf$oos_results$rsq[1], .669)

  expect_silent(fit_srl_par <- fastTS(UKDriverDeaths, ptrain = .5, weight_type = "parametric"))
  expect_gt(fit_srl_par$oos_results$rsq[1], 0.654)

  expect_silent(fit_srl_par <- fastTS(UKDriverDeaths, ptrain = .5, weight_type = "parametric", m = 12))
  expect_gt(fit_srl_par$oos_results$rsq[1], .676)

  expect_silent(fit_srl_par <- fastTS(UKDriverDeaths, ptrain = .5, weight_type = "parametric", m = 12, r = c(0.2, .01)))
  expect_gt(fit_srl_par$oos_results$rsq[1], .698)

  expect_silent(fit_srl_par <- fastTS(UKDriverDeaths, ptrain = .5, weight_type = "parametric", r = .1, plot = T, n_lags_max = 24))
  expect_gt(fit_srl_par$oos_results$rsq[1], .618)

  # Penalty factors should be increasing when r > 0
  expect_true(all(diff(fit_srl_par$fits[[4]]$penalty.factor) > 0))

  # test with r < 0
  expect_silent(fit_srl_par2 <- fastTS(UKDriverDeaths, ptrain = .5, weight_type = "parametric",
                                       r = -.1, plot = T, n_lags_max = 24))
  expect_gt(fit_srl_par2$oos_results$rsq[1], .616)

  # This should pick gamma = 0 (equal AR weight penalties)
  best_AICc = apply(sapply(fit_srl_par2$fits, AICc), 2, min)
  best_BIC = apply(sapply(fit_srl_par2$fits, BIC), 2, min)
  expect_equal(which.min(best_AICc), which.min(best_BIC))
  expect_equal(which.min(best_AICc), 1)
})

test_that("fastTS print and summary function works as expected", {
  expect_silent(fit_srl_pacf <- fastTS(UKDriverDeaths))
  expect_output(print(fit_srl_pacf))
  expect_output((s1 <- summary(fit_srl_pacf)))
  expect_output((s2 <- summary(fit_srl_pacf, choose = "BIC")))
  expect_error(summary(fit_srl_pacf, choose = "EBIC"))

  expect_true(!identical(s1, s2))

})
