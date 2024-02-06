
library(srlTS)

set.seed(1)
y <- cumsum(rnorm(100))

test_that("srlTS works as expected, endogenous", {

  expect_silent(fit <- srlTS(y, gamma = c(0, .5)))
  expect_silent(b <- coef(fit))

  expect_length(fit$fits, 2)

  expect_output(print(fit))
  expect_invisible(plot(fit))

})

data(iris)
X <- model.matrix(~., iris[sample(1:150, size = 100),])[,-1]

test_that("srlTS works as expected, exogenous", {

  expect_silent(fit <- srlTS(y, gamma = c(0, .5), X = X))
  expect_silent(b <- coef(fit))
  expect_length(b, 17)

  expect_output(print(fit))
  expect_invisible(plot(fit))

  expect_silent(fit <<- srlTS(y, gamma = c(0, .5), X = X, w_exo = "unpenalized"))

})


test_that("srlTS stops with missings", {
  y2 <- y; y2[c(1, 50)] <- NA
  X2 <- X; X2[1,4] <- NA
  expect_error(fit <- srlTS(y2, gamma = c(0, .5)))
  expect_error(fit <- srlTS(y, X=X2, gamma = c(0, .5)))
})

test_that("srlTS works with missing and nonstandard w_exo, w_endo", {
  expect_error(fit <- srlTS(y, gamma = c(0, .5), w_exo = c(NA, 1)))
  expect_error(fit <- srlTS(y, gamma = c(0, .5), w_endo = c(NA, 1)))
  expect_error(fit <- srlTS(y, gamma = c(0, .5), w_endo = c(NA, 1), w_exo = c(NA, 1)))

  expect_silent(fit <- srlTS(y, gamma = c(0, .5), X = X[,1:2],  w_exo = c(Inf, 1)))
  expect_silent(fit <- srlTS(y, gamma = c(0, .5), X = X[,1:2], w_exo = c(0, 1)))
  expect_error(fit <- srlTS(y, gamma = c(0, .5),  X = X[,1:2],  w_exo = c(-1, 1)))

  expect_silent(fit <- srlTS(y, gamma = c(0, .5), n_lags_max = 2, w_endo = c(Inf, 1)))
  expect_silent(fit <- srlTS(y, gamma = c(0, .5), n_lags_max = 2, w_endo = c(0, 1)))
  expect_error(fit <- srlTS(y, gamma = c(0, .5), n_lags_max = 2, w_endo = c(-1, 1)))
})

test_that("srlTS n_lags_max works", {
  expect_silent(fit <- srlTS(y, gamma = c(0, .5), n_lags_max = 10))
  expect_silent(b <- coef(fit))
  expect_length(fit$fits, 2)

  expect_error(fit <- srlTS(y, gamma = c(0, .5), n_lags_max = 100))
  expect_error(fit <- srlTS(y, gamma = c(0, .5), n_lags_max = 1))
  expect_silent(fit <- srlTS(y, gamma = c(0, .5), n_lags_max = 2))

  expect_error(fit <- srlTS(y, gamma = c(0, .5), n_lags_max = 1000))
  expect_error(fit <- srlTS(y, gamma = c(0, .5), n_lags_max = -1))
  expect_error(fit <- srlTS(y, gamma = c(0, .5), n_lags_max = NA))
  expect_error(fit <- srlTS(y, gamma = c(0, .5), n_lags_max = c(4, 5)))
})

test_that("srlTS ptrain works", {
  expect_silent(fit <- srlTS(y, gamma = c(0, .5), ptrain = .5))
  expect_silent(b <- coef(fit))
  expect_length(fit$fits, 2)

  expect_error(fit <- srlTS(y, gamma = c(0, .5), ptrain = .1))
  expect_silent(fit <- srlTS(y, gamma = c(0, .5), ptrain = .12))
  expect_silent(fit <- srlTS(y, gamma = c(0, .5), ptrain = 1))
  expect_error(fit <- srlTS(y, gamma = c(0, .5), ptrain = 0))
  expect_error(fit <- srlTS(y, gamma = c(0, .5), ptrain = NA))
  expect_error(fit <- srlTS(y, gamma = c(0, .5), ptrain = c(.5, .6)))
})

## Restart testing here
test_that("srlTS gamma works", {
  expect_silent(fit <- srlTS(y, gamma = c(0, .5)))
  expect_silent(b <- coef(fit))
  expect_length(fit$fits, 2)

  expect_silent(fit <- srlTS(y, gamma = c(0, 1)))
  expect_silent(b <- coef(fit))
  expect_length(fit$fits, 2)

  expect_silent(fit <- srlTS(y, gamma = c(0, 1, 2)))
  expect_silent(b <- coef(fit))
  expect_length(fit$fits, 3)

  expect_error(fit <- srlTS(y, gamma = c(0, 1, 2, 3)))
  expect_error(fit <- srlTS(y, gamma = c(0, 1, 2, -1)))
  expect_error(fit <- srlTS(y, gamma = c(0, 1, 2, NA)))
})

test_that("srlTS works with ts", {
  y2 <- ts(y)
  expect_silent(fit <- srlTS(y2, gamma = c(0, .5)))
  expect_silent(b <- coef(fit))
  expect_length(fit$fits, 2)
})

test_that("srlTS works with non-numeric y", {
  y2 <- as.character(y)
  expect_error(fit <- srlTS(y2, gamma = c(0, .5)))
  y2 <- as.factor(y)
  expect_error(fit <- srlTS(y2, gamma = c(0, .5)))
  y2 <- as.logical(y)
  expect_error(fit <- srlTS(y2, gamma = c(0, .5)))
})

test_that("srlTS works with non-numeric X", {
  X2 <- as.character(X)
  expect_error(fit <- srlTS(y, gamma = c(0, .5), X = X2))
  X2 <- as.factor(X)
  expect_error(fit <- srlTS(y, gamma = c(0, .5), X = X2))
  X2 <- as.logical(X)
  expect_error(fit <- srlTS(y, gamma = c(0, .5), X = X2))
})

test_that("srlTS works with ncvreg_args", {
  expect_silent(fit <- srlTS(y, gamma = c(0, .5), ncvreg_args = list(penalty = "mcp")))
  expect_silent(b <- coef(fit))
  expect_length(fit$fits, 2)

  expect_silent(fit <- srlTS(y, gamma = c(0, .5), ncvreg_args = list(penalty = "mcp", lambda.min = .1)))
  expect_silent(b <- coef(fit))
  expect_length(fit$fits, 2)
})

test_that("coef.srlTS parameter testing", {
  expect_silent(fit <- srlTS(y))
  expect_error(b <- coef(fit, choose = "EBIC"))
  expect_silent(b <- coef(fit, choose = "AICc"))
  expect_silent(b <- coef(fit, choose = "BIC"))
})
