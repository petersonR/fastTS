
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

test_that("predict.srlTS works as intended when X not supplied", {
  expect_silent(p <- predict(fit))
  expect_gt(cor(p, y, use = "pairwise.complete"), .95)
  expect_silent(p2 <- predict(fit, n_ahead = 2))

  expect_length(p, length(y))
  expect_length(p2, length(y))

  expect_gt(cor(p, y, use = "pairwise.complete"), .95)
  expect_gt(cor(p2, y, use = "pairwise.complete"), .9)
  expect_gt(cor(p, p2, use = "pairwise.complete"), .97)

})

X_test <- X[12:1,]
test_that("predict.srlTS works as intended when X_test supplied", {
  expect_warning(p <- predict(fit, X_test = X_test))
  expect_length(p, nrow(X_test))

  expect_silent(p <- predict(fit, X_test = X_test, y_test = y[12:1]))
  expect_length(p, nrow(X_test))

  expect_silent(p2 <- predict(fit, X_test = X_test, y_test = y[12:1], n_ahead = 2))
  expect_length(p2, nrow(X_test))
  expect_gt(cor(p2, p), .88)

  expect_silent(pc <- predict(fit, X_test = X_test, y_test = y[12:1], cumulative = 12))
  expect_length(pc, nrow(X_test))
  expect_equal(pc[12], sum(p))

  expect_silent(p <- predict(fit, X_test = X_test, y_test = y[12:1], cumulative = 2, n_ahead = 2))
  expect_length(p, nrow(X_test))
})
