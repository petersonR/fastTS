
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

  expect_silent(fit <- srlTS(y, gamma = c(0, .5), X = X, w_exo = "unpenalized"))

})


test_that("srlTS stops with missings", {
  y2 <- y; y2[c(1, 50)] <- NA
  X2 <- X; X2[1,4] <- NA
  expect_error(fit <- srlTS(y2, gamma = c(0, .5)))
  expect_error(fit <- srlTS(y, X=X2, gamma = c(0, .5)))
})


