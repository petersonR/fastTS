set.seed(123)
data("LakeHuron")
lh_train <- LakeHuron[1:80]
lh_test <- LakeHuron[-(1:80)]

# endogenous model
fit <- fastTS(lh_train)

# exogenous model
X <- rnorm(98)
X_train <- X[1:80]
X_test <- X[-(1:80)]

fitX <- fastTS(lh_train, X = X_train)


test_that("predict.fastTS when neither X_test nor y_test supplied", {

  # endogenous model
  expect_silent(p <- predict(fit))
  expect_length(p, length(lh_train))

  expect_gt(rho_1step <- cor(p, lh_train, use = "pairwise.complete"), .8)

  expect_silent(p2 <- predict(fit, n_ahead = 2))
  expect_length(p2, length(lh_train))

  expect_gt(rho_2step <- cor(p2, lh_train, use = "pairwise.complete"), .5)

  expect_gt(rho_1step, rho_2step)

  # Exogenous model
  expect_silent(p <- predict(fitX))
  expect_length(p, length(lh_train))

  expect_gt(rho_1step <- cor(p, lh_train, use = "pairwise.complete"), .8)

  expect_silent(p2 <- predict(fit, n_ahead = 2))
  expect_length(p2, length(lh_train))

  expect_gt(rho_2step <- cor(p2, lh_train, use = "pairwise.complete"), .5)

  expect_gt(rho_1step, rho_2step)
})

test_that("predict.fastTS when X_test not supplied, y_test supplied", {
  expect_silent(p <- predict(fit, y_test = lh_test))
  expect_length(p, length(lh_test))

  expect_gt(rho_1step <- cor(p, lh_test, use = "pairwise.complete"), .75)
  expect_silent(p2 <- predict(fit, y_test = lh_test, n_ahead = 2))
  expect_length(p2, length(lh_test))
  expect_gt(rho_2step <- cor(p2, lh_test, use = "pairwise.complete"), .475)

  expect_gt(rho_1step, rho_2step)

  # Exogenous model (should not work when X_test not supplied)
  expect_error(p <- predict(fitX, y_test = lh_test),
               "Detected exogenous model; for future predictions, you must supply X_test")
})

test_that("predict.fastTS when X_test supplied", {

  # should fail with endogenous model
  expect_error(p <- predict(fit, X_test = X_test))

  expect_silent(p <- predict(fitX, X_test = X_test, y_test = lh_test))
  expect_length(p, length(X_test))

  expect_silent(p2 <- predict(fitX, X_test = X_test, y_test = lh_test, n_ahead = 2))
  expect_length(p2, length(X_test))
  expect_gt(cor(p2, p), .99)

  expect_silent(p10 <- predict(fitX, X_test = X_test, y_test = lh_test, n_ahead = 10))
  expect_length(p10, length(X_test))

  expect_warning(pmax <- predict(fitX, X_test = X_test))
  expect_length(pmax, length(X_test))

  expect_gt(rho_max <- cor(pmax, lh_test, use = "pair"), 0)
  expect_gt(rho_1s <- cor(p, lh_test, use = "pair"), 0.75)
  expect_gt(rho_2s <- cor(p2, lh_test, use = "pair"), .74)
  expect_gt(rho_10s <- cor(p10, lh_test, use = "pair"), .45)

  expect_gt(rho_1s, rho_2s)
  expect_gt(rho_1s, rho_10s)
  expect_gt(rho_10s, rho_max)
})

data("sunspot.month")
ntrain <- 2000
ss_train <- sunspot.month[1:ntrain]
ss_test <- sunspot.month[-(1:ntrain)]

# endogenous model
fit <- fastTS(ss_train)

# exogenous model
X <- rnorm(length(sunspot.month))
X_train <- X[1:ntrain]
X_test <- X[-(1:ntrain)]

fitX <- fastTS(ss_train, X = X_train)

test_that("cumulative predict.fastTS when X_test not supplied", {

  # On existing data (endogenous)
  p <- predict(fit)
  p2 <- predict(fit, n_ahead = 2)
  p10 <- predict(fit, n_ahead = 10)

  y_c10 <- roll_sum(ss_train, n = 10, align = "right", fill = NA)
  y_c2 <- roll_sum(ss_train, n = 2, align = "right", fill = NA)

  expect_silent(p_c10 <- predict(fit, cumulative = 10))
  expect_silent(p2_c10 <- predict(fit, cumulative = 10, n_ahead = 2))
  expect_silent(p10_c2 <- predict(fit, cumulative = 2, n_ahead = 10))
  expect_silent(p10_c10 <- predict(fit, cumulative = 10, n_ahead = 10))

  # check rolling sums for last value
  expect_equal(sum(tail(p, 10)), tail(p_c10, 1))
  expect_equal(sum(tail(p2, 10)), tail(p2_c10, 1))
  expect_equal(sum(tail(p10, 2)), tail(p10_c2, 1))
  expect_equal(sum(tail(p10, 10)), tail(p10_c10, 1))

  # check correlations
  rho_c10_p1 <- cor(p_c10, y_c10, use = "pair")
  rho_c10_p2 <- cor(p2_c10, y_c10, use = "pair")
  rho_c10_p10 <- cor(p10_c10, y_c10, use = "pair")
  expect_gt(rho_c10_p10, 0.97)
  expect_gt(rho_c10_p2, rho_c10_p10)
  expect_gt(rho_c10_p1, rho_c10_p2)


  p <- predict(fitX)
  p2 <- predict(fitX, n_ahead = 2)
  p10 <- predict(fitX, n_ahead = 10)

  # On existing data (exogenous)
  expect_silent(p_c10 <- predict(fitX, cumulative = 10))
  expect_silent(p2_c10 <- predict(fitX, cumulative = 10, n_ahead = 2))
  expect_silent(p10_c2 <- predict(fitX, cumulative = 2, n_ahead = 10))
  expect_silent(p10_c10 <- predict(fitX, cumulative = 10, n_ahead = 10))

  # check rolling sums for last value
  expect_equal(sum(tail(p, 10)), tail(p_c10, 1))
  expect_equal(sum(tail(p2, 10)), tail(p2_c10, 1))
  expect_equal(sum(tail(p10, 2)), tail(p10_c2, 1))
  expect_equal(sum(tail(p10, 10)), tail(p10_c10, 1))

  # check correlations
  rho_c10_p1 <- cor(p_c10, y_c10, use = "pair")
  rho_c10_p2 <- cor(p2_c10, y_c10, use = "pair")
  rho_c10_p10 <- cor(p10_c10, y_c10, use = "pair")
  expect_gt(rho_c10_p10, .974)
  expect_gt(rho_c10_p2, rho_c10_p10)
  expect_gt(rho_c10_p1, rho_c10_p2)

})

test_that("cumulative predict.fastTS when X_test supplied", {

  # should fail with endogenous model
  expect_error(p <- predict(fit, X_test = X_test, cumulative = 10))

  # On existing data (warn if y_test not supplied)
  expect_warning(p_c10 <- predict(fitX,  X_test = X_test, cumulative = 10))

  y_c10 <- roll_sum(ss_test, n = 10, align = "right", fill = NA)
  y_c2 <- roll_sum(ss_test, n = 2, align = "right", fill = NA)

  p <- predict(fitX, X_test = X_test, y_test = ss_test)
  p2 <- predict(fitX, n_ahead = 2, X_test = X_test, y_test = ss_test)
  p10 <- predict(fitX, n_ahead = 10, X_test = X_test, y_test = ss_test)

  expect_silent(p_c10 <- predict(fitX,  X_test = X_test,  y_test = ss_test, cumulative = 10))
  expect_silent(p2_c10 <- predict(fitX, X_test = X_test,  y_test = ss_test,  cumulative = 10, n_ahead = 2))
  expect_silent(p10_c2 <- predict(fitX, X_test = X_test,  y_test = ss_test,  cumulative = 2, n_ahead = 10))
  expect_silent(p10_c10 <- predict(fitX, X_test = X_test, y_test = ss_test,   cumulative = 10, n_ahead = 10))

  # check rolling sums for last value
  expect_equal(sum(tail(p, 10)), tail(p_c10, 1))
  expect_equal(sum(tail(p2, 10)), tail(p2_c10, 1))
  expect_equal(sum(tail(p10, 2)), tail(p10_c2, 1))
  expect_equal(sum(tail(p10, 10)), tail(p10_c10, 1))

  # check correlations
  rho_c10_p1 <- cor(p_c10, y_c10, use = "pair")
  rho_c10_p2 <- cor(p2_c10, y_c10, use = "pair")
  rho_c10_p10 <- cor(p10_c10, y_c10, use = "pair")
  expect_gt(rho_c10_p10, 0.992)
  expect_gt(rho_c10_p2, rho_c10_p10)
  expect_gt(rho_c10_p1, rho_c10_p2)
})

test_that("cumulative predict.fastTS when X_test not supplied, y_test supplied", {
  y_c10 <- roll_sum(ss_test, n = 10, align = "right", fill = NA)

  expect_silent(p <- predict(fit, y_test = ss_test, cumulative = 10))
  expect_length(p, length(y_c10))

  expect_gt(rho_1step <- cor(p, y_c10, use = "pairwise.complete"), .99)
  expect_silent(p2 <- predict(fit, y_test = ss_test, n_ahead = 10, cumulative = 10))
  expect_length(p2, length(y_c10))
  expect_gt(rho_2step <- cor(p2, y_c10, use = "pairwise.complete"), 0.98)

  expect_gt(rho_1step, rho_2step)

  # Exogenous model (should not work when X_test not supplied)
  expect_error(p <- predict(fitX, y_test = lh_test, cumulative = 10),
               "Detected exogenous model; for future predictions, you must supply X_test")
})



