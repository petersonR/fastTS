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

  expect_silent(p <- predict(fit, n_ahead = 2, return_intermediate = TRUE))
  expect_equal(ncol(p), 2)
  expect_equal(nrow(p), length(lh_train))

  expect_silent(p <- predict(fit, n_ahead = 10, return_intermediate = TRUE))
  expect_equal(ncol(p), 10)
  expect_equal(nrow(p), length(lh_train))

  expect_silent(p <- predict(fit, n_ahead = 10, cumulative = TRUE))
  expect_equal(length(p), length(lh_train))

  expect_silent(p <- predict(fit))

  expect_gt(rho_1step <- cor(p, lh_train, use = "pairwise.complete"), .853)

  expect_silent(p2 <- predict(fit, n_ahead = 2, return_intermediate = TRUE))
  expect_identical(p2$p1, p)

  expect_silent(p <- predict(fit, n_ahead = 2))
  expect_identical(p2$p2, p)

  expect_gt(rho_2step <- cor(p, lh_train, use = "pairwise.complete"), 0.597)

  expect_gt(rho_1step, rho_2step)

  expect_silent(p10 <- predict(fit, n_ahead = 10))
  expect_gt(rho_10step <- cor(p10, lh_train, use = "pairwise.complete"), 0.228)

  # Exogenous model
  expect_silent(p <- predict(fitX))
  expect_length(p, length(lh_train))

  expect_gt(rho_1step <- cor(p, lh_train, use = "pairwise.complete"), .852)

  expect_silent(p2 <- predict(fitX, n_ahead = 2))
  expect_length(p2, length(lh_train))

  expect_gt(rho_2step <- cor(p2, lh_train, use = "pairwise.complete"), 0.598)

  expect_gt(rho_1step, rho_2step)

  expect_silent(p <- predict(fitX, n_ahead = 10))
  expect_length(p, length(lh_train))
  expect_gt(rho_10step <- cor(p, lh_train, use = "pairwise.complete"), 0.228)

  expect_gt(rho_1step, rho_10step)
  expect_gt(rho_2step, rho_10step)

})

test_that("predict.fastTS when X_test not supplied, y_test supplied", {
  expect_silent(p <- predict(fit, y_test = lh_test))
  expect_length(p, length(lh_test))

  expect_gt(rho_1step <- cor(p, lh_test, use = "pairwise.complete"), .75)
  expect_silent(p2 <- predict(fit, y_test = lh_test, n_ahead = 2))
  expect_length(p2, length(lh_test))
  expect_gt(rho_2step <- cor(p2, lh_test, use = "pairwise.complete"), 0.478)

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
  expect_gt(cor(p2, p), 0.629)

  expect_silent(p10 <- predict(fitX, X_test = X_test, y_test = lh_test, n_ahead = 10))
  expect_length(p10, length(X_test))

  expect_error(pmax <- predict(fitX, X_test = X_test))
  expect_silent(pmax <- predict(fitX, X_test = X_test, y_test=lh_test, n_ahead = length(X_test)))

  expect_length(pmax, length(X_test))

  expect_gt(rho_max <- cor(pmax, lh_test, use = "pair"), 0.272)
  expect_gt(rho_1s <- cor(p, lh_test, use = "pair"), 0.75)
  expect_gt(rho_2s <- cor(p2, lh_test, use = "pair"), .474)

  expect_gt(rho_1s, rho_2s)

  # Try intermediate coding
  expect_silent(p <- predict(fitX, X_test = X_test, y_test = lh_test, n_ahead = 10,
                             return_intermediate = TRUE))
  expect_equal(ncol(p), 10)
  expect_equal(nrow(p), length(X_test))
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
  p2 <- predict(fit, n_ahead = 2)[1:ntrain]
  p10 <- predict(fit, n_ahead = 10)[1:ntrain]

  y_c10 <- roll_sum(ss_train, n = 10, align = "right", fill = NA)
  y_c2 <- roll_sum(ss_train, n = 2, align = "right", fill = NA)

  p2_a <- predict(fit, n_ahead = 2, return_intermediate = TRUE)
  p10_a <- predict(fit, n_ahead = 10, return_intermediate = TRUE)

  expect_silent(p10c <- predict(fit, n_ahead = 10, cumulative = TRUE))
  expect_silent(p2c <- predict(fit, cumulative = TRUE, n_ahead = 2))

  # check correlations
  rho_c10 <- cor(p10c, y_c10, use = "pair")
  rho_c2 <- cor(p2c, y_c2, use = "pair")
  expect_gt(rho_c10, .970)
  expect_gt(rho_c2, .960)

  p <- predict(fitX)
  p2 <- predict(fitX, n_ahead = 2)
  p10 <- predict(fitX, n_ahead = 10)

  expect_silent(p10c <- predict(fitX, n_ahead = 10, cumulative = TRUE))
  expect_silent(p2c <- predict(fitX, cumulative = TRUE, n_ahead = 2))

  # check correlations
  rho_c10 <- cor(p10c, y_c10, use = "pair")
  rho_c2 <- cor(p2c, y_c10, use = "pair")
  expect_gt(rho_c10, .970)
  expect_gt(rho_c2, .983)
})

test_that("cumulative predict.fastTS when X_test supplied", {

  # should fail with endogenous model
  expect_error(p <- predict(fit, X_test = X_test, cumulative = TRUE))

  # On existing data (error if y_test not supplied)
  expect_error(p_c10 <- predict(fitX,  X_test = X_test[1:100], cumulative = TRUE))
  expect_silent(p_c10 <- predict(fitX,  X_test = X_test[1:100], y_test = ss_test[1:100], cumulative = TRUE))
  expect_silent(p_c10 <- predict(fitX,  X_test = X_test[1:100], y_test = ss_test[1:100], cumulative = TRUE, n_ahead = 10))

  y_c10 <- roll_sum(ss_test, n = 10, align = "right", fill = NA)
  y_c2 <- roll_sum(ss_test, n = 2, align = "right", fill = NA)

  p <- predict(fitX, X_test = X_test, y_test = ss_test)
  p2 <- predict(fitX, n_ahead = 2, X_test = X_test, y_test = ss_test)
  p10 <- predict(fitX, n_ahead = 10, X_test = X_test, y_test = ss_test)

  expect_silent(p10 <- predict(fitX,  X_test = X_test,  y_test = ss_test, n_ahead = 10,
                               cumulative = TRUE))
  expect_silent(p2 <- predict(fitX,  X_test = X_test,  y_test = ss_test,
                              cumulative = TRUE, n_ahead = 2))

  # check correlations
  rho_10 <- cor(p10, y_c10, use = "pair")
  rho_2 <- cor(p2, y_c2, use = "pair")
  expect_gt(rho_10, 0.978)
  expect_gt(rho_2, 0.966)
})

test_that("cumulative predict.fastTS when X_test not supplied, y_test supplied", {
  y_c10 <- roll_sum(ss_test, n = 10, align = "right", fill = NA)

  expect_silent(p <- predict(fit, y_test = ss_test, cumulative = TRUE))
  expect_length(p, length(y_c10))

  expect_gt(rho_1step <- cor(p, y_c10, use = "pairwise.complete"), .968)
  expect_silent(p2 <- predict(fit, y_test = ss_test, n_ahead = 10, cumulative = TRUE))
  expect_length(p2, length(y_c10))
  expect_gt(rho_2step <- cor(p2, y_c10, use = "pairwise.complete"), 0.978)

  # Exogenous model (should not work when X_test not supplied)
  expect_error(p <- predict(fitX, y_test = lh_test, cumulative = 10),
               "Detected exogenous model; for future predictions, you must supply X_test")
})


test_that("predictions work as expected on uihc data", {
  data("uihc_ed_arrivals")
  y <- uihc_ed_arrivals$Arrivals[1:5000]
  expect_silent(fit <- fastTS(y, ptrain = 0.9))

  expect_silent(p_1step_endo <- predict(fit))

  expect_silent(p_2step_endo <- predict(fit, n_ahead = 2))

  expect_silent(p_10step_endo <- predict(fit, n_ahead = 10))

  rho_1_endo <- cor(p_1step_endo, y, use="pairwise")

  rho_2_endo <- cor(p_2step_endo, y, use="pairwise")

  rho_10_endo <- cor(p_10step_endo, y, use="pairwise")

  expect_gt(rho_1_endo, .754)
  expect_gt(rho_2_endo, .752)
  expect_gt(rho_10_endo, .751)

  ## Cumulative
  y_c10hr <- RcppRoll::roll_sum(y, 10, align = "right", fill = NA)
  p_c1_10hr <-  RcppRoll::roll_sum(p_1step_endo, 10, align = "right", fill = NA)
  p_c10_10hr <-  RcppRoll::roll_sum(p_10step_endo, 10, align = "right", fill = NA)

  rho_1pc_endo <- cor(p_c1_10hr, y_c10hr, use="pairwise")
  rho_10pc_endo <- cor(p_c10_10hr, y_c10hr, use="pairwise")

  expect_gt(rho_1pc_endo, .939)
  expect_gt(rho_10pc_endo, .917)

  p_10step_csum_endo <- predict(fit, n_ahead = 10, cumulative = TRUE)

  rho_10c_endo <- cor(p_10step_csum_endo, y_c10hr, use="pairwise")

  expect_gt(rho_10c_endo, 0.923)
})

test_that("Forecasting", {
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

  # endogenous model
  expect_silent(p <- predict(fit, n_ahead = 10, forecast_ahead = TRUE))
  expect_length(p, 10)

  expect_gt(cor(p, lh_test[1:10]), 0.67)

  # exogenous model, n_ahead doesn't match X_test
  expect_warning(p <- predict(fitX, X_test = X_test, n_ahead = 10, forecast_ahead = TRUE))
  expect_length(p, 18)

  # exogenous model, n_ahead matches X_test
  expect_silent(p <- predict(fitX, X_test = X_test[1:10], n_ahead = 10, forecast_ahead = TRUE))
  expect_length(p, 10)

  expect_gt(cor(p, lh_test[1:10]), 0.538)

})
