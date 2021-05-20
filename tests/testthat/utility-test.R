
test_that("new_formula() creates correct output", {

  df <- data.frame(y = stats::rnorm(10))
  df$cons <- 1 # intercept explicit
  df$x1 <- stats::rnorm(10) # exogenous regressor
  df$x2 <- stats::rnorm(10) # endogenous regressor
  df$z2 <- stats::rnorm(10) # excluded instrument

  # here nothing should need to be done; check -1 and 0 treated equally
  f1 <- y ~ -1 + cons + x1 + x2 | -1 + cons + x1 + z2
  f2 <- y ~ 0 + cons + x1 + x2 | 0 + cons + x1 + z2
  f3 <- y ~ -1 + cons + x1 + x2 | 0 + cons + x1 + z2
  f4 <- y ~ 0 + cons + x1 + x2 | -1 + cons + x1 + z2
  # here should run model without an intercept
  f5 <- y ~ -1 + x1 + x2 | -1 + x1 + z2
  # here should detect collinearity and delete cons but add (Intercept) instead
  f6 <- y ~ cons + x1 + x2 | cons + x1 + z2
  # here should run model with intercept and call it (Intercept)
  f7 <- y ~ x1 + x2 | x1 + z2


  ### test whether have problems with naming of different dependent variable
  new_formula(formula = f1, data = df)
  new_formula(formula = f2, data = df)
  new_formula(formula = f3, data = df)
  new_formula(formula = f4, data = df)
  new_formula(formula = f5, data = df)
  new_formula(formula = f6, data = df)
  new_formula(formula = f7, data = df)

})

test_that("ivgets works correctly", {

  library(r2sls)
  p <- generate_param(10, 2, 3, beta = c(6, 5, 0, 0, 0, 0, 0, 0, 0, 0, -4, 3), sigma = 2)
  d <- generate_data(p, 100)
  df <- d$data

  form <- y ~ -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 | -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13

  ivgets(formula = form, data = df, keep_exog = c("x1"))
  ivgets(formula = form, data = df, keep_exog = c(12, 13))
  ivgets(formula = form, data = df, keep_exog = c("x11", "x12"))
  ivgets(formula = form, data = df, keep_exog = c(4))

  form <- y ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 | x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13

a <- ivgets(formula = form, data = df, keep_exog = c(12, 13))
  ivgets(formula = form, data = df, keep_exog = c(0, 12, 13))



  form <- y ~ -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 | -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13
  ivgets(formula = form, data = df, keep_exog = c("x11", "x12"), overid = 0.05, weak = 0.05)

})
