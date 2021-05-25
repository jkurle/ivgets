test_that("ivisat() works correctly", {

  # set up a 2SLS structure (broadly)
  set.seed(123)
  df <- data.frame(u = stats::rnorm(50))
  df$z2 <- stats::rnorm(50) # excluded instrument
  df$z3 <- stats::rnorm(50) # excluded instrument
  df$r <- 0.5 * df$u + stats::rnorm(50) # so r and u are correlated
  df$x2 <- df$z2 - 0.5*df$z3 + df$r # endogenous regressor, relevant
  df$cons <- 1 # intercept, relevant
  df$x1 <- stats::rnorm(50) # exogenous regressor, relevant
  df$y <- df$cons + 2*df$x1 - df$x2 + df$u # coefficients c(1,2,-1)
  # delete unobserved errors
  df$u <- NULL
  df$r <- NULL
  # GUM
  fml <- y ~ -1+cons+x1+x2 | -1+cons+x1+z2+z3
  model <- ivreg::ivreg(formula = fml, data = df) # model seems consistent

  m1 <- ivisat(formula = fml, data = df, iis = TRUE, print.searchinfo = FALSE)
  # check basic output
  expect_type(m1, "list")
  expect_length(m1, 2L)
  expect_named(m1, c("selection", "final"))
  expect_identical(class(m1$final), "ivreg")
  expect_identical(class(m1$selection), "isat")
  expect_identical(m1$selection$ISnames, c("iis16", "iis18"))
  expect_true(all(m1$selection$ISnames %in% names(m1$final$coefficients)))
  # order may be different, so more robust to check they are the same sets
  expect_true(setequal(names(m1$final$coefficients),
                       c("cons", "x1", "x2", "iis16", "iis18")))
  m1$selection$time.started <- NULL
  m1$selection$time.finished <- NULL
  expect_snapshot_output(m1)

  m2 <- ivisat(formula = fml, data = df, iis = FALSE, sis = TRUE,
               print.searchinfo = FALSE) # nothing retained
  expect_type(m2, "list")
  expect_length(m2, 2L)
  expect_named(m2, c("selection", "final"))
  expect_identical(class(m2$final), "ivreg")
  expect_identical(class(m2$selection), "isat")
  expect_true(setequal(names(m2$final$coefficients), c("cons", "x1", "x2")))
  expect_null(m2$selection$ISnames)
  m2$selection$time.started <- NULL
  m2$selection$time.finished <- NULL
  expect_snapshot_output(m2)

  m3 <- ivisat(formula = fml, data = df, iis = FALSE, sis = FALSE, tis = TRUE,
               print.searchinfo = FALSE) # many retained
  expect_type(m3, "list")
  expect_length(m3, 2L)
  expect_named(m3, c("selection", "final"))
  expect_identical(class(m3$final), "ivreg")
  expect_identical(class(m3$selection), "isat")
  expect_true(setequal(names(m3$final$coefficients),
                       c("cons", "x1", "x2", "tis8", "tis9", "tis12", "tis16",
                         "tis17", "tis19", "tis20")))











})
