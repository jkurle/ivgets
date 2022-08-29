# note: in likely update of gets (version 0.36 or 0.37), isat stores additional info in selection$aux$args
# this is different from my own arguments stored in selection$aux$arguments

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

  # iis and others often don't run here because blocks take away too many obs
  # in these cases use max.block.size argument

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
  m1$selection$date <- NULL
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
  m2$selection$date <- NULL
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
  m3$selection$time.started <- NULL
  m3$selection$time.finished <- NULL
  m3$selection$date <- NULL
  expect_snapshot_output(m3)

  # test uis; use iis indicators but call them different
  # same model setup as before, so should retain "my16" and "my18"
  uismatrix <- diag(50)
  names <- NULL
  for (i in 1:50) {
    new <- paste("my", i, sep = "")
    names <- cbind(names, new)
  }
  colnames(uismatrix) <- names
  m4 <- ivisat(formula = fml, data = df, iis = FALSE, print.searchinfo = FALSE,
               uis = uismatrix)
  expect_type(m4, "list")
  expect_length(m4, 2L)
  expect_named(m4, c("selection", "final"))
  expect_identical(class(m4$final), "ivreg")
  expect_identical(class(m4$selection), "isat")
  expect_identical(m4$selection$ISnames, c("my16", "my18"))
  expect_true(all(m4$selection$ISnames %in% names(m4$final$coefficients)))
  # order may be different, so more robust to check they are the same sets
  expect_true(setequal(names(m4$final$coefficients),
                       c("cons", "x1", "x2", "my16", "my18")))
  m4$selection$time.started <- NULL
  m4$selection$time.finished <- NULL
  m4$selection$date <- NULL
  expect_snapshot_output(m4)

  # test uis as list with iis indicators
  uislist <- list(matrix1 = uismatrix[, 1:25], matrix2 = uismatrix[, 26:50])
  m5 <- ivisat(formula = fml, data = df, iis = FALSE, print.searchinfo = FALSE,
               uis = uislist)
  expect_type(m5, "list")
  expect_length(m5, 2L)
  expect_named(m5, c("selection", "final"))
  expect_identical(class(m5$final), "ivreg")
  expect_identical(class(m5$selection), "isat")
  expect_identical(m5$selection$ISnames, c("my16", "my18"))
  expect_true(all(m5$selection$ISnames %in% names(m5$final$coefficients)))
  # order may be different, so more robust to check they are the same sets
  expect_true(setequal(names(m5$final$coefficients),
                       c("cons", "x1", "x2", "my16", "my18")))
  m5$selection$time.started <- NULL
  m5$selection$time.finished <- NULL
  m5$selection$date <- NULL
  expect_snapshot_output(m5)

  # test diagnostics
  expect_silent(m6 <- ivisat(formula = fml, data = df, iis = TRUE,
                              print.searchinfo = FALSE,
                              overid = 0.05, weak = 0.05))
  # make diagnostics unlikely to pass
  expect_warning(m7 <- ivisat(formula = fml, data = df, iis = TRUE,
                              print.searchinfo = FALSE,
                              overid = 0.95, weak = 0.05),
                 "No selection was undertaken")
  expect_identical(m7$selection$no.of.estimations,
                   m7$selection$no.of.getsFun.calls)
  expect_identical(m7$selection$no.of.estimations, 3)
  expect_identical(m7$final, NULL)

})

test_that("isat.ivreg() works correctly", {

  # since this function builds on ivisat(), do not test too much here

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
  base <- ivreg::ivreg(formula = fml, data = df) # model seems consistent

  # now do selection from base model
  s1 <- isat(base, print.searchinfo = FALSE)
  # check basic output
  expect_type(s1, "list")
  expect_length(s1, 2L)
  expect_named(s1, c("selection", "final"))
  expect_identical(class(s1$final), "ivreg")
  expect_identical(class(s1$selection), "isat")
  expect_identical(s1$selection$ISnames, c("iis16", "iis18"))
  expect_true(all(s1$selection$ISnames %in% names(s1$final$coefficients)))
  # order may be different, so more robust to check they are the same sets
  expect_true(setequal(names(s1$final$coefficients),
                       c("cons", "x1", "x2", "iis16", "iis18")))
  # s1 does the same as m1 above
  m1 <- ivisat(formula = fml, data = df, iis = TRUE, print.searchinfo = FALSE)

  s1$selection$time.started <- NULL
  s1$selection$time.finished <- NULL
  s1$selection$date <- NULL
  m1$selection$time.started <- NULL
  m1$selection$time.finished <- NULL
  m1$selection$date <- NULL
  # m1 doesn't record rownames of user.estimator$z (not sure why not)
  rownames(s1$selection$aux$user.estimator$z) <- NULL
  attr(s1$selection$aux$user.estimator$formula, ".Environment") <- NULL
  attr(m1$selection$aux$user.estimator$formula, ".Environment") <- NULL
  rownames(s1$selection$aux$arguments$mxreg) <- NULL
  rownames(s1$selection$aux$arguments$user.estimator$z) <- NULL
  attr(m1$selection$aux$arguments$user.estimator$formula, '.Environment') <- NULL
  attr(s1$selection$aux$arguments$user.estimator$formula, '.Environment') <- NULL
  attr(m1$final$formula, '.Environment') <- NULL
  attr(s1$final$formula, '.Environment') <- NULL
  attr(m1$final$terms$regressors, '.Environment') <- NULL
  attr(s1$final$terms$regressors, '.Environment') <- NULL
  attr(m1$final$terms$instruments, '.Environment') <- NULL
  attr(s1$final$terms$instruments, '.Environment') <- NULL
  attr(s1$final$terms$full, '.Environment') <- NULL
  attr(m1$final$terms$full, '.Environment') <- NULL
  attr(attr(m1$final$model, 'terms'), '.Environment') <- NULL
  attr(attr(s1$final$model, 'terms'), '.Environment') <- NULL
  attr(m1$final$model, 'row.names') <- as.character(attr(m1$final$model, 'row.names'))
  attr(s1$selection$aux$arguments$y, "names") <- NULL

  # gets now also stores arguments in selection$aux$args, need to mute them for comparison
  rownames(s1$selection$aux$args$user.estimator$z) <- NULL
  attr(m1$selection$aux$args$user.estimator$formula, '.Environment') <- NULL
  attr(s1$selection$aux$args$user.estimator$formula, '.Environment') <- NULL

  expect_identical(m1, s1)

})

