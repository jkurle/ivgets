test_that("ivgets() produces correct output", {

  # set up a 2SLS structure (broadly)
  set.seed(123)
  df <- data.frame(u = stats::rnorm(50))
  df$z6 <- stats::rnorm(50) # excluded instrument
  df$z7 <- stats::rnorm(50) # excluded instrument
  df$r <- 0.5 * df$u + stats::rnorm(50) # so r and u are correlated
  df$x6 <- df$z6 - 0.5*df$z7 + df$r # endogenous regressor, relevant
  df$cons <- 1 # intercept, relevant
  df$x1 <- stats::rnorm(50) # exogenous regressor, relevant
  df$x2 <- stats::rnorm(50) # exogenous regressor, irrelevant
  df$x3 <- stats::rnorm(50) # exogenous regressor, irrelevant
  df$x4 <- stats::rnorm(50) # exogenous regressor, irrelevant
  df$x5 <- stats::rnorm(50) # exogenous regressor, irrelevant
  df$y <- df$cons + 2*df$x1 - df$x6 + df$u # coefficients c(1,2,0,0,0,0,-1)
  # delete unobserved errors
  df$u <- NULL
  df$r <- NULL
  # GUM
  fml <- y ~ -1+cons+x1+x2+x3+x4+x5+x6 | -1+cons+x1+x2+x3+x4+x5+z6+z7
  # model <- ivreg::ivreg(formula = fml, data = df) # model seems consistent

  # no keep
  expect_silent(ivgets(formula = fml, data = df, t.pval = 1/100,
                       print.searchinfo = FALSE))
  m1 <- ivgets(formula = fml, data = df, t.pval = 1/100,
               print.searchinfo = FALSE)
  # check basics of the output
  expect_type(m1, "list")
  expect_length(m1, 3L)
  expect_named(m1, c("selection", "final", "keep"))
    # since did not specify 'keep' argument, only endog. regr. should be in keep
  expect_identical(m1$keep, "x6")
    # final model should be an ivreg object
  expect_identical(class(m1$final), "ivreg")
    # selection should be a list with several entries returned by getsFun()
  expect_type(m1$selection, "list")
  expect_named(m1$selection, c("time.started", "time.finished", "call",
                               "no.of.estimations", "paths", "terminals",
                               "terminals.results", "best.terminal",
                               "specific.spec"))
  # have selected the DGP: cons, x1, x6
  expect_identical(names(m1$final$coefficients), c("cons", "x1", "x6"))
  # to save snapshot output, have to delete time b/c will change
  m1$selection$call <- NULL
  m1$selection$time.started <- NULL
  m1$selection$time.finished <- NULL
  expect_snapshot_output(m1, cran = FALSE)

  # have some in keep
  m2 <- ivgets(formula = fml, data = df, t.pval = 1/100,
               keep_exog = c("cons", "x1", "x2"), print.searchinfo = FALSE)
  expect_identical(m2$keep, c("cons", "x1", "x2", "x6"))
  expect_identical(names(m2$final$coefficients), c("cons", "x1", "x2", "x6"))
  m2$selection$call <- NULL
  m2$selection$time.started <- NULL
  m2$selection$time.finished <- NULL
  expect_snapshot_output(m2, cran = FALSE)

  # have diagnostics
  m3 <- ivgets(formula = fml, data = df, t.pval = 1/100, weak = 0.05,
               overid = 0.05, keep_exog = c("cons", "x1"),
               print.searchinfo = FALSE)
  expect_identical(m3$keep, c("cons", "x1", "x6"))
  expect_identical(names(m3$final$coefficients), c("cons", "x1", "x6"))
  # still finds DGP (so diagnostics have not cause to retain more)
  # same coefficient result as m1 (but not overall b/c call is different)
  expect_identical(m3$final$coefficients, m1$final$coefficients)
  m3$selection$call <- NULL
  m3$selection$time.started <- NULL
  m3$selection$time.finished <- NULL
  expect_snapshot_output(m3, cran = FALSE)

  # have more diagnostics
  m4 <- ivgets(formula = fml, data = df, t.pval = 1/100, weak = 0.05,
               overid = 0.05, keep_exog = c("cons", "x1"),
               print.searchinfo = FALSE, normality.JarqueB = 0.05,
               ar.LjungB = c(1, 0.05), arch.LjungB = c(1, 0.05))
  # still finds DGP (so diagnostics have not cause to retain more)
  expect_identical(m4$final$coefficients, m1$final$coefficients)
  m4$selection$call <- NULL
  m4$selection$time.started <- NULL
  m4$selection$time.finished <- NULL
  expect_snapshot_output(m4, cran = FALSE)

  # make selection p-value less strict, so expect to retain more
  m5 <- ivgets(formula = fml, data = df, t.pval = 1/50,
               print.searchinfo = FALSE)
  # now spuriously retain x5
  expect_identical(m5$keep, "x6")
  expect_identical(names(m5$final$coefficients), c("cons", "x1", "x5", "x6"))
  m5$selection$call <- NULL
  m5$selection$time.started <- NULL
  m5$selection$time.finished <- NULL
  expect_snapshot_output(m5, cran = FALSE)

  # can also select keep via indices corresponding to indices in original df
  # should keep "cons" and "x1" (plus endogenous "x6")
  m6 <- ivgets(formula = fml, data = df, t.pval = 1/100, keep = c(4, 5),
               print.searchinfo = FALSE)
  expect_identical(m6$keep, c("cons", "x1", "x6"))
  m6$selection$call <- NULL
  m6$selection$time.started <- NULL
  m6$selection$time.finished <- NULL
  expect_snapshot_output(m6, cran = FALSE)

  # check diagnostics are really used by setting a ridiculous sign. level
  # in this case, GUM might does not pass, so expect warning
  expect_warning(m7 <- ivgets(formula = fml, data = df, t.pval = 1/100,
                              print.searchinfo = FALSE, overid = 0.9),
                 "does not pass one or more diagnostic checks. No selection.")
  expect_identical(m7$final, NULL)
  expect_identical(m7$keep, "x6")

  # use diagnostics where in a path a regressor is added back in due to diagn
  # have to use ridiculous sign. level but is for illustration only
  m8 <- ivgets(formula = fml, data = df, t.pval = 1/100,
               normality.JarqueB = 0.92, print.searchinfo = FALSE)
  expect_identical(names(m8$final$coefficients),
                   c("cons", "x1", "x2", "x5", "x6"))
  expect_identical(names(m8$selection$specific.spec),
                   names(m8$final$coefficients))

  # check different formula and data frame combination (generating intercept)
  # will first get warning as cons is deleted
  # if have specified cons as keep should get error
  fml <- y ~ cons+x1+x2+x3+x4+x5+x6 | cons+x1+x2+x3+x4+x5+z6+z7
  expect_warning(expect_error(ivgets(formula = fml, data = df, t.pval = 1/100,
                      print.searchinfo = FALSE, keep_exog = "cons"),
               "regressor that was specified in 'keep_exog' has been dropped"))
  expect_warning(m9 <- ivgets(formula = fml, data = df, t.pval = 1/100,
               print.searchinfo = FALSE, keep_exog = "x1"))
  expect_identical(m9$keep, c("x1", "x6"))
  expect_identical(names(m9$final$coefficients), c("Intercept", "x1", "x6"))
  # m9 and m6 have same selected but only named differently
  coef9 <- m9$final$coefficients
  coef6 <- m6$final$coefficients
  names(coef9) <- names(coef6) <- NULL
  expect_identical(coef6, coef9)

})

test_that("gets.ivreg() throws correct errors", {

  # set up a 2SLS structure (broadly)
  set.seed(123)
  df <- data.frame(u = stats::rnorm(50))
  df$z6 <- stats::rnorm(50) # excluded instrument
  df$z7 <- stats::rnorm(50) # excluded instrument
  df$r <- 0.5 * df$u + stats::rnorm(50) # so r and u are correlated
  df$x6 <- df$z6 - 0.5*df$z7 + df$r # endogenous regressor, relevant
  df$cons <- 1 # intercept, relevant
  df$x1 <- stats::rnorm(50) # exogenous regressor, relevant
  df$y <- df$cons + 2*df$x1 - df$x6 + df$u # coefficients c(1,2,0,0,0,0,-1)
  # delete unobserved errors
  df$u <- NULL
  df$r <- NULL

  iv <- ivreg::ivreg(y ~ -1+cons+x1+x6 | -1+cons+x1+z6+z7, data = df,
                     model = FALSE)
  expect_error(gets(iv),
               "Please specify 'model = TRUE' in the original function call")
  iv <- ivreg::ivreg(y ~ -1+cons+x1+x6 | -1+cons+x1+z6+z7, data = df,
                     weights = c(rep(1/2, 25), rep(3/2, 25)))
  expect_error(gets(iv), "GETS modelling currently does not support weights")

})


test_that("gets.ivreg() works correctly", {

  # since this function builds on ivgets(), do not test too much here

  # set up a 2SLS structure (broadly)
  set.seed(123)
  df <- data.frame(u = stats::rnorm(50))
  df$z6 <- stats::rnorm(50) # excluded instrument
  df$z7 <- stats::rnorm(50) # excluded instrument
  df$r <- 0.5 * df$u + stats::rnorm(50) # so r and u are correlated
  df$x6 <- df$z6 - 0.5*df$z7 + df$r # endogenous regressor, relevant
  df$cons <- 1 # intercept, relevant
  df$x1 <- stats::rnorm(50) # exogenous regressor, relevant
  df$x2 <- stats::rnorm(50) # exogenous regressor, irrelevant
  df$x3 <- stats::rnorm(50) # exogenous regressor, irrelevant
  df$x4 <- stats::rnorm(50) # exogenous regressor, irrelevant
  df$x5 <- stats::rnorm(50) # exogenous regressor, irrelevant
  df$y <- df$cons + 2*df$x1 - df$x6 + df$u # coefficients c(1,2,0,0,0,0,-1)
  # delete unobserved errors
  df$u <- NULL
  df$r <- NULL
  # GUM
  fml <- y ~ -1+cons+x1+x2+x3+x4+x5+x6 | -1+cons+x1+x2+x3+x4+x5+z6+z7
  base <- ivreg::ivreg(formula = fml, data = df)

  # now do selection from the base model
  s1 <- gets(base, t.pval = 1/100, print.searchinfo = FALSE)
  expect_type(s1, "list")
  expect_length(s1, 3L)
  expect_named(s1, c("selection", "final", "keep"))
  # since did not specify 'keep' argument, only endog. regr. should be in keep
  expect_identical(s1$keep, "x6")
  # final model should be an ivreg object
  expect_identical(class(s1$final), "ivreg")
  # selection should be a list with several entries returned by getsFun()
  expect_type(s1$selection, "list")
  expect_named(s1$selection, c("time.started", "time.finished", "call",
                               "no.of.estimations", "paths", "terminals",
                               "terminals.results", "best.terminal",
                               "specific.spec"))
  # have selected the DGP: cons, x1, x6
  expect_identical(names(s1$final$coefficients), c("cons", "x1", "x6"))
  # s1 should do the same as m1 above
  m1 <- ivgets(formula = fml, data = df, t.pval = 1/100,
               print.searchinfo = FALSE)
  # check whether identical (delete environment and different types of rownames)
  s1$selection$time.started <- NULL
  s1$selection$time.finished <- NULL
  m1$selection$time.started <- NULL
  m1$selection$time.finished <- NULL
  attr(s1$final$formula, ".Environment") <- NULL
  attr(m1$final$formula, ".Environment") <- NULL
  attr(s1$final$terms$regressors, ".Environment") <- NULL
  attr(m1$final$terms$regressors, ".Environment") <- NULL
  attr(s1$final$terms$instruments, ".Environment") <- NULL
  attr(m1$final$terms$instruments, ".Environment") <- NULL
  attr(s1$final$terms$full, ".Environment") <- NULL
  attr(m1$final$terms$full, ".Environment") <- NULL
  attr(attr(s1$final$model, "terms"), ".Environment") <- NULL
  attr(attr(m1$final$model, "terms"), ".Environment") <- NULL
  # now they only differ in that s1 has rownames as character vector while m1 as integer vector
  attr(m1$final$model, "row.names") <- as.character(attr(m1$final$model, "row.names"))
  expect_identical(s1, m1)

})
