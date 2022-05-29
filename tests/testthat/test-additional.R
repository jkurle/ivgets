test_that("ivregFun() works correctly number 2", {


  set.seed(1)
  y <- stats::rnorm(10)
  z <- stats::rnorm(10)
  colnames(z)
  fml <- y ~ x1 | z2

  out <- ivregFun(y = y, x = NULL, z = z, formula = fml, tests = TRUE)
  expect_equal(out$k, 0L)
  expect_equal(out$df, 10L)

  out <- ivregFun(y = y, x = matrix(NA, nrow = 10, ncol = 0), z = z, formula = fml, tests = TRUE)
  expect_equal(out$k, 0L)
  expect_equal(out$df, 10L)


  set.seed(123)
  df <- data.frame(y = stats::rnorm(10))
  df$cons <- 1
  df$x1 <- stats::rnorm(10) # exogenous regressor
  df$x2 <- stats::rnorm(10) # endogenous regressor
  df$z2 <- stats::rnorm(10) # excluded instrument
  y <- as.vector(df$y)
  x <- as.matrix(df[, c("cons", "x1", "x2"), drop = FALSE])
  z <- as.matrix(df[, c("z2"), drop = FALSE])
  fml <- y ~ -1+cons+x1+x2 | -1+cons+x1+z2
  out <- ivregFun(y = y, x = NULL, z = z, formula = fml, tests = TRUE)

  expect_type(out, "list")
  expect_length(out, 9)
  expect_named(out, c("n", "k", "df", "coefficients", "vcov", "logl", "diag",
                      "residuals", "std.residuals"))
  expect_equal(out$n, 10L)
  expect_equal(out$k, 0L)
  expect_equal(out$df, 10L)
  expect_identical(out$coefficients, NULL)
  expect_identical(out$vcov, NULL)
  expect_type(out$logl, "double")
  expect_length(out$logl, 1L)
  expect_identical(out$diag, NULL)
  expect_type(out$residuals, "double")
  expect_identical(class(out$residuals), "numeric")
  expect_length(out$residuals, 10L)
  expect_identical(out$residuals, y)
  expect_type(out$std.residuals, "double")
  expect_identical(class(out$std.residuals), "numeric")
  expect_length(out$std.residuals, 10L)

  # must be able to handle empty models
  out <- ivregFun(y = y, x = matrix(nrow = 0, ncol = 0), z = z, formula = fml, tests = TRUE)
  expect_type(out, "list")
  expect_length(out, 9)
  expect_named(out, c("n", "k", "df", "coefficients", "vcov", "logl", "diag",
                      "residuals", "std.residuals"))
  expect_equal(out$n, 10L)
  expect_equal(out$k, 0L)
  expect_equal(out$df, 10L)
  expect_identical(out$coefficients, NULL)
  expect_identical(out$vcov, NULL)
  expect_type(out$logl, "double")
  expect_length(out$logl, 1L)
  expect_identical(out$diag, NULL)
  expect_type(out$residuals, "double")
  expect_identical(class(out$residuals), "numeric")
  expect_length(out$residuals, 10L)
  expect_identical(out$residuals, y)
  expect_type(out$std.residuals, "double")
  expect_identical(class(out$std.residuals), "numeric")
  expect_length(out$std.residuals, 10L)

  # must be able to handle models where x deviates from "formula x"
  # e.g. if path search and have deleted some regressors
  x <- x[, -1, drop = FALSE] # suppose cons has been deleted
  out <- ivregFun(y = y, x = x, z = z, formula = fml, tests = TRUE)
  expect_type(out, "list")
  expect_length(out, 9)
  expect_named(out, c("n", "k", "df", "coefficients", "vcov", "logl", "diag",
                      "residuals", "std.residuals"))
  expect_identical(out$n, 10L)
  expect_identical(out$k, 2L)
  expect_identical(out$df, 8L)
  expect_length(out$coefficients, 2L)
  expect_named(out$coefficients, c("x1", "x2"))
  expect_identical(class(out$vcov), c("matrix", "array"))
  expect_identical(colnames(out$vcov), c("x1", "x2"))
  expect_identical(rownames(out$vcov), c("x1", "x2"))
  expect_type(out$logl, "double")
  expect_length(out$logl, 1L)
  expect_identical(class(out$diag), c("matrix", "array"))
  expect_identical(colnames(out$diag), c("df1", "df2", "statistic", "p-value"))
  expect_identical(rownames(out$diag),
                   c("Weak instruments", "Wu-Hausman", "Sargan"))
  expect_type(out$residuals, "double")
  expect_identical(class(out$residuals), "numeric")
  expect_length(out$residuals, 10L)
  expect_type(out$std.residuals, "double")
  expect_identical(class(out$std.residuals), "numeric")
  expect_length(out$std.residuals, 10L)

  # now suppose that x1 has been deleted
  x <- as.matrix(df[, c("cons", "x2"), drop = FALSE])
  out <- ivregFun(y = y, x = x, z = z, formula = fml, tests = TRUE)
  expect_type(out, "list")
  expect_length(out, 9)
  expect_named(out, c("n", "k", "df", "coefficients", "vcov", "logl", "diag",
                      "residuals", "std.residuals"))
  expect_identical(out$n, 10L)
  expect_identical(out$k, 2L)
  expect_identical(out$df, 8L)
  expect_length(out$coefficients, 2L)
  expect_named(out$coefficients, c("cons", "x2"))
  expect_identical(class(out$vcov), c("matrix", "array"))
  expect_identical(colnames(out$vcov), c("cons", "x2"))
  expect_identical(rownames(out$vcov), c("cons", "x2"))
  expect_type(out$logl, "double")
  expect_length(out$logl, 1L)
  expect_identical(class(out$diag), c("matrix", "array"))
  expect_identical(colnames(out$diag), c("df1", "df2", "statistic", "p-value"))
  expect_identical(rownames(out$diag),
                   c("Weak instruments", "Wu-Hausman", "Sargan"))
  expect_type(out$residuals, "double")
  expect_identical(class(out$residuals), "numeric")
  expect_length(out$residuals, 10L)
  expect_type(out$std.residuals, "double")
  expect_identical(class(out$std.residuals), "numeric")
  expect_length(out$std.residuals, 10L)

  # must be able to handle additional regressors, if added indicators in isat()
  x <- as.matrix(df[, c("cons", "x1", "x2"), drop = FALSE])
  iis3 <- diag(10)[, 3, drop = FALSE]
  colnames(iis3) <- "iis3"
  x <- cbind(iis3, x)
  out <- ivregFun(y = y, x = x, z = z, formula = fml, tests = TRUE)

  expect_type(out, "list")
  expect_length(out, 9)
  expect_named(out, c("n", "k", "df", "coefficients", "vcov", "logl", "diag",
                      "residuals", "std.residuals"))
  expect_identical(out$n, 10L)
  expect_identical(out$k, 4L)
  expect_identical(out$df, 6L)
  expect_length(out$coefficients, 4L)
  expect_named(out$coefficients, c("iis3", "cons", "x1", "x2"))
  expect_identical(class(out$vcov), c("matrix", "array"))
  expect_identical(colnames(out$vcov), c("iis3", "cons", "x1", "x2"))
  expect_identical(rownames(out$vcov), c("iis3", "cons", "x1", "x2"))
  expect_type(out$logl, "double")
  expect_length(out$logl, 1L)
  expect_identical(class(out$diag), c("matrix", "array"))
  expect_identical(colnames(out$diag), c("df1", "df2", "statistic", "p-value"))
  expect_identical(rownames(out$diag),
                   c("Weak instruments", "Wu-Hausman", "Sargan"))
  expect_type(out$residuals, "double")
  expect_identical(class(out$residuals), "numeric")
  expect_length(out$residuals, 10L)
  expect_type(out$std.residuals, "double")
  expect_identical(class(out$std.residuals), "numeric")
  expect_length(out$std.residuals, 10L)

  # check that position of where iis is added does not matter
  x <- as.matrix(df[, c("cons", "x1", "x2"), drop = FALSE])
  x <- cbind(x[, 1, drop = FALSE], iis3, x[, -1, drop = FALSE]) # is now second
  out2 <- ivregFun(y = y, x = x, z = z, formula = fml, tests = TRUE)

  expect_type(out2, "list")
  expect_length(out2, 9)
  expect_named(out2, c("n", "k", "df", "coefficients", "vcov", "logl", "diag",
                       "residuals", "std.residuals"))
  expect_identical(out2$n, 10L)
  expect_identical(out2$k, 4L)
  expect_identical(out2$df, 6L)
  expect_length(out2$coefficients, 4L)
  expect_named(out2$coefficients, c("cons", "iis3", "x1", "x2"))
  expect_identical(class(out2$vcov), c("matrix", "array"))
  expect_identical(colnames(out2$vcov), c("cons", "iis3", "x1", "x2"))
  expect_identical(rownames(out2$vcov), c("cons", "iis3", "x1", "x2"))
  expect_type(out2$logl, "double")
  expect_length(out2$logl, 1L)
  expect_identical(class(out2$diag), c("matrix", "array"))
  expect_identical(colnames(out2$diag), c("df1", "df2", "statistic", "p-value"))
  expect_identical(rownames(out2$diag),
                   c("Weak instruments", "Wu-Hausman", "Sargan"))
  expect_type(out2$residuals, "double")
  expect_identical(class(out2$residuals), "numeric")
  expect_length(out2$residuals, 10L)
  expect_type(out2$std.residuals, "double")
  expect_identical(class(out2$std.residuals), "numeric")
  expect_length(out2$std.residuals, 10L)

  expect_equal(out$residuals, out2$residuals)
  expect_equal(out$std.residuals, out2$std.residuals)

  # try more indicators
  x <- as.matrix(df[, c("cons", "x1", "x2"), drop = FALSE])
  sis8 <- as.matrix(c(0,0,0,0,0,0,0,1,1,1), nrow = 10, ncol = 1)
  colnames(sis8) <- "sis8"
  x <- cbind(iis3, sis8, x)
  out <- ivregFun(y = y, x = x, z = z, formula = fml, tests = TRUE)

  expect_type(out, "list")
  expect_length(out, 9)
  expect_named(out, c("n", "k", "df", "coefficients", "vcov", "logl", "diag",
                      "residuals", "std.residuals"))
  expect_identical(out$n, 10L)
  expect_identical(out$k, 5L)
  expect_identical(out$df, 5L)
  expect_length(out$coefficients, 5L)
  expect_named(out$coefficients, c("iis3", "sis8", "cons", "x1", "x2"))
  expect_identical(class(out$vcov), c("matrix", "array"))
  expect_identical(colnames(out$vcov), c("iis3", "sis8", "cons", "x1", "x2"))
  expect_identical(rownames(out$vcov), c("iis3", "sis8", "cons", "x1", "x2"))
  expect_type(out$logl, "double")
  expect_length(out$logl, 1L)
  expect_identical(class(out$diag), c("matrix", "array"))
  expect_identical(colnames(out$diag), c("df1", "df2", "statistic", "p-value"))
  expect_identical(rownames(out$diag),
                   c("Weak instruments", "Wu-Hausman", "Sargan"))
  expect_type(out$residuals, "double")
  expect_identical(class(out$residuals), "numeric")
  expect_length(out$residuals, 10L)
  expect_type(out$std.residuals, "double")
  expect_identical(class(out$std.residuals), "numeric")
  expect_length(out$std.residuals, 10L)

})

test_that("ivDiag() works correctly number 2", {

  # takes an object returned by ivregFun()
  # base setup
  set.seed(123)
  df <- data.frame(y = stats::rnorm(10))
  df$cons <- 1
  df$x1 <- stats::rnorm(10) # exogenous regressor
  df$x2 <- stats::rnorm(10) # endogenous regressor
  df$z2 <- stats::rnorm(10) # excluded instrument
  df$z3 <- stats::rnorm(10) # excluded instrument
  y <- as.vector(df$y)
  x <- as.matrix(df[, c("cons", "x1", "x2"), drop = FALSE])
  z <- as.matrix(df[, c("z2", "z3"), drop = FALSE])
  fml <- y ~ -1+cons+x1+x2 | -1+cons+x1+z2+z3
  out <- ivregFun(y = y, x = x, z = z, formula = fml, tests = TRUE)

  # no misspecification test specified
  expect_identical(ivDiag(x = out, weak = FALSE, overid = FALSE), NULL)

  # specify weak instrument test
  d <- ivDiag(x = out, weak = TRUE, overid = FALSE)
  expect_identical(class(d), c("matrix", "array"))
  expect_identical(dim(d), c(1L, 3L))
  expect_identical(colnames(d), c("statistic", "df", "p-value"))
  expect_identical(rownames(d), "Weak instruments")
  # weak instruments test has df1 and df2, so set df to NA
  expect_identical(d["Weak instruments", "df"], as.double(NA))
  # attribute not used; keep test for now
  expect_identical(attr(d, "is.reject.bad"), FALSE) # want to reject weak

  # specify overid test
  d <- ivDiag(x = out, weak = FALSE, overid = TRUE)
  expect_identical(class(d), c("matrix", "array"))
  expect_identical(dim(d), c(1L, 3L))
  expect_identical(colnames(d), c("statistic", "df", "p-value"))
  expect_identical(rownames(d), "Sargan")
  # attribute not used; keep test for now
  expect_identical(attr(d, "is.reject.bad"), TRUE)

  # specify both tests
  d <- ivDiag(x = out, weak = TRUE, overid = TRUE)
  expect_identical(class(d), c("matrix", "array"))
  expect_identical(dim(d), c(2L, 3L))
  expect_identical(colnames(d), c("statistic", "df", "p-value"))
  expect_identical(rownames(d), c("Weak instruments", "Sargan"))
  # weak instruments test has df1 and df2, so set df to NA
  expect_identical(d["Weak instruments", "df"], as.double(NA))
  # attribute not used; keep test for now
  expect_identical(attr(d, "is.reject.bad"), c(FALSE, TRUE))

  # weak instrument test when have more than one endogenous regressor
  df$x3 <- stats::rnorm(10) # endogenous regressor
  x <- as.matrix(df[, c("cons", "x1", "x2", "x3"), drop = FALSE])
  fml <- y ~ -1+cons+x1+x2+x3 | -1+cons+x1+z2+z3
  out <- ivregFun(y = y, x = x, z = z, formula = fml, tests = TRUE)
  d <- ivDiag(x = out, weak = TRUE, overid = FALSE) # now have two tests weak
  expect_identical(class(d), c("matrix", "array"))
  expect_identical(dim(d), c(2L, 3L))
  expect_identical(colnames(d), c("statistic", "df", "p-value"))
  expect_identical(rownames(d),
                   c("Weak instruments (x2)", "Weak instruments (x3)"))
  # weak instruments test has df1 and df2, so set df to NA
  expect_identical(as.vector(d[c("Weak instruments (x2)", "Weak instruments (x3)"), "df"]),
                   c(as.double(NA), as.double(NA)))
  # attribute not used; keep test for now
  expect_identical(attr(d, "is.reject.bad"), c(FALSE, FALSE))

})
