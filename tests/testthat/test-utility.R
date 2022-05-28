test_that("extract_variables() throws correct errors", {

  f1 <- y ~ x1 + x2 + x3 + z4
  f2 <- 0
  expect_error(extract_variables(f1), "not of the required format")
  expect_error(extract_variables(f2), "not of the required format")

  f3 <- y ~ 1 + x1 + x2 | 1 + x1 + z2 | z3
  expect_error(extract_variables(f3), "does not consist of three parts")

  f4 <- ~ x1 + x2 + x3 | x1 + z2 + z3
  expect_error(extract_variables(f4), "does not specify any dependent variable")

})

test_that("extract_variables() works correctly", {

  f1 <- y ~ x1 + x2 | x1 + z2
  f2 <- y ~ -1 + x1 + x2 | -1 + x1 + z2
  f3 <- y ~ 0 + x1 + x2 | 0 + x1 + z2
  f4 <- y ~ -1 + x1 + x2 | 0 + x1 + z2
  f5 <- y ~ 0 + x1 + x2 | -1 + x1 + z2
  f6 <- y ~ -1 + x1 + x2 + x3 + x4 + x5 | -1 + x1 + x2 + x3 + x4 + z5 + z6 + z7
  f7 <- yy ~ -1 + x1 + x2 + x3 + x4 + x5 | -1 + x1 + x2 + x3 + x4 + z5 + z6 + z7

  expect_type(extract_variables(f1), "list")
  expect_type(extract_variables(f2), "list")
  expect_type(extract_variables(f3), "list")
  expect_type(extract_variables(f4), "list")
  expect_type(extract_variables(f5), "list")
  expect_type(extract_variables(f6), "list")
  expect_type(extract_variables(f7), "list")
  expect_length(extract_variables(f1), 3)
  expect_length(extract_variables(f2), 3)
  expect_length(extract_variables(f3), 3)
  expect_length(extract_variables(f4), 3)
  expect_length(extract_variables(f5), 3)
  expect_length(extract_variables(f6), 3)
  expect_length(extract_variables(f7), 3)
  names <- c("yvar", "first", "second")
  expect_named(extract_variables(f1), names)
  expect_named(extract_variables(f2), names)
  expect_named(extract_variables(f3), names)
  expect_named(extract_variables(f4), names)
  expect_named(extract_variables(f5), names)
  expect_named(extract_variables(f6), names)
  expect_named(extract_variables(f7), names)

  expect_identical(extract_variables(f1)$yvar, "y")
  expect_identical(extract_variables(f2)$yvar, "y")
  expect_identical(extract_variables(f3)$yvar, "y")
  expect_identical(extract_variables(f4)$yvar, "y")
  expect_identical(extract_variables(f5)$yvar, "y")
  expect_identical(extract_variables(f6)$yvar, "y")
  expect_identical(extract_variables(f7)$yvar, "yy")

  stage1 <- c("x1", "z2")
  expect_identical(extract_variables(f1)$first, stage1)
  expect_identical(extract_variables(f2)$first, c("-1", stage1))
  expect_identical(extract_variables(f3)$first, c("0", stage1))
  expect_identical(extract_variables(f4)$first, c("0", stage1))
  expect_identical(extract_variables(f5)$first, c("-1", stage1))
  stage1 <- c("-1", "x1", "x2", "x3", "x4", "z5", "z6", "z7")
  expect_identical(extract_variables(f6)$first, stage1)
  expect_identical(extract_variables(f7)$first, stage1)

  stage2 <- c("x1", "x2")
  expect_identical(extract_variables(f1)$second, stage2)
  expect_identical(extract_variables(f2)$second, c("-1", stage2))
  expect_identical(extract_variables(f3)$second, c("0", stage2))
  expect_identical(extract_variables(f4)$second, c("-1", stage2))
  expect_identical(extract_variables(f5)$second, c("0", stage2))
  stage2 <- c("-1", "x1", "x2", "x3", "x4", "x5")
  expect_identical(extract_variables(f6)$second, stage2)
  expect_identical(extract_variables(f7)$second, stage2)

})

test_that("new_formula() throws correct error", {

  f1 <- y ~ x1 + x2 | x1 + z2
  df <- data.frame(y = stats::rnorm(10))
  df$x1 <- stats::rnorm(10) # exogenous regressor
  df$x2 <- stats::rnorm(10) # endogenous regressor
  df$z2 <- stats::rnorm(10) # excluded instrument
  expect_error(new_formula(f1, df, keep_exog = TRUE),
               "must either be NULL, numeric, or a character")
  expect_error(new_formula(f1, df, keep_exog = x ~ y),
               "must either be NULL, numeric, or a character")
  expect_error(new_formula(f1, df, keep_exog = list(c(1,2), c("x1"))),
               "must either be NULL, numeric, or a character")

  f2 <- y ~ x1 + x2 | -1 + x1 + z2
  f3 <- y ~ x1 + x2 | 0 + x1 + z2
  expect_error(new_formula(f2, df, NULL),
               "If have intercept in the structural equation, it should also be in the first stage")
  expect_error(new_formula(f3, df, NULL),
               "If have intercept in the structural equation, it should also be in the first stage")

  f4 <- y ~ -1 + x1 + x2 | x1 + z2
  f5 <- y ~ 0 + x1 + x2 | x1 + z2
  expect_error(new_formula(f4, df, NULL),
               "Using intercept as excluded instrument in the first stage is currently not supported")
  expect_error(new_formula(f5, df, NULL),
               "Using intercept as excluded instrument in the first stage is currently not supported")

  f6 <- y ~ Intercept + x1 + x2 | Intercept + x1 + z2
  df$Intercept <- 1
  expect_error(new_formula(f6, df, NULL),
               "Formula specification creates an intercept but regressor named \"Intercept\" is also included in formula")

  df$cons <- 2
  f7 <- y ~ Intercept + cons + x1 + x2 | Intercept + cons + x1 + z2
  expect_error(new_formula(f7, df, NULL),
               "Original formula specification has perfect collinearity")

  f8 <- y ~ cons + x1 + x2 | cons + x1 + z2
  expect_warning(new_formula(f8, df, NULL),
                 "Intercept has introduced collinearity. Drop one regressor")

  # should also get collinearity error when add intecept to two dummies
  df$dummy1 <- c(rep(1, 5), rep(0, 5))
  df$dummy2 <- c(rep(0, 5), rep(1, 5))
  f9 <- y ~ dummy1 + dummy2 + x1 + x2 | dummy1 + dummy2 + x1 + z2
  expect_warning(new_formula(f9, df, NULL),
                 "Intercept has introduced collinearity. Drop one regressor")

  # here drops cons due to collinearity but was specified as keep
  f10 <- y ~ cons + x1 + x2 | cons + x1 + z2
  expect_warning(expect_error(new_formula(f10, df, keep_exog = "cons")))

  # keep_names with name not in colnames of data
  expect_error(new_formula(y ~ x1+x2|x1+z2, df, keep_exog = "nonexist"),
               "Argument 'keep_exog' specifies names that cannot be found in the data frame")

})

test_that("new_formula() works correctly", {

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
  # here should detect collinearity and delete cons but add Intercept instead
  f6 <- y ~ cons + x1 + x2 | cons + x1 + z2
  # here should run model with intercept and call it Intercept
  f7 <- y ~ x1 + x2 | x1 + z2

  call1 <- new_formula(f1, data = df, keep_exog = NULL)
  call2 <- new_formula(f2, data = df, keep_exog = NULL)
  call3 <- new_formula(f3, data = df, keep_exog = NULL)
  call4 <- new_formula(f4, data = df, keep_exog = NULL)
  call5 <- new_formula(f5, data = df, keep_exog = NULL)
  # wrap this in expect_warning because otherwise things the test failed
  expect_warning(call6 <- new_formula(f6, data = df, keep_exog = NULL))
  call7 <- new_formula(f7, data = df, keep_exog = NULL)

  expect_type(call1, "list")
  expect_type(call2, "list")
  expect_type(call3, "list")
  expect_type(call4, "list")
  expect_type(call5, "list")
  expect_type(call6, "list")
  expect_type(call7, "list")

  expect_length(call1, 15)
  expect_length(call2, 15)
  expect_length(call3, 15)
  expect_length(call4, 15)
  expect_length(call5, 15)
  expect_length(call6, 15)
  expect_length(call7, 15)

  names <- c("fml", "y", "x", "z", "depvar", "x1", "x2", "z1", "z2", "dx1",
             "dx2", "dz1", "dz2", "keep", "keep.names")

  expect_named(call1, names)
  expect_named(call2, names)
  expect_named(call3, names)
  expect_named(call4, names)
  expect_named(call5, names)
  expect_named(call6, names)
  expect_named(call7, names)

  expect_identical(call1$fml, "y ~ -1+cons+x1+x2 | -1+cons+x1+z2")
  expect_identical(call2$fml, "y ~ -1+cons+x1+x2 | -1+cons+x1+z2")
  expect_identical(call3$fml, "y ~ -1+cons+x1+x2 | -1+cons+x1+z2")
  expect_identical(call4$fml, "y ~ -1+cons+x1+x2 | -1+cons+x1+z2")
  expect_identical(call5$fml, "y ~ -1+x1+x2 | -1+x1+z2")
  expect_identical(call6$fml, "y ~ -1+Intercept+x1+x2 | -1+Intercept+x1+z2")
  expect_identical(call7$fml, "y ~ -1+Intercept+x1+x2 | -1+Intercept+x1+z2")

  y <- as.vector(df$y)
  expect_identical(call1$y, y)
  expect_identical(call2$y, y)
  expect_identical(call3$y, y)
  expect_identical(call4$y, y)
  expect_identical(call5$y, y)
  expect_identical(call6$y, y)
  expect_identical(call7$y, y)

  x <- as.matrix(df[, c("cons", "x1", "x2"), drop = FALSE])
  expect_identical(call1$x, x)
  expect_identical(call2$x, x)
  expect_identical(call3$x, x)
  expect_identical(call4$x, x)
  x <- x[, -1, drop = FALSE]
  expect_identical(call5$x, x)
  Intercept <- rep(1, 10)
  x <- cbind(Intercept, x)
  expect_identical(call6$x, x)
  expect_identical(call7$x, x)

  z <- as.matrix(df[, c("z2"), drop = FALSE])
  expect_identical(call1$z, z)
  expect_identical(call2$z, z)
  expect_identical(call3$z, z)
  expect_identical(call4$z, z)
  expect_identical(call5$z, z)
  expect_identical(call6$z, z)
  expect_identical(call7$z, z)

  expect_identical(call1$depvar, "y")
  expect_identical(call2$depvar, "y")
  expect_identical(call3$depvar, "y")
  expect_identical(call4$depvar, "y")
  expect_identical(call5$depvar, "y")
  expect_identical(call6$depvar, "y")
  expect_identical(call7$depvar, "y")
  f8 <- yyy ~ -1 + cons + x1 + x2 | -1 + cons + x1 + z2
  df$yyy <- df$y
  call8 <- new_formula(f8, data = df, keep_exog = NULL)
  expect_identical(call8$depvar, "yyy")

  expect_identical(call1$x1, c("cons", "x1"))
  expect_identical(call2$x1, c("cons", "x1"))
  expect_identical(call3$x1, c("cons", "x1"))
  expect_identical(call4$x1, c("cons", "x1"))
  expect_identical(call5$x1, c("x1"))
  expect_identical(call6$x1, c("Intercept", "x1"))
  expect_identical(call7$x1, c("Intercept", "x1"))

  expect_identical(call1$x2, "x2")
  expect_identical(call2$x2, "x2")
  expect_identical(call3$x2, "x2")
  expect_identical(call4$x2, "x2")
  expect_identical(call5$x2, "x2")
  expect_identical(call6$x2, "x2")
  expect_identical(call7$x2, "x2")

  expect_identical(call1$z1, c("cons", "x1"))
  expect_identical(call2$z1, c("cons", "x1"))
  expect_identical(call3$z1, c("cons", "x1"))
  expect_identical(call4$z1, c("cons", "x1"))
  expect_identical(call5$z1, "x1")
  expect_identical(call6$z1, c("Intercept", "x1"))
  expect_identical(call7$z1, c("Intercept", "x1"))

  expect_identical(call1$z2, "z2")
  expect_identical(call2$z2, "z2")
  expect_identical(call3$z2, "z2")
  expect_identical(call4$z2, "z2")
  expect_identical(call5$z2, "z2")
  expect_identical(call6$z2, "z2")
  expect_identical(call7$z2, "z2")

  expect_identical(call1$dx1, 2L)
  expect_identical(call2$dx1, 2L)
  expect_identical(call3$dx1, 2L)
  expect_identical(call4$dx1, 2L)
  expect_identical(call5$dx1, 1L)
  expect_identical(call6$dx1, 2L)
  expect_identical(call7$dx1, 2L)

  expect_identical(call1$dx2, 1L)
  expect_identical(call2$dx2, 1L)
  expect_identical(call3$dx2, 1L)
  expect_identical(call4$dx2, 1L)
  expect_identical(call5$dx2, 1L)
  expect_identical(call6$dx2, 1L)
  expect_identical(call7$dx2, 1L)

  expect_identical(call1$dz1, 2L)
  expect_identical(call2$dz1, 2L)
  expect_identical(call3$dz1, 2L)
  expect_identical(call4$dz1, 2L)
  expect_identical(call5$dz1, 1L)
  expect_identical(call6$dz1, 2L)
  expect_identical(call7$dz1, 2L)

  expect_identical(call1$dz2, 1L)
  expect_identical(call2$dz2, 1L)
  expect_identical(call3$dz2, 1L)
  expect_identical(call4$dz2, 1L)
  expect_identical(call5$dz2, 1L)
  expect_identical(call6$dz2, 1L)
  expect_identical(call7$dz2, 1L)
  f9 <- y ~ -1 + cons + x1 + x2 | -1 + cons + x1 + z2 + z3
  df$z3 <- rnorm(10)
  call9 <- new_formula(f9, df, NULL)
  expect_identical(call9$dz2, 2L)

  expect_identical(length(call1$keep), length(call1$keep.names))
  expect_identical(length(call2$keep), length(call2$keep.names))
  expect_identical(length(call3$keep), length(call3$keep.names))
  expect_identical(length(call4$keep), length(call4$keep.names))
  expect_identical(length(call5$keep), length(call5$keep.names))
  expect_identical(length(call6$keep), length(call6$keep.names))
  expect_identical(length(call7$keep), length(call7$keep.names))
  expect_identical(length(call8$keep), length(call8$keep.names))
  expect_identical(length(call9$keep), length(call9$keep.names))

})

test_that("new_formula() creates keep correctly", {

  # test the keep_exog argument -> should return all keep vars (including endog)
  # repeat formulae from above
  # here nothing should need to be done; check -1 and 0 treated equally
  f1 <- y ~ -1 + cons + x1 + x2 | -1 + cons + x1 + z2
  f2 <- y ~ 0 + cons + x1 + x2 | 0 + cons + x1 + z2
  f3 <- y ~ -1 + cons + x1 + x2 | 0 + cons + x1 + z2
  f4 <- y ~ 0 + cons + x1 + x2 | -1 + cons + x1 + z2
  # here should run model without an intercept
  f5 <- y ~ -1 + x1 + x2 | -1 + x1 + z2
  # here should detect collinearity and delete cons but add Intercept instead
  f6 <- y ~ cons + x1 + x2 | cons + x1 + z2
  # here should run model with intercept and call it Intercept
  f7 <- y ~ x1 + x2 | x1 + z2

  df <- data.frame(y = stats::rnorm(10))
  df$cons <- 1 # intercept explicit
  df$x1 <- stats::rnorm(10) # exogenous regressor
  df$x2 <- stats::rnorm(10) # endogenous regressor
  df$z2 <- stats::rnorm(10) # excluded instrument

  call1 <- new_formula(f1, df, keep_exog = "cons") # this should be constant
  expect_identical(call1$keep, c(1L, 3L))
  expect_identical(call1$keep.names, c("cons", "x2"))

  call2 <- new_formula(f1, df, keep_exog = c("cons", "x1"))
  expect_identical(call2$keep, c(1L, 2L, 3L))
  expect_identical(call2$keep.names, c("cons", "x1", "x2"))

  call3 <- new_formula(f1, df, keep_exog = "x1")
  expect_identical(call3$keep, c(2L, 3L))
  expect_identical(call3$keep.names, c("x1", "x2"))

  # check that can equally select index
  call4 <- new_formula(f1, df, keep_exog = 2) # should be cons
  expect_identical(call1, call4)
  call5 <- new_formula(f1, df, keep_exog = c(2, 3))
  expect_identical(call2, call5)
  call6 <- new_formula(f1, df, keep_exog = 3)
  expect_identical(call3, call6)

  # check that can keep intercept even though not created yet
  call7 <- new_formula(f7, df, keep_exog = 0)
  expect_identical(call7$keep, c(1L, 3L))
  expect_identical(call7$keep.names, c("Intercept", "x2"))
  call8 <- new_formula(f7, df, keep_exog = "Intercept")
  expect_identical(call7, call8)
  call9 <- new_formula(f7, df, keep_exog = 3)
  expect_identical(call9$keep, c(2L, 3L))
  expect_identical(call9$keep.names, c("x1", "x2"))
  call10 <- new_formula(f7, df, keep_exog = "x1")
  expect_identical(call9, call10)
  call11 <- new_formula(f7, df, keep_exog = c("Intercept", "x1"))
  expect_identical(call11$keep, c(1L, 2L, 3L))
  expect_identical(call11$keep.names, c("Intercept", "x1", "x2"))
  call12 <- new_formula(f7, df, keep_exog = c(0, 3))
  expect_identical(call11, call12)

})

test_that("factory_indicators() works correctly", {

  # test input errors
  expect_error(factory_indicators("c"), "must be a single numeric value")
  expect_error(factory_indicators(1.3), "must be an integer")
  expect_error(factory_indicators(c(1, 2)), "must have length 1")
  expect_error(factory_indicators(0), "must equal the sample size, so cannot be 0 or negative")
  expect_error(factory_indicators(-3), "must equal the sample size, so cannot be 0 or negative")

  expect_identical(class(factory_indicators(10)), "function")
  expect_type(factory_indicators(10), "closure")

  # will have to test the function that is created by the factory
  c1 <- factory_indicators(5)
  impulses <- diag(5)
  colnames(impulses) <- c("iis1", "iis2", "iis3", "iis4", "iis5")
  for (i in 1:NCOL(impulses)) {
    ind <- c1(name = colnames(impulses)[i], uis = NULL)
    expect_identical(ind, impulses[, i, drop = FALSE])
  }
  steps <- matrix(0, nrow = 5, ncol = 5)
  steps[lower.tri(steps, diag = TRUE)] <- 1
  colnames(steps) <- c("sis1", "sis2", "sis3", "sis4", "sis5")
  for (i in 1:NCOL(steps)) {
    ind <- c1(name = colnames(steps)[i], uis = NULL)
    expect_identical(ind, steps[, i, drop = FALSE])
  }
  trends <- matrix(c(1,2,3,4,5), nrow = 5, ncol = 1)
  trends <- cbind(trends, c(0,1,2,3,4))
  trends <- cbind(trends, c(0,0,1,2,3))
  trends <- cbind(trends, c(0,0,0,1,2))
  trends <- cbind(trends, c(0,0,0,0,1))
  colnames(trends) <- c("tis1", "tis2", "tis3", "tis4", "tis5")
  for (i in 1:NCOL(trends)) {
    ind <- c1(name = colnames(trends)[i], uis = NULL)
    expect_identical(ind, trends[, i, drop = FALSE])
  }

  # check that dimensions are right, so try with different n
  c2 <- factory_indicators(3)
  impulses <- diag(3)
  colnames(impulses) <- c("iis1", "iis2", "iis3")
  for (i in 1:NCOL(impulses)) {
    ind <- c2(name = colnames(impulses)[i], uis = NULL)
    expect_identical(ind, impulses[, i, drop = FALSE])
  }
  steps <- matrix(0, nrow = 3, ncol = 3)
  steps[lower.tri(steps, diag = TRUE)] <- 1
  colnames(steps) <- c("sis1", "sis2", "sis3")
  for (i in 1:NCOL(steps)) {
    ind <- c2(name = colnames(steps)[i], uis = NULL)
    expect_identical(ind, steps[, i, drop = FALSE])
  }
  trends <- matrix(c(1,2,3), nrow = 3, ncol = 1)
  trends <- cbind(trends, c(0,1,2))
  trends <- cbind(trends, c(0,0,1))
  colnames(trends) <- c("tis1", "tis2", "tis3")
  for (i in 1:NCOL(trends)) {
    ind <- c2(name = colnames(trends)[i], uis = NULL)
    expect_identical(ind, trends[, i, drop = FALSE])
  }

  # test uis; use iis indicators but call them different
  uismatrix <- diag(5)
  colnames(uismatrix) <- c("my1", "my2", "my3", "my4", "my5")

  for (i in 1:NCOL(uismatrix)) {
    ind <- c1(name = colnames(uismatrix)[i], uis = uismatrix)
    expect_identical(ind, uismatrix[, i, drop = FALSE])
  }
  # test uis as list with iis and sis indicators
  m1 <- diag(5)
  names1 <- c("myiis1", "myiis2", "myiis3", "myiis4", "myiis5")
  colnames(m1) <- names1
  m2 <- matrix(0, nrow = 5, ncol = 5)
  m2[lower.tri(m2, diag = TRUE)] <- 1
  names2 <- c("mysis1", "mysis2", "mysis3", "mysis4", "mysis5")
  colnames(m2) <- names2
  uislist <- list(m1 = m1, m2 = m2)
  names <- c(names1, names2)
  base <- cbind(m1, m2)
  for (i in 1:length(names)) {
    ind <- c1(name = names[i], uis = uislist)
    expect_identical(ind, base[, i, drop = FALSE])
  }

  # test uis with unnamed matrix
  f <- factory_indicators(5)
  m <- diag(5)
  expect_identical(f(name = "uisxreg1", uis = m), diag(5)[, 1, drop = FALSE])
  expect_identical(f(name = "uisxreg4", uis = m), diag(5)[, 4, drop = FALSE])

  # check for errors within the creator function
  expect_error(c1(name = "iis10", uis = NULL), "Specified iis, sis, or tis of length larger than sample size")
  expect_error(c1(name = "iis0", uis = NULL))
  expect_error(c1(name = "sis6", uis = NULL), "Specified iis, sis, or tis of length larger than sample size")
  expect_error(c1(name = "sis0", uis = NULL))
  expect_error(c1(name = "tis7", uis = NULL), "Specified iis, sis, or tis of length larger than sample size")
  expect_error(c1(name = "tis0", uis = NULL))
  expect_error(c1(name = "my1", uis = NULL), "Retained indicator could not be created or found")
  expect_error(c1(name = "nonexistent", uis = uislist), "Retained indicator could not be created or found")

})
