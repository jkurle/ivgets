library(gets)
library(r2sls)
library(ivreg)

# setup
# install form GitHub
#install.packages("githubinstall")
library(githubinstall)
library(devtools)
#gh_install_packages("jkurle/gets")
#gh_list_packages(username = "gsucarrat")
remove.packages("gets")
install_github("jkurle/gets", ref = "2sls-v2", subdir = "gets")

# 2sls-v2 incorporates the changes discussed with Genaro
# diagnostics explicitly in user diagnostics to allow for rejecting being good
# in arx() deleting colnames revert and put into user estimator; handle empty
# try new function for ivregFun to solve matching problem of order and names

ivregFun <- function(y, x, z, formula, ...) {
  # extract formula
  # convert formula to character vector of length 1
  fml <- Reduce(paste, deparse(formula))
  # split formula into its three party: y, x, z
  fml_split <- strsplit(fml, "~|\\|")
  # delete symbols and leading & trailing spaces, collect in character vector
  y_var <- trimws(fml_split[[1]][1])
  x_var <- fml_split[[1]][2]
  x_var <- trimws(strsplit(x_var, "\\+|\\*")[[1]])
  z_var <- fml_split[[1]][3]
  z_var <- trimws(strsplit(z_var, "\\+|\\*")[[1]])

  x2_base <- setdiff(x_var, z_var) # endogenous regressors
  x1_base <- setdiff(x_var, x2_base) # exogenous regressors
  z1_base <- x1_base # included instruments
  z2_base <- setdiff(z_var, z1_base) # outside instruments

  result <- list()
  result$n <- length(y)
  if (is.null(x) || NCOL(x) == 0) {
    result$k <- 0
  } else {
    # if have no column names then need to give them some names and run model
    # x should include the structural regressors (exog. and endog.)
    # z should only include the excluded instruments
    # if (is.null(colnames(x))) {
    #   xnames <- NULL
    #   for (n in 1:NCOL(x)) {
    #     new <- paste("x", n, sep = "")
    #     xnames <- c(xnames, new)
    #   }
    #   colnames(x) <- xnames
    # }

    df <- data.frame(cbind(y, x, z))

    # create the formula for estimation using the available variables
    reg <- colnames(x)
    reg <- c("-1", reg)
    fml_structural <- paste(reg, sep = "", collapse = "+")
    reg_endog <- intersect(x2_base, reg)
    reg_exog <- setdiff(reg, reg_endog)
    instr <- union(reg_exog, colnames(z))
    fml_first <- paste(instr, sep = "", collapse = "+")
    new_fml <- paste(y_var, fml_structural, sep = " ~ ")
    new_fml <- paste(new_fml, fml_first, sep = " | ")
    result$k <- NCOL(x)

  }
  result$df <- result$n - result$k

  # call ivreg::ivreg() if k > 0
  if (result$k > 0) { # have regressors
    tmp <- ivreg::ivreg(formula = as.formula(new_fml), data = df)

    result$coefficients <- coef(tmp) # only the beta 2nd stage coeff
    result$vcov <- vcov(tmp)
    sigma2 <- sum(tmp$residuals^2) / tmp$nobs
    # assume normality of first stage errors for likelihood as Autometrics does
    result$logl <- - (result$n / 2) * (1 + log(2*pi) + log(sigma2))
    result$diag <- summary(tmp)$diagnostics
    result$residuals <- residuals(tmp)

  } else { # regressor matrix empty

    result$coefficients <- NULL
    result$vcov <- NULL
    result$logl <- sum(stats::dnorm(y, sd = sqrt(var(y)), log = TRUE))
    result$residuals <- y

  } # end if



  return(result)

}

ivDiag <- function(x, weak = FALSE, overid = FALSE) {

  diagn <- x$diag
  diagnostic.rule <- NULL

  weak_test <- NULL
  overid_test <- NULL
  if (weak == TRUE) {
    # select all rows except for last two
    weak_test <- diagn[c(-NROW(diagn), -NROW(diagn)+1), , drop = FALSE]
    weak_diagnostic.rule <- rep(FALSE, NROW(weak_test))
    diagnostic.rule <- c(diagnostic.rule, weak_diagnostic.rule)
  }
  if (overid == TRUE) {
    # select last row
    overid_test <- diagn[NROW(diagn), , drop = FALSE]
    overid_diagnostic.rule <- rep(TRUE, NROW(overid_test))
    diagnostic.rule <- c(diagnostic.rule, overid_diagnostic.rule)
  }

  out <- rbind(weak_test, overid_test)
  out <- out[, c("statistic", "df1", "p-value", "df1", "df2"), drop = FALSE]
  colnames(out)[[2]] <- "df"
  out[-NROW(out), "df"] <- NA
  out <- out[, 1:3, drop = FALSE]

  attr(out, "is.reject.bad") <- diagnostic.rule

  return(out)

}

# test setup
p <- generate_param(10, 2, 3, beta = c(6, 5, 0, 0, 0, 0, 0, 0, 0, 0, -4, 3), sigma = 2)
d <- generate_data(p, 100)
df <- d$data

form <- y ~ -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 | -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13

y <- as.vector(df[, "y"])
x <- as.matrix(df[, c("x1", "x2","x3","x4","x5","x6","x7","x8","x9","x10","x11", "x12")])
z <- as.matrix(df[, c("z11","z12", "z13")])

i <- getsFun(y, x, user.estimator = list(name = "ivregFun", z = z, formula = form), keep = c(11, 12), do.pet = TRUE,
             user.diagnostics = list(name = "ivDiag", pval = c(0.05, 0.05, 0.05),
                                     is.reject.bad = c(FALSE, FALSE, TRUE),
                                     weak = TRUE, overid = TRUE))
isat(y, mxreg = x, mc = FALSE, iis = TRUE, sis = TRUE, tis = FALSE, t.pval = 0.001,
     user.estimator = list(name = "ivregFun", z = z, formula = form),
     user.diagnostics = list(name = "ivDiag", pval = c(0.05), is.reject.bad = TRUE, overid = TRUE),
     ar.LjungB = list(lag = 1, pval = 0.05), arch.LjungB = list(lag = 1, pval = 0.05), normality.JarqueB = 0.05)

# check different intercept specifications
p <- generate_param(10, 2, 3, beta = c(6, 5, 0, 0, 0, 0, 0, 0, 0, 0, -4, 3), sigma = 2)
d <- generate_data(p, 100)
df <- d$data
y <- as.vector(df[, "y"])
x <- as.matrix(df[, c("x1", "x2","x3","x4","x5","x6","x7","x8","x9","x10","x11", "x12")])
z <- as.matrix(df[, c("z11","z12", "z13")])

form1 <- y ~ -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 | -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13
i <- getsFun(y, x, user.estimator = list(name = "ivregFun", z = z, formula = form1), keep = c(11, 12), do.pet = TRUE,
             user.diagnostics = list(name = "ivDiag", pval = c(0.05, 0.05, 0.05),
                                     is.reject.bad = c(FALSE, FALSE, TRUE),
                                     weak = TRUE, overid = TRUE))
# retains 1, 2, 3, 11, 12
colnames(x)[1] <- "intacapt"
form2 <- y ~ x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 | x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13
i <- getsFun(y, x, user.estimator = list(name = "ivregFun", z = z, formula = form2), keep = c(11, 12), do.pet = TRUE,
             user.diagnostics = list(name = "ivDiag", pval = c(0.05, 0.05, 0.05),
                                     is.reject.bad = c(FALSE, FALSE, TRUE),
                                     weak = TRUE, overid = TRUE))
# retains intacapt, 2, 3, 11, 12
# so matrix x really needs to contain only the regressors that are specified in the formula
# otherwise we get a problem (here intacapt is kept even though should not even be in the estimation)
# this is because getsFun() has to handle the estimation dynamically (e.g. if indicators are added)

x <- x[, -1] # delete the intercept regressor
i <- getsFun(y, x, user.estimator = list(name = "ivregFun", z = z, formula = form2), keep = c(10, 11), do.pet = TRUE,
             user.diagnostics = list(name = "ivDiag", pval = c(0.05, 0.05, 0.05),
                                     is.reject.bad = c(FALSE, FALSE, TRUE),
                                     weak = TRUE, overid = TRUE))
# now only keeps other regressors x2, x4, x5, x6, x10, x11, x12
# no intercept even though formula has specified an intercept
# this is because internally, the formula is always created such that -1 is added
form3 <- y ~ -1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 | -1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13
i <- getsFun(y, x, user.estimator = list(name = "ivregFun", z = z, formula = form3), keep = c(10, 11), do.pet = TRUE,
             user.diagnostics = list(name = "ivDiag", pval = c(0.05, 0.05, 0.05),
                                     is.reject.bad = c(FALSE, FALSE, TRUE),
                                     weak = TRUE, overid = TRUE))
# so good thing is that this selects the same as before: x2, x4, x5, x6, x10, x11, x12
# what does ivreg do if 1 or -1 are not specified "consistently" for first part and | second part?
p <- generate_param(2, 1, 2, beta = c(3, -3, 5), sigma = 1)
d <- generate_data(p, 100)
df <- d$data
ivreg::ivreg(y ~ -1 + x1 + x2 + x3 | -1 + x1 + x2 + z3, data = df)
ivreg::ivreg(y ~ 0 + x1 + x2 + x3 | 0 + x1 + x2 + z3, data = df)
ivreg::ivreg(y ~ x2 + x3 | x2 + z3, data = df)
ivreg::ivreg(y ~ -1 + x2 + x3 | -1 + x2 + z3, data = df)
ivreg::ivreg(y ~ -1 + x2 + x3 | x2 + z3, data = df) # this gives different results (means intercept is now excluded instrument)
ivreg::ivreg(y ~ x2 + x3 | -1 + x2 + z3 + z4, data = df)
# gives error if were not in the overid case
# but accepts this (using both z3 and z4) but I believe this should not be allowed
# intercept is always exogenous, so needs to be in the set of instruments
ivreg::ivreg(y ~ 0 + x2 + x3 | 0 + x2 + z3, data = df) # zero also takes out the intercept



ivgets <- function(
  # ivreg::ivreg arguments
  formula, data, subset, na.action, weights, offset, contrasts, model, x, y,
  # gets::getsFun() arguments
  gum.result = NULL, t.pval = 0.05, wald.pval = t.pval, do.pet = TRUE,
  ar.LjungB = NULL, arch.LjungB = NULL, normality.JarqueB = NULL,
  include.gum = FALSE, include.1cut = FALSE, include.empty = FALSE,
  max.paths = NULL, turbo = FALSE, tol = 1e-07, max.regs = NULL,
  print.searchinfo = TRUE, alarm = FALSE,
  # new arguments
  keep_exog = NULL, overid = FALSE, overid.pval = t.pval, weak = FALSE,
  weak.pval = t.pval) {

  # arguments that I did not use: untransformed.residuals, user.estimator,
  # ar.LjungB, arch.LjungB

}









f <- function(# ivreg::ivreg arguments
  formula, data, subset, na.action, weights, offset, contrasts, model, x, y,
  # gets::isat arguments
  iis, sis, tis, uis, blocks, ratio.threshold, max.block.size, t.pval, wald.pval, do.pet,
  ar.LjungB, arch.LjungB, normality.JarqueB, info.method, include.gum, include.1cut,
  include.empty, max.paths, parallel.options, turbo, tol, LAPACK, max.regs, print.searchinfo,
  plot, alarm,
  # own arguments
  overid, weak) {

  # extract formula
  # convert formula to character vector of length 1
  fml <- Reduce(paste, deparse(formula))
  # split formula into its three party: y, x, z
  fml_split <- strsplit(fml, "~|\\|")
  # delete symbols and leading & trailing spaces, collect in character vector
  y_var <- trimws(fml_split[[1]][1])
  x_var <- fml_split[[1]][2]
  x_var <- trimws(strsplit(x_var, "\\+|\\*")[[1]])
  z_var <- fml_split[[1]][3]
  z_var <- trimws(strsplit(z_var, "\\+|\\*")[[1]])

  x2_base <- setdiff(x_var, z_var) # endogenous regressors
  x1_base <- setdiff(x_var, x2_base) # exogenous regressors
  z1_base <- x1_base # included instruments
  z2_base <- setdiff(z_var, z1_base) # outside instruments

  # they contain -1 if user has specified it
  if ("-1" %in% x_var) {
    x_select <- setdiff(x_var, "-1")
  }

  y <- as.vector(data[, y_var])
  x <- as.matrix(data[, x_select])
  z <- as.matrix(data[, z2_base])



}



# what if give uis
ret <- isat(y, mxreg = x, mc = FALSE, iis = FALSE, sis = FALSE, tis = FALSE, t.pval = 0.001,
            user.estimator = list(name = "ivregFun", z = z, formula = form),
            user.diagnostics = list(name = "ivDiag", pval = c(0.05), is.reject.bad = TRUE, overid = TRUE),
            ar.LjungB = list(lag = 1, pval = 0.05), arch.LjungB = list(lag = 1, pval = 0.05),
            normality.JarqueB = 0.05, uis = diag(100))


u <- diag(100)
unames <- NULL
for(i in 1:100) {unames <- c(unames, paste("uis", i, sep = ""))}
colnames(u) <- unames

ret <- isat(y, mxreg = x, mc = FALSE, iis = FALSE, sis = FALSE, tis = FALSE, t.pval = 0.001,
            user.estimator = list(name = "ivregFun", z = z, formula = form),
            user.diagnostics = list(name = "ivDiag", pval = c(0.05), is.reject.bad = TRUE, overid = TRUE),
            ar.LjungB = list(lag = 1, pval = 0.05), arch.LjungB = list(lag = 1, pval = 0.05),
            normality.JarqueB = 0.05, uis = u)

# what if given as list? (must be regressors named)
p <- generate_param(10, 2, 3, beta = c(6, 5, 0, 0, 0, 0, 0, 0, 0, 0, -4, 3), sigma = 2)
d <- generate_data(p, 100)
df <- d$data
y <- as.vector(df[, "y"])
x <- as.matrix(df[, c("x1", "x2","x3","x4","x5","x6","x7","x8","x9","x10","x11", "x12")])
z <- as.matrix(df[, c("z11","z12", "z13")])
form <- y ~ -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 | -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13


ret <- isat(y, mxreg = x, mc = FALSE, iis = FALSE, sis = FALSE, tis = FALSE, t.pval = 0.001,
            user.estimator = list(name = "ivregFun", z = z, formula = form),
            user.diagnostics = list(name = "ivDiag", pval = c(0.05), is.reject.bad = TRUE, overid = TRUE),
            ar.LjungB = list(lag = 1, pval = 0.05), arch.LjungB = list(lag = 1, pval = 0.05),
            normality.JarqueB = 0.05, uis = list(u[,1:15], u[, 16:100]))


test <- ivisat(formula = form, data = df, overid = 0.05, uis = list(u[,1:15], u[, 16:100]), iis = FALSE, ar.LjungB = list(lag = 1, pval = 0.05), arch.LjungB = list(lag = 1, pval = 0.05),
       normality.JarqueB = 0.05)
