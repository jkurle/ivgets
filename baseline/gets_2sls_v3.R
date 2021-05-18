library(gets)
library(r2sls)
library(ivreg)

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
ivreg::ivreg(form, data = df)

y <- as.vector(df[, "y"])
x <- as.matrix(df[, c("x1", "x2","x3","x4","x5","x6","x7","x8","x9","x10","x11", "x12")])
x_orig <- x
z <- as.matrix(df[, c("z11","z12", "z13")])


## modify to check whether can deal with iis flexibly
#x <- cbind(c(1, rep(0,99)), x, c(0,1,rep(0,98)), c(0,0,1,rep(0,97)))
#colnames(x)[c(1, 14, 15)] <- c("iis1", "iis2", "iis3")

isat(y, mxreg = x, mc = FALSE, iis = TRUE, sis = FALSE, t.pval = 0.05,
     user.estimator = list(name = "ivregFun", z = z, formula = form))
# this looks promising, only 3 indicators are retained when 5 are expected
# check that coefficients are correct

iis <- diag(100)
iis <- iis[, c(5, 51, 53)]
colnames(iis) <- c("iis5", "iis51", "iis53")
x <- cbind(x, iis)
form_iis <- y ~ -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + iis5 + iis51 + iis53 | -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13 + iis5 + iis51 + iis53
df2 <- cbind(df, iis)
a <- ivreg::ivreg(form_iis, data = df2)
summary(a)
# yes, results coincide

# does it detect outliers?
# add 2 deterministic outliers

df$y[1] <- df$y[1] + 6 # 6 is 3 times sigma
df$y[89] <- df$y[89] + 12
form <- y ~ -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 | -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13
y <- as.vector(df[, "y"])
x <- as.matrix(df[, c("x1", "x2","x3","x4","x5","x6","x7","x8","x9","x10","x11", "x12")])
z <- as.matrix(df[, c("z11","z12", "z13")])
isat(y, mxreg = x, mc = FALSE, iis = TRUE, sis = FALSE, t.pval = 0.05,
     user.estimator = list(name = "ivregFun", z = z, formula = form))
# exactly 1 and 89 were found

# try gets instead of isat
p <- generate_param(10, 2, 3, beta = c(6, 5, 0, 0, 0, 0, 0, 0, 0, 0, -4, 3), sigma = 2)
d <- generate_data(p, 100)
df <- d$data

form <- y ~ -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 | -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13

y <- as.vector(df[, "y"])
x <- as.matrix(df[, c("x1", "x2","x3","x4","x5","x6","x7","x8","x9","x10","x11", "x12")])
z <- as.matrix(df[, c("z11","z12", "z13")])

i <- getsFun(y, x, user.estimator = list(name = "ivregFun", z = z, formula = form), keep = c(11, 12), do.pet = TRUE,
        user.diagnostics = list(name = "ivDiag", pval = c(0.05, 0.05, 0.05), weak = TRUE, overid = TRUE))
# found
formula <- y ~ -1 + x1 + x2 + x3 + x11 + x12 | -1 + x1 + x2 + x3 + z11 + z12 + z13
a <- ivreg::ivreg(formula, data = df)
summary(a) # why keep x3 even though p value is 0.7?

# try without diagnostics
getsFun(y, x, user.estimator = list(name = "ivregFun", z = z, formula = form), keep = c(11, 12))
# now it drops it, so probably kept it because of PET or weak or overid
getsFun(y, x, user.estimator = list(name = "ivregFun", z = z, formula = form), keep = c(11, 12), do.pet = TRUE)
# PET was not the reason
a <- ivreg::ivreg(y ~ -1 + x1 + x2 + x11 + x12 | -1 + x1 + x2 + z11 + z12 + z13, data = df)
summary(a)
# but even without x3, weak instruments are rejected and Sargan does not reject
# no idea why keeps it; should not

# have coser look at path 3
f3 <- y ~ -1 + x1 + x2 + x3 + x4 + x6 + x7 + x10 + x11 + x12 | -1 + x1 + x2 + x3 + x4 + x6 + x7 + x10 + z11 + z12 + z13
a <- ivreg::ivreg(f3, data = df)
summary(a) # all tests are nice and x3 is the least significant, so delete that one next
f3 <- y ~ -1 + x1 + x2 + x4 + x6 + x7 + x10 + x11 + x12 | -1 + x1 + x2 + x4 + x6 + x7 + x10 + z11 + z12 + z13
a <- ivreg::ivreg(f3, data = df)
summary(a) # now that 3 is excluded, one of the weak instrument tests actually does not reject; this is why keep in
# then have two different final models
# and decide for the one with x3 in it because SC info criterion says it is the better model
# but the DGP actually also passes


# try gets with more observations
p <- generate_param(10, 2, 3, beta = c(6, 5, 0, 0, 0, 0, 0, 0, 0, 0, -4, 3), sigma = 2)
d <- generate_data(p, 1000)
df <- d$data

form <- y ~ -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 | -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13

y <- as.vector(df[, "y"])
x <- as.matrix(df[, c("x1", "x2","x3","x4","x5","x6","x7","x8","x9","x10","x11", "x12")])
z <- as.matrix(df[, c("z11","z12", "z13")])

getsFun(y, x, user.estimator = list(name = "ivregFun", z = z, formula = form), keep = c(11, 12), do.pet = TRUE,
        user.diagnostics = list(name = "ivDiag", pval = c(0.05, 0.05, 0.05), weak = TRUE, overid = TRUE))
# finds the DGP


# check isat with diagnostics
p <- generate_param(10, 2, 3, beta = c(6, 5, 0, 0, 0, 0, 0, 0, 0, 0, -4, 3), sigma = 2)
d <- generate_data(p, 100)
df <- d$data

form <- y ~ -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 | -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13

y <- as.vector(df[, "y"])
x <- as.matrix(df[, c("x1", "x2","x3","x4","x5","x6","x7","x8","x9","x10","x11", "x12")])
z <- as.matrix(df[, c("z11","z12", "z13")])

a <- isat(y, mxreg = x, mc = FALSE, iis = TRUE, sis = FALSE, t.pval = 0.05,
     user.estimator = list(name = "ivregFun", z = z, formula = form), 
     user.diagnostics = list(name = "ivDiag", pval = c(0.05, 0.05, 0.05), weak = TRUE, overid = TRUE))
iis <- diag(100)
iis <- iis[, c(5, 51, 53)]
colnames(iis) <- c("iis5", "iis51", "iis53")
x <- cbind(x, iis)
form_iis <- y ~ -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + iis5 + iis51 + iis53 | -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13 + iis5 + iis51 + iis53
df2 <- cbind(df, iis)
final <- ivreg::ivreg(form_iis, data = df2)
summary(final)

# check sis
p <- generate_param(10, 2, 3, beta = c(6, 5, 0, 0, 0, 0, 0, 0, 0, 0, -4, 3), sigma = 2)
d <- generate_data(p, 100)
df <- d$data

form <- y ~ -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 | -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13

y <- as.vector(df[, "y"])
x <- as.matrix(df[, c("x1", "x2","x3","x4","x5","x6","x7","x8","x9","x10","x11", "x12")])
z <- as.matrix(df[, c("z11","z12", "z13")])

a <- isat(y, mxreg = x, mc = FALSE, iis = FALSE, sis = TRUE, t.pval = 0.01,
          user.estimator = list(name = "ivregFun", z = z, formula = form), 
          user.diagnostics = list(name = "ivDiag", pval = c(0.05, 0.05, 0.05), weak = TRUE, overid = TRUE))
# in this setting, does not retain any steps
p <- generate_param(10, 2, 3, beta = c(6, 5, 0, 0, 0, 0, 0, 0, 0, 0, -4, 3), sigma = 2)
d <- generate_data(p, 100)
df <- d$data

form <- y ~ -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 | -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13

y <- as.vector(df[, "y"])
x <- as.matrix(df[, c("x1", "x2","x3","x4","x5","x6","x7","x8","x9","x10","x11", "x12")])
z <- as.matrix(df[, c("z11","z12", "z13")])

a <- isat(y, mxreg = x, mc = FALSE, iis = FALSE, sis = TRUE, t.pval = 0.05,
          user.estimator = list(name = "ivregFun", z = z, formula = form), 
          user.diagnostics = list(name = "ivDiag", pval = c(0.05, 0.05, 0.05), weak = TRUE, overid = TRUE))
# expect to retain 5
# actually retained 8; check whether coefficients and standard errors are correctly matched
# note that in gets, sis is 0 until the break and then 1 until the rest
df$sis5 <- c(rep(0,4),rep(1,96))
df$sis6 <- c(rep(0,5),rep(1,95))
df$sis13 <- c(rep(0,12),rep(1,88))
df$sis18 <- c(rep(0,17),rep(1,83))
df$sis23 <- c(rep(0,22),rep(1,78))
df$sis52 <- c(rep(0,51),rep(1,49))
df$sis54 <- c(rep(0,53),rep(1,47))
df$sis75 <- c(rep(0,74),rep(1,26))
form2 <- y ~ -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + sis5 + sis6 + sis13 + sis18 + sis23 + sis52 + sis54 + sis75 | -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13 + sis5 + sis6 + sis13 + sis18 + sis23 + sis52 + sis54 + sis75
b <- ivreg::ivreg(formula = form2, data = df)
summary(b) # yes, the results do coincide

# try tis even though there is no reason this should be used in cross-section
p <- generate_param(10, 2, 3, beta = c(6, 5, 0, 0, 0, 0, 0, 0, 0, 0, -4, 3), sigma = 2)
d <- generate_data(p, 100)
df <- d$data

form <- y ~ -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 | -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13

y <- as.vector(df[, "y"])
x <- as.matrix(df[, c("x1", "x2","x3","x4","x5","x6","x7","x8","x9","x10","x11", "x12")])
z <- as.matrix(df[, c("z11","z12", "z13")])

a <- isat(y, mxreg = x, mc = FALSE, iis = FALSE, sis = FALSE, tis = TRUE, t.pval = 0.01,
          user.estimator = list(name = "ivregFun", z = z, formula = form), 
          user.diagnostics = list(name = "ivDiag", pval = c(0.05, 0.05, 0.05), weak = TRUE, overid = TRUE))
# one indicator, tis14, has been retained


# have iis and sis at the same time
p <- generate_param(10, 2, 3, beta = c(6, 5, 0, 0, 0, 0, 0, 0, 0, 0, -4, 3), sigma = 2)
d <- generate_data(p, 100)
df <- d$data

form <- y ~ -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 | -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13

y <- as.vector(df[, "y"])
x <- as.matrix(df[, c("x1", "x2","x3","x4","x5","x6","x7","x8","x9","x10","x11", "x12")])
z <- as.matrix(df[, c("z11","z12", "z13")])

a <- isat(y, mxreg = x, mc = FALSE, iis = TRUE, sis = TRUE, tis = FALSE, t.pval = 0.01,
          user.estimator = list(name = "ivregFun", z = z, formula = form), 
          user.diagnostics = list(name = "ivDiag", pval = c(0.05, 0.05, 0.05), weak = TRUE, overid = TRUE))
# no indicator retained - very good because none of them belonged into the DGP


# turn on only one set of diagnostics
a <- isat(y, mxreg = x, mc = FALSE, iis = TRUE, sis = TRUE, tis = FALSE, t.pval = 0.01,
          user.estimator = list(name = "ivregFun", z = z, formula = form), 
          user.diagnostics = list(name = "ivDiag", pval = c(0.05, 0.05), weak = TRUE))
# this worked (surprisingly?)
a <- isat(y, mxreg = x, mc = FALSE, iis = TRUE, sis = FALSE, tis = FALSE, t.pval = 0.01,
          user.estimator = list(name = "ivregFun", z = z, formula = form), 
          user.diagnostics = list(name = "ivDiag", pval = c(0.05), overid = TRUE))
# some of the final output is incorrect; diagnostics are changed Statistic and df
a <- isat(y, mxreg = x, mc = FALSE, iis = TRUE, sis = FALSE, tis = FALSE, t.pval = 0.01,
          user.estimator = list(name = "ivregFun", z = z, formula = form), 
          user.diagnostics = list(name = "ivDiag", pval = c(0.05), overid = TRUE))
# diagnostics are correct now, had to change order of statistic and df
a <- isat(y, mxreg = x, mc = FALSE, iis = TRUE, sis = FALSE, tis = FALSE, t.pval = 0.01,
          user.estimator = list(name = "ivregFun", z = z, formula = form), 
          user.diagnostics = list(name = "ivDiag", pval = c(0.05, 0.05, 0.05), overid = TRUE, weak = TRUE))
a <- isat(y, mxreg = x, mc = FALSE, iis = TRUE, sis = FALSE, tis = FALSE, t.pval = 0.01,
          user.estimator = list(name = "ivregFun", z = z, formula = form), 
          user.diagnostics = list(name = "ivDiag", pval = c(0.05), overid = TRUE))



# test all arguments -> does it still work?
p <- generate_param(10, 2, 3, beta = c(6, 5, 0, 0, 0, 0, 0, 0, 0, 0, -4, 3), sigma = 2)
d <- generate_data(p, 75)
df <- d$data

form <- y ~ -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 | -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13

y <- as.vector(df[, "y"])
x <- as.matrix(df[, c("x1", "x2","x3","x4","x5","x6","x7","x8","x9","x10","x11", "x12")])
z <- as.matrix(df[, c("z11","z12", "z13")])

a <- isat(y, mxreg = x, mc = FALSE, iis = TRUE, sis = FALSE, tis = FALSE, t.pval = 0.01,
          user.estimator = list(name = "ivregFun", z = z, formula = form), 
          user.diagnostics = list(name = "ivDiag", pval = c(0.01, 0.01, 0.01), weak = TRUE, overid = TRUE))

# is there a problem if the dependent variable is not named "y"?
depvar <- as.vector(df[, "y"])
form <- depvar ~ -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 | -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13
a <- isat(y = depvar, mxreg = x, mc = FALSE, iis = TRUE, sis = FALSE, tis = FALSE, t.pval = 0.01,
          user.estimator = list(name = "ivregFun", z = z, formula = form), 
          user.diagnostics = list(name = "ivDiag", pval = c(0.01, 0.01, 0.01), weak = TRUE, overid = TRUE))
# no, works



# try naming issues again
df$dep <- df$y
df$y <- NULL
form <- depvar ~ -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 | -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13
depvar <- as.vector(df[, "dep"])
x <- as.matrix(df[, c("x1", "x2","x3","x4","x5","x6","x7","x8","x9","x10","x11", "x12")])
z <- as.matrix(df[, c("z11","z12", "z13")])
a <- isat(y = depvar, mxreg = x, mc = FALSE, iis = TRUE, sis = FALSE, tis = FALSE, t.pval = 0.01,
          user.estimator = list(name = "ivregFun", z = z, formula = form), 
          user.diagnostics = list(name = "ivDiag", pval = c(0.01, 0.01, 0.01), weak = TRUE, overid = TRUE))
# yeah, it works

# test more getsFun arguments
p <- generate_param(10, 2, 3, beta = c(6, 5, 0, 0, 0, 0, 0, 0, 0, 0, -4, 3), sigma = 2)
d <- generate_data(p, 100)
df <- d$data

form <- y ~ -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 | -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13

y <- as.vector(df[, "y"])
x <- as.matrix(df[, c("x1", "x2","x3","x4","x5","x6","x7","x8","x9","x10","x11", "x12")])
z <- as.matrix(df[, c("z11","z12", "z13")])

i <- getsFun(y, x, user.estimator = list(name = "ivregFun", z = z, formula = form), keep = c(11, 12), do.pet = TRUE,
             user.diagnostics = list(name = "ivDiag", pval = c(0.05, 0.05, 0.05), weak = TRUE, overid = TRUE),
             ar.LjungB = c(1,0.05), arch.LjungB = c(1, 0.05), normality.JarqueB = 0.05)
i <- isat(y, mxreg = x, mc = FALSE, iis = TRUE, sis = FALSE, tis = FALSE, t.pval = 0.01,
          user.estimator = list(name = "ivregFun", z = z, formula = form), 
          user.diagnostics = list(name = "ivDiag", pval = c(0.05), overid = TRUE),
          ar.LjungB = list(lag = 1, pval = 0.05), arch.LjungB = list(lag = 1, pval = 0.05), normality.JarqueB = 0.05)
# retains indicators iis12 and iis22 even though not statistically significant (pvalues 0.1 and 0.25)
# strange is that even when exclude iis12, still passes all diagnostics
# only reason would be PET test?
df$iis12 <- c(rep(0,11), 1, rep(0, 88))
df$iis22 <- c(rep(0,21), 1, rep(0, 78))
f <- y ~ -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + iis12 + iis22 | -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13 + iis12 + iis22
final <- ivreg::ivreg(formula = f, data = df)
f_no12 <- y ~ -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + iis22 | -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13 + iis22
f_no22 <- y ~ -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + iis12 | -1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z11 + z12 + z13 + iis12
no12 <- ivreg::ivreg(formula = f_no12, data = df)
no22 <- ivreg::ivreg(formula = f_no22, data = df)
Box.test(no12$residuals, lag = 1, type = "L") # insig
Box.test(no12$residuals^2, lag = 1, type = "L") # insig
# normality test
zhatadj <- no12$residuals
n <- length(zhatadj)
avgzhat <- mean(zhatadj) #do I really need this?
zhat.avgzhat <- zhatadj-avgzhat #do I really need this?
zhat.avgzhat2 <- zhat.avgzhat^2
K <- n*sum(zhat.avgzhat^4)/(sum(zhat.avgzhat2)^2)
S <- (sum(zhat.avgzhat^3)/n)/(sum(zhat.avgzhat2)/n)^(3/2)
JB <- (n/6)*(S^2 + 0.25*((K-3)^2))
JBpval <- pchisq(JB, df = 2, lower.tail=FALSE) # also insignificant


Box.test(no22$residuals, lag = 1, type = "L") # insig
Box.test(no22$residuals^2, lag = 1, type = "L") # insig
# normality test
zhatadj <- no22$residuals
n <- length(zhatadj)
avgzhat <- mean(zhatadj) #do I really need this?
zhat.avgzhat <- zhatadj-avgzhat #do I really need this?
zhat.avgzhat2 <- zhat.avgzhat^2
K <- n*sum(zhat.avgzhat^4)/(sum(zhat.avgzhat2)^2)
S <- (sum(zhat.avgzhat^3)/n)/(sum(zhat.avgzhat2)/n)^(3/2)
JB <- (n/6)*(S^2 + 0.25*((K-3)^2))
JBpval <- pchisq(JB, df = 2, lower.tail=FALSE) # also insignificant


zhatadj <- final$residuals
n <- length(zhatadj)
avgzhat <- mean(zhatadj) #do I really need this?
zhat.avgzhat <- zhatadj-avgzhat #do I really need this?
zhat.avgzhat2 <- zhat.avgzhat^2
K <- n*sum(zhat.avgzhat^4)/(sum(zhat.avgzhat2)^2)
S <- (sum(zhat.avgzhat^3)/n)/(sum(zhat.avgzhat2)/n)^(3/2)
JB <- (n/6)*(S^2 + 0.25*((K-3)^2))
JBpval <- pchisq(JB, df = 2, lower.tail=FALSE) # also insignificant
Box.test(final$residuals, lag = 1, type = "L")
Box.test(final$residuals^2, lag = 1, type = "L")

# the reason is that we have two terminal models: one without any indicators and
# one with iis12 and iis22; both pass all diagnostics but the info criterion is
# simply better for the model with the two indicators
# probably include.gum arguments should change stuff (here the one with both IIS
# is treated as the gum even though it really shouldn't be)

# how does ivreg handle empty models?
# ivreg::ivreg(y ~ -1, data = df) does not seem to be able to handle that
# ivreg::ivreg(y ~ 0)
y <- rnorm(20)
is.empty.model(y ~ 0)
is.empty.model(y ~ -1)
is.empty.model(lm(y ~ 0))
a <- lm(y ~ 0)


# re-written diagnostics function
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
isat(y, mxreg = x, mc = FALSE, iis = TRUE, sis = FALSE, tis = FALSE, t.pval = 0.01,
     user.estimator = list(name = "ivregFun", z = z, formula = form), 
     user.diagnostics = list(name = "ivDiag", pval = c(0.05), is.reject.bad = TRUE, overid = TRUE),
     ar.LjungB = list(lag = 1, pval = 0.05), arch.LjungB = list(lag = 1, pval = 0.05), normality.JarqueB = 0.05)





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




# install form GitHub
#install.packages("githubinstall")
library(githubinstall)
library(devtools)
#gh_install_packages("jkurle/gets")
#gh_list_packages(username = "gsucarrat")
remove.packages("gets") 
install_github("jkurle/gets", ref = "2sls-v2", subdir = "gets")





