#' Extract the first and second stage regressors of ivreg formula
#'
#' \code{extract_variables} takes a formula object for [ivreg::ivreg()], i.e.
#' in a format of \code{y ~ x1 + x2 | x1 + z2} and extracts the different
#' elements in a list.
#'
#' @param formula A formula for the [ivreg::ivreg] function, i.e. in format
#'   \code{y ~ x1 + x2 | z1 + z2}.
#'
#' @return \code{extract_variables} returns a list with three components:
#'   \code{$yvar} stores the name of the dependent variable, \code{$first} the
#'   names of the regressors of the first stage and \code{$second} the names of
#'   the second stage regressors.
#'
#' @export

extract_variables <- function(formula) {

  # convert formula to character vector of length 1
  # use Reduce() to avoid length > 1 if formula is too long
  fml <- Reduce(paste, deparse(formula))

  # check that formula contains both "~" and "|" symbols
  if (!(grepl("~", fml) && grepl("\\|", fml))) {
    stop(strwrap("The `formula` is not of the required format since it does not
          include both symbols `~` and `|`", prefix = " ", initial = ""))
  }

  # split formula into its three party: y, x, z
  fml_split <- strsplit(fml, "~|\\|")

  # ensure that the formula contains three parts
  if (length(fml_split[[1]]) != 3) {
    stop(strwrap("The `formula` does not consist of three parts as in
         y ~ x1 + x2 | x1 + z2", prefix = " ", initial = ""))
  }

  # delete symbols and leading & trailing spaces, collect in character vector
  yvar <- trimws(fml_split[[1]][1])
  second <- fml_split[[1]][2]
  second <- trimws(strsplit(second, "\\+|\\*")[[1]])
  first <- fml_split[[1]][3]
  first <- trimws(strsplit(first, "\\+|\\*")[[1]])

  if (yvar == "") {
    stop(strwrap("The `formula` does not specify any dependent variable",
                 prefix = " ", initial = ""))
  }

  vars <- list(yvar = yvar, first = first, second = second)
  return(vars)

}

#' Takes ivreg formula and returns formula compatible with model selection
#'
#' \code{new_formula} takes a formula object for [ivreg::ivreg()], i.e. in a
#' format of \code{y ~ x1 + x2 | x1 + z2}, and returns a list with element
#' suitable for model selection. For example, it updates the data by creating
#' an intercept if specified in the formula, checks for collinearity among the
#' regressors, and updates the formula accordingly.
#'
#' @inheritParams extract_variables
#' @inheritParams ivgets
#' @param data A data frame.
#'
#' @return A list with several named elements. Component \code{$fml} stores the
#'   new baseline formula that will be used for model selection. Components
#'   \code{y}, \code{x}, and \code{z} store the data of the dependent variable,
#'   structural regressors, and excluded instruments. The entries
#'   \code{$depvar}, \code{$x1}, \code{$x2}, \code{$z1}, and \code{$z2} contain
#'   the names of the dependent variable, endogenous and exogenous regressors,
#'   included and excluded instruments. \code{$dx1}, \code{$dx2}, \code{$dz1},
#'   \code{$dz2} store the dimensions of the respective variables. Finally,
#'   \code{$keep} and \code{$keep.names} contain the indices and names of the
#'   regressors that will not be selected over.
#'
#' @export

new_formula <- function(formula, data, keep_exog) {

  if (!(is.numeric(keep_exog) | is.character(keep_exog) | is.null(keep_exog))) {
    stop("Argument 'keep_exog' must either be NULL, numeric, or a character.")
  }

  keep_intercept <- FALSE
  # keep_exog = selection of exogenous regressors that should not select over
  if (is.numeric(keep_exog)) {
    keep_intercept <- (0 %in% keep_exog)
    if (length(setdiff(keep_exog, 0)) == 0) { # only keep intercept
      keep_names <- NULL
    } else {
      keep_names <- colnames(data)[setdiff(keep_exog, 0)]
    }
  } else if (is.character(keep_exog)) {
    keep_intercept <- ("Intercept" %in% keep_exog)
    if (length(setdiff(keep_exog, "Intercept")) == 0) { # only keep intercept
      keep_names <- NULL
    } else {
      keep_names <- setdiff(keep_exog, "Intercept")
      if (any(!(keep_names %in% colnames(data)))) {
        stop("Argument 'keep_exog' specifies names that cannot be found in the data frame.")
      }
    }
  } else {
    keep_names <- NULL
  }
  # keep_intercept is TRUE if keep_exog had index 0 or "Intercept"
  # keep_names contains the names of variables excluding any intercept name
  # keep_names is NULL if no variables should be kept (except intercept maybe)

  # check formula: extract regressors and check presence of intercept
  vars <- extract_variables(formula = formula)

  if (any(c("-1", "0") %in% vars$first)) {
    intercept1 <- FALSE
  } else {
    intercept1 <- TRUE
  }
  if (any(c("-1", "0") %in% vars$second)) {
    intercept2 <- FALSE
  } else {
    intercept2 <- TRUE
  }

  # throw errors for two specific cases; require (no) intercept to be specified
  # symmetrically for the first and second stage at the moment
  if (intercept2 == TRUE & intercept1 == FALSE) {
    stop("If have intercept in the structural equation, it should also be in the first stage.")
  }
  if (intercept2 == FALSE & intercept1 == TRUE) {
    stop("Using intercept as excluded instrument in the first stage is currently not supported.")
  }

  # extract the base variables (this excludes any intercept specifications)
  reg1_base <- setdiff(vars$first, c("-1", "0", "1"))
  reg2_base <- setdiff(vars$second, c("-1", "0", "1"))
  x2_base <- setdiff(reg2_base, reg1_base)
  x1_base <- setdiff(reg2_base, x2_base)
  z1_base <- x1_base
  z2_base <- setdiff(reg1_base, x1_base)

  # extract the base vectors/matrices of regressand, regressors, instruments
  y <- as.vector(data[, vars$yvar])
  x <- as.matrix(data[, c(x1_base, x2_base), drop = FALSE], drop = FALSE)
  z <- as.matrix(data[, z2_base, drop = FALSE], drop = FALSE)

  # check for perfect multicollinearity issues
  # multicollinearity in original matrix
  if (length(colnames(x)) > length(colnames(gets::dropvar(x, silent = TRUE)))) {
    stop("Original formula specification has perfect collinearity. Please adjust regressors.")
  }
  # create new structural equation regressor matrix
  if (intercept2) {
    xnames <- colnames(x)
    # check whether regressor named "Intercept" already present in formula
    if ("Intercept" %in% xnames) {
      stop("Formula specification creates an intercept but regressor named \"Intercept\" is also included in formula. Please specify \"-1\" in the formula.")
    }
    # add an intercept
    x <- cbind(1, x)
    colnames(x)[1] <- "Intercept"
    xnew <- gets::dropvar(x, silent = TRUE)
    # returns matrix as before but now has a column with constant 1 called
    # "Intercept". If had intercept present before then the other one is
    # dropped.
    dropnames <- setdiff(xnames, colnames(xnew))
    # rank deficient by more than 1 (should not happen)
    if (length(dropnames) > 1) { # nocov start
      stop("Original structural equation had multicollinearity issues. Please re-specify the initial model such that no multicollinearity arises.")
    } # nocov end
    # dropnames can only have dropped a regressor that was perfectly collinear
    # with the intercept; so can delete that regressor from the exogenous regr.
    # but add the regressor Intercept
    if (length(dropnames) == 1) { # rank deficient by 1
      warning("Intercept has introduced collinearity. Drop one regressor: ", dropnames)
    }
    if (any(dropnames %in% keep_names)) {
      stop("A regressor that was specified in 'keep_exog' has been dropped due to multicollinearity. Please re-specify model.")
    }
    x <- xnew
    x1_base <- c("Intercept", setdiff(x1_base, dropnames))
    z1_base <- x1_base
  }

  # find the indices of the regressors that should be kept, indices corr. to x
  if (keep_intercept) {
    keep_names <- union("Intercept", keep_names)
  }
  # also need to keep all endogenous regressors
  keep <- union(keep_names, x2_base)
  keep_names <- keep
  keep <- which(colnames(x) %in% keep_names)
  # in practice, keep should never be empty unless we are really in OLS case
  # nocov start
  if (length(keep) == 0) { keep <- NULL }
  # nocov end

  # save number of the types of regressors
  dx1 <- length(x1_base)
  dx2 <- length(x2_base)
  dz1 <- length(z1_base)
  dz2 <- length(z2_base)
  # sanity check
  if (!identical(dx1, dz1)) { # nocov start
    stop("Something went wrong. dx1 should be equal to dz1.")
  } # nocov end

  # create new baseline formula
  baseline_x <- paste(c("-1", x1_base, x2_base), sep = "", collapse = "+")
  baseline_z <- paste(c("-1", z1_base, z2_base), sep = "", collapse = "+")
  baseline <- paste(c(vars$yvar, baseline_x), sep = "", collapse = " ~ ")
  baseline <- paste(c(baseline, baseline_z), sep = "", collapse = " | ")

  out <- list(fml = baseline, y = y, x = x, z = z, depvar = vars$yvar,
              x1 = x1_base, x2 = x2_base, z1 = z1_base, z2 = z2_base, dx1 = dx1,
              dx2 = dx2, dz1 = dz1, dz2 = dz2, keep = keep,
              keep.names = keep_names)

  return(out)

}

#' Function factory for creating indicators from their names
#'
#' \code{factory_indicators} creates a function that takes the name of an
#' indicator and returns the corresponding indicator to be used in a regression.
#' For user-specified indicators, it extracts the corresponding column from the
#' uis matrix.
#'
#' @param n An integer specifying the length of the indicators.
#'
#' @return \code{factory_indicators} returns a function called \code{creator()}.
#'
#' @details Argument \code{n} should equal the number of observations in the
#' data set which will be augmented with the indicators.
#'
#' The created function takes a name of an indicator and the original uis
#' argument that was used in indicator saturation and returns the indicator.
#'
#' @export

factory_indicators <- function(n) {

  if (!is.numeric(n)) {
    stop("Argument 'n' must be a single numeric value (integer).")
  }
  if (!identical(length(n), 1L)) {
    stop("Argument 'n' must have length 1.")
  }
  if (!(n %% 1 == 0)) {
    stop("Argument 'n' must be an integer.")
  }
  if (n < 1) {
    stop("Argument 'n' must equal the sample size, so cannot be 0 or negative.")
  }

  creator <- function(name, uis) {

    type <- stringr::str_extract(string = name, pattern = "^[[:alpha:]]+")
    no <- as.numeric(stringr::str_extract(string = name,
                                          pattern = "[1-9]([0-9]+)?"))
    indicator <- NULL # initialise as NULL
    if (type %in% c("iis", "sis", "tis") & (no > n)) {
      stop("Specified iis, sis, or tis of length larger than sample size.")
    }

    if (identical(type, "iis")) {
      indicator <- c(rep(0, times = (no - 1)), 1, rep(0, times = (n - no)))
      indicator <- as.matrix(indicator, ncol = 1, nrow = n)
      colnames(indicator) <- name

    } else if (identical(type, "sis")) {

      indicator <- c(rep(0, times = (no - 1)), rep(1, times = (n - no + 1)))
      indicator <- as.matrix(indicator, ncol = 1, nrow = n)
      colnames(indicator) <- name

    } else if (identical(type, "tis")) {

      m <- matrix(0,n,n)
      v1n <- seq(1,n)
      loop.indx <- 1:n
      tmp <- function(i){
        m[c(i:n),i] <<- v1n[1:c(n-i+1)]
      }
      sapply(loop.indx,tmp)
      indicator <- m[, no]
      indicator <- as.matrix(indicator, ncol = 1, nrow = n)
      colnames(indicator) <- name

    } else if (is.matrix(uis)) { # uis is a matrix; could be named or not

      if (identical(type, "uisxreg")) { # indicator from unnamed matrix uis

        indicator <- uis[, no, drop = FALSE] # retrieve by indicator index

      } else { # indicator from named matrix uis

        indicator <- uis[, name, drop = FALSE] # retrieve by indicator name

      } # end if uis is matrix

    } else if (is.list(uis)) { # uis is a list of matrices; cols must be named

      for (matx in seq_along(uis)) {

        if (name %in% colnames(uis[[matx]])) { # found
          indicator <- uis[[matx]][, name, drop = FALSE] # retrieve by ind name
          break
        } # end if found

      } # end for through all matrices in uis

    } # end if uis is list

    if (is.null(indicator)) {
      stop("Retained indicator could not be created or found.")
    } else {
      return(indicator)
    } # check that indicator has really been found

  }

  return(creator) # return a function

}

#' 2SLS estimator wrapper
#'
#' Stripped down version of [ivreg::ivreg()] that does not allow for
#' weights, offset, other methods than 2SLS, and does not calculate influence
#' statistics. Supposedly faster but returns much less detailed output.
#'
#' @inheritParams ivgets
#'
#' @return \code{twosls()} returns a list with eight names elements:
#'   \code{$coefficients} stores the coefficient estimates of the second stage,
#'   \code{$residuals} the residuals of the structural equation (i.e. using X
#'   and not Xhat), \code{$fitted.values} the fitted values of the second stage,
#'   \code{$n} and \code{$nobs} the sample size, \code{$k} the number of
#'   regressors in the structural equation, \code{$cov.unscaled} the unscaled
#'   variance-covariance matrix, and \code{$sigma} the degrees-of-freedom
#'   adjusted equation standard error.
#'
#' @section WARNING:
#' The return value is given class [ivreg::ivreg()] but it is not a true
#' \code{"ivreg"} object. This does not pose any problems for internal use but
#' should not be used outside of its usage in its current form. The class
#' assignment is likely to change in the future.
#'
#' @export

twosls <- function(formula, data) {

  # can only use this function when "Formula" is installed
  if (!requireNamespace("Formula", quietly = TRUE)) { # nocov start
    stop("Package 'Formula' must be installed to use this function.", .call = FALSE)
  } # nocov end

  # capture function call
  mf <- match.call()
  mf$drop.unused.levels <- TRUE
  formula <- Formula::as.Formula(formula)
  mf$formula <- formula
  # convert formula to model frame
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  Y <- model.response(mf, "numeric") # extract depvar
  mtX <- terms(formula, data= data, rhs = 1)
  X <- model.matrix(mtX, mf, contrasts.arg = NULL) # extract X
  mtZ <- delete.response(terms(formula, data = data, rhs = 2))
  Z <- model.matrix(mtZ, mf, contrasts.arg = NULL) # extract Z

  n <- NROW(Y)
  k <- ncol(X)
  stopifnot(n == nrow(X))
  stopifnot(n == nrow(Z))
  stopifnot(ncol(Z) >= ncol(X))

  fs <- lm.fit(Z, X) # run first stage
  Xhat <- as.matrix(fs$fitted.values) # first stage fitted values
  colnames(Xhat) <- colnames(X)
  ss <- lm.fit(Xhat, Y) # run second stage
  notna <- which(!is.na(ss$coefficients)) # regressors for which coefficient is not NA
  Yhat <- drop(X[, notna, drop = FALSE] %*% ss$coefficients[notna]) # fitted values of second stage using actual x values
  names(Yhat) <- names(Y)
  res <- Y - Yhat # residuals of second stage
  varcov <- chol2inv(ss$qr$qr[1:length(notna), 1:length(notna), drop = FALSE])
  colnames(varcov) <- rownames(varcov) <- names(ss$coefficients[notna])
  sigma <- sqrt(sum(res^2)/ss$df.residual)

  rval <- list(coefficients = ss$coefficients, residuals = res,
               fitted.values = Yhat, n = n, nobs = n, k = k,
               cov.unscaled = varcov, sigma = sigma)

  # give class "ivreg" so that can use methods for ivreg objects
  class(rval) <- "ivreg"
  return(rval)

}


