#' Extract the first and second stage regressors of ivreg formula
#'
#' \code{extract_regressors} takes a formula object for [ivreg::ivreg()], i.e.
#' in a format of \code{y ~ x1 + x2 | x1 + z2} and extracts the different
#' elements in a list.
#'
#' @param formula A formula for the [ivreg::ivreg] function, i.e. in format
#'   \code{y ~ x1 + x2 | z1 + z2}.
#'
#' @return \code{extract_regressors} returns a list with three components:
#'   \code{$yvar} stores the name of the dependent variable, \code{$first} the
#'   names of the regressors of the first stage and \code{$second} of the second
#'   stage regressors.
#'
#' @export

extract_regressors <- function(formula) {

  # convert formula to character vector of length 1
  # use Reduce() to avoid length > 1 if formula is too long
  fml <- Reduce(paste, deparse(formula))

  # check that formula contains both "~" and "|" symbols
  if (!(grepl("~", fml) && grepl("|", fml))) {
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
#' format of \code{y ~ x1 + x2 | x1 + z2} and returns a new formula and data
#' suitable for model selection.
#'
#' @inheritParams extract_regressors
#' @inheritParams ivgets
#' @param data A data frame.
#'
#' @return A list
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
      keep_names <- colnames(df)[setdiff(keep_exog, 0)]
    }
  } else if (is.character(keep_exog)) {
    keep_intercept <- ("Intercept" %in% keep_exog)
    if (length(setdiff(keep_exog, "Intercept")) == 0) { # only keep intercept
      keep_names <- NULL
    } else {
      keep_names <- setdiff(keep_exog, "Intercept")
      if (any(!(keep_names %in% colnames(df)))) {
        stop("Argument 'keep_exog' specifies names that cannot be found in the
           data frame.")
      }
    }
  } else {
    keep_names <- NULL
  }
  # keep_intercept is TRUE is keep_exog had index 0 or "Intercept"
  # keep_names contains the names of variables excluding any intercept name
  # keep_names is NULL if no variables should be kept (except intercept maybe)

  # check formula: extract regressors and check presence of intercept
  vars <- extract_regressors(formula = formula)

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
    stop("If have intercept in the structural equation, it should also be in the
         first stage.")
  }
  if (intercept2 == FALSE & intercept1 == TRUE) {
    stop("Using intercept as excluded instrument in the first stage is currently
         not supported.")
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
  # create new structural equation regressor matrix
  if (intercept2) {
    xnames <- colnames(x)
    # check whether regressor named "Intercept" already present in formula
    if ("Intercept" %in% xnames) {
      stop("Formula specification creates an intercept but regressor named
           \"Intercept\" is also included in formula. Please specify \"-1\" in
           the formula.")
    }
    # add an intercept
    x <- cbind(1, x)
    colnames(x)[1] <- "Intercept"
    xnew <- gets::dropvar(x, silent = TRUE)
    # returns matrix as before but now has a column with constant 1 called
    # "Intercept". If had intercept present before then the other one is
    # dropped.
    dropnames <- setdiff(xnames, colnames(xnew))
    if (length(dropnames) > 1) { # rank deficient by more than 1
      stop("Original structural equation had multicollinearity issues. Please
           re-specify the initial model such that no multicollinearity arises.")
    }
    # dropnames can only have dropped a regressor that was perfectly collinear
    # with the intercept; so can delete that regressor from the exogenous regr.
    # but add the regressor Intercept
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
  if (length(keep) == 0) { keep <- NULL }

  # save number of the types of regressors
  dx1 <- length(x1_base)
  dx2 <- length(x2_base)
  dz1 <- length(z1_base)
  dz2 <- length(z2_base)
  if (!identical(dx1, dz1)) { # sanity check
    stop("Something went wrong. dx1 should be equal to dz1.")
  }

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





