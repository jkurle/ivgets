#' General-to-specific modelling for 2SLS models
#'
#' @inheritParams gets::getsFun
#' @param keep_exog A numeric vector of indices or a character vector of names
#' corresponding to the exogenous regressors in the \code{data} that should not
#' be selected over. Default \code{NULL} means that selection is over all
#' exogenous regressors. If an intercept has been specified in the
#' \code{formula} but is not already included in the \code{data}, then it can be
#' kept by either including the index \code{0} or the character
#' \code{"Intercept"}, respectively, as an element in \code{keep_exog}.
#' @param overid \code{NULL} if no Sargan test of overidentifying restrictions
#' should be used as a diagnostic check for model selection or a numeric value
#' between 0 and 1. In the latter case, the test is conducted using this value
#' as the significance level.
#' @param weak \code{NULL} if no weak instrument F-test on the first stage
#' should be used as a diagnostic check for model selection or a numeric value
#' between 0 and 1. In the latter case, the test is conducted using this value
#' as the significance level.
#'
#' @importFrom gets gets
#' @export

ivgets <- function(
  # ivreg::ivreg arguments
  # unused: subset, na.action, weights, offset, contrasts, model, x, y
  formula, data,
  # gets::getsFun() arguments
  # unused: y, x, untransformed.residuals, user.estimator, gof.function,
  # gof.method, keep, LAPACK
  gum.result = NULL, t.pval = 0.05, wald.pval = t.pval, do.pet = TRUE,
  ar.LjungB = NULL, arch.LjungB = NULL, normality.JarqueB = NULL,
  include.gum = FALSE, include.1cut = FALSE, include.empty = FALSE,
  max.paths = NULL, turbo = FALSE, tol = 1e-07, max.regs = NULL,
  print.searchinfo = TRUE, alarm = FALSE,
  # new arguments
  keep_exog = NULL, overid = NULL, weak = NULL) {

  # transform the inputs such that can pass on to ivregFun()
  # mainly intercept handling
  setup <- new_formula(formula = formula, data = data, keep_exog = keep_exog)

  # user estimator
  userest <- list(name = ivgets::ivregFun, z = setup$z,
                  formula = as.formula(setup$fml))

  # user diagnostics
  is.reject.bad <- NULL
  pvalues <- NULL
  if (is.null(weak)) {
    weak <- FALSE
  } else {
    # how many first stage equations (and hence weak instrument F-tests)
    weak.n <- setup$dx2
    weak.pval <- rep(weak, weak.n)
    pvalues <- c(pvalues, weak.pval)
    weak.is.reject.bad <- rep(FALSE, weak.n)
    is.reject.bad <- c(is.reject.bad, weak.is.reject.bad)
    weak <- TRUE
  }
  if (is.null(overid)) {
    overid <- FALSE
  } else {
    # determine degree of overidentification
    overid.degree <- setup$dz2 - setup$dx2
    if (overid.degree < 1) {
      overid <- FALSE
      warning("Sargan test of overidentifying restrictions cannot be calculated
              because model is not overidentified. Argument was set to FALSE.")
    } else {
      overid.pval <- overid
      pvalues <- c(pvalues, overid.pval)
      overid <- TRUE
      overid.is.reject.bad <- TRUE
      is.reject.bad <- c(is.reject.bad, overid.is.reject.bad)
    }
  }
  # is.reject.bad is NULL if both tests are not used
  userdia <- list(name = ivgets::ivDiag, weak = weak, overid = overid,
                  pval = pvalues, is.reject.bad = is.reject.bad)

  # do model selection
  a <- gets::getsFun(y = setup$y, x = setup$x, untransformed.residuals = NULL,
                     user.estimator = userest, gum.result = gum.result,
                     t.pval = t.pval, wald.pval = wald.pval, do.pet = do.pet,
                     ar.LjungB = ar.LjungB, arch.LjungB = arch.LjungB,
                     normality.JarqueB = normality.JarqueB,
                     user.diagnostics = userdia,
                     gof.function = list(name = "infocrit"), gof.method = "min",
                     keep = setup$keep, include.gum = include.gum,
                     include.1cut = include.1cut, include.empty = include.empty,
                     max.paths = max.paths, turbo = turbo, tol = tol,
                     LAPACK = FALSE, max.regs = max.regs,
                     print.searchinfo = print.searchinfo, alarm = alarm)

  if ("- GUM does not pass one or more diagnostic checks" %in% a$messages) {
    warning("GUM does not pass one or more diagnostic checks. No selection.")
    out <- list(selection = a, final = NULL, keep = setup$keep.names)
  } else {
    vars_sel <- names(a$specific.spec)
    x1_sel <- intersect(setup$x1, vars_sel)
    x2_sel <- setup$x2 # do not select over endog. regressors, so stays the same
    z1_sel <- x1_sel
    z2_sel <- setup$z2 # do not select over excluded instr., so stays the same
    x_sel <- paste(c("-1", x1_sel, x2_sel), sep = "", collapse = "+")
    z_sel <- paste(c("-1", z1_sel, z2_sel), sep = "", collapse = "+")
    fml_sel <- paste(c(setup$depvar, x_sel), sep = "", collapse = " ~ ")
    fml_sel <- paste(c(fml_sel, z_sel), sep = "", collapse = " | ")
    y <- setup$y
    x <- setup$x
    z <- setup$z
    d <- data.frame(cbind(y, x, z))
    fin <- ivreg::ivreg(formula = as.formula(fml_sel), data = d)
    out <- list(selection = a, final = fin, keep = setup$keep.names)
  }

  class(out) <- "ivgets"

  return(out)

}

#' Gets modelling on an ivreg object
#'
#' \code{gets.ivreg} conducts general-to-specific model selection on an ivreg
#' object returned by [ivreg::ivreg()].
#'
#' @inheritParams ivgets
#' @param ivreg_object An object of class \code{"ivreg"}, as returned by
#' [ivreg::ivreg()].
#'
#' @importFrom gets gets
#' @export

gets.ivreg <- function(
  ivreg_object,
  # gets::getsFun() arguments
  # unused: untransformed.residuals, user.estimator, gof.function, gof.method,
  # keep, LAPACK
  gum.result = NULL, t.pval = 0.05, wald.pval = t.pval, do.pet = TRUE,
  ar.LjungB = NULL, arch.LjungB = NULL, normality.JarqueB = NULL,
  include.gum = FALSE, include.1cut = FALSE, include.empty = FALSE,
  max.paths = NULL, turbo = FALSE, tol = 1e-07, max.regs = NULL,
  print.searchinfo = TRUE, alarm = FALSE,
  # new arguments
  keep_exog = NULL, overid = NULL, weak = NULL) {

  if (!identical(class(ivreg_object), "ivreg")) {
    stop("Argument 'ivreg_object' must be an ivreg object from the package
         ivreg.")
  }
  if (is.null(ivreg_object$model)) {
    stop("Please specify 'model = TRUE' in the original function call so the
         data is part of the model object.")
  }
  if (!is.null(ivreg_object$weights)) {
    stop("GETS modelling currently does not support weights.")
  }

  # can build on the ivgets function, only need to extract the data and formula
  formula <- ivreg_object$formula
  data <- ivreg_object$model

  aux <- ivgets(formula = formula, data = data, gum.result = gum.result,
                t.pval = t.pval, wald.pval = t.pval, do.pet = do.pet,
                ar.LjungB = ar.LjungB, arch.LjungB = arch.LjungB,
                normality.JarqueB = normality.JarqueB,
                include.gum = include.gum, include.1cut = include.1cut,
                include.empty = include.empty, max.paths = max.paths,
                turbo = turbo, tol = tol, max.regs = max.regs,
                print.searchinfo = print.searchinfo, alarm = alarm,
                keep_exog = keep_exog, overid = overid, weak = weak)

  return(aux)

}


























