#' Indicator saturation modeling for 2SLS models
#'
#' @inheritParams gets::isat
#' @inheritParams ivgets
#' @param formula A formula in the format \code{y ~ x1 + x2 | z1 + z2}.
#' @param data A data frame with all necessary variables y, x, and z.
#' @param uis a matrix of regressors, or a list of matrices. If a list, the
#'   matrices must have named columns that should not overlap with column names
#'   of any other matrices in the list.
#'
#' @return Returns a list of class \code{"ivisat"} with two named elements.
#'   \code{$selection} stores the selection results from
#'   \code{\link[gets]{isat}} (including paths, terminal models, and best
#'   specification). \code{$final} stores the \code{\link[ivreg]{ivreg}} model
#'   object of the best specification or \code{NULL} if the GUM does not pass
#'   all diagnostics.
#'
#' @importFrom gets isat
#' @export

ivisat <- function(
  # ivreg::ivreg arguments
  # unused: subset, na.action, weights, offset, contrasts, model, x, y
  formula, data,
  # gets::isat() arguments
  # unused: y, mc, ar, ewma, mxreg, vcov.type, user.diagnostics, user.estimator,
  # gof.function, gof.method, LAPACK, include.gum (deprecated)
  iis = TRUE, sis = FALSE, tis = FALSE, uis = FALSE, blocks = NULL,
  ratio.threshold = 0.8, max.block.size = 30, t.pval = 1/NROW(data),
  wald.pval = t.pval, do.pet = FALSE, ar.LjungB = NULL, arch.LjungB = NULL,
  normality.JarqueB = NULL, info.method = c("sc","aic","hq"),
  include.1cut = FALSE, include.empty = FALSE,
  max.paths = NULL, parallel.options = NULL, turbo = FALSE, tol = 1e-07,
  max.regs = NULL, print.searchinfo = TRUE, plot = NULL, alarm = FALSE,
  # new arguments
  overid = NULL, weak = NULL) {

  if (is.list(uis)) {
    for (ele in seq_along(uis)) {
      if (is.null(colnames(uis[[ele]]))) {
        stop("The matrices in the list 'uis' all must have column names.")
      }
    }
  }

  # in isat, all regressors will be kept anyway
  # so can specify keep_exog = NULL
  setup <- new_formula(formula = formula, data = data, keep_exog = NULL)

  # user estimator
  if (is.null(overid) & is.null(weak)) {
    test <- FALSE
  } else {
    test <- TRUE
  }
  userest <- list(name = ivgets::ivregFun, z = setup$z,
                  formula = as.formula(setup$fml), test = test)

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
  # always specify mc = FALSE because intercept will have been created if needed
  # if want to test, use userdia; otherwise don't
  if (test == TRUE) {
    a <- gets::isat(y = setup$y, mc = FALSE, ar = NULL, ewma = NULL,
                    mxreg = setup$x, iis = iis, sis = sis, tis = tis, uis = uis,
                    blocks = blocks, ratio.threshold = ratio.threshold,
                    max.block.size = max.block.size, t.pval = t.pval,
                    wald.pval = wald.pval, vcov.type = "ordinary",
                    do.pet = do.pet, ar.LjungB = ar.LjungB,
                    arch.LjungB = arch.LjungB,
                    normality.JarqueB = normality.JarqueB,
                    info.method = info.method, user.diagnostics = userdia,
                    user.estimator = userest, gof.function = NULL,
                    gof.method = "min", include.gum = NULL,
                    include.1cut = include.1cut, include.empty = include.empty,
                    max.paths = max.paths, parallel.options = parallel.options,
                    turbo = turbo, tol = tol, LAPACK = FALSE,
                    max.regs = max.regs, print.searchinfo = print.searchinfo,
                    plot = plot, alarm = alarm)
  } else {
    a <- gets::isat(y = setup$y, mc = FALSE, ar = NULL, ewma = NULL,
                    mxreg = setup$x, iis = iis, sis = sis, tis = tis, uis = uis,
                    blocks = blocks, ratio.threshold = ratio.threshold,
                    max.block.size = max.block.size, t.pval = t.pval,
                    wald.pval = wald.pval, vcov.type = "ordinary",
                    do.pet = do.pet, ar.LjungB = ar.LjungB,
                    arch.LjungB = arch.LjungB,
                    normality.JarqueB = normality.JarqueB,
                    info.method = info.method, user.diagnostics = NULL,
                    user.estimator = userest, gof.function = NULL,
                    gof.method = "min", include.gum = NULL,
                    include.1cut = include.1cut, include.empty = include.empty,
                    max.paths = max.paths, parallel.options = parallel.options,
                    turbo = turbo, tol = tol, LAPACK = FALSE,
                    max.regs = max.regs, print.searchinfo = print.searchinfo,
                    plot = plot, alarm = alarm)
  }


  if (a$no.of.estimations == a$no.of.getsFun.calls) {
    warning("No selection was undertaken. Probable reason: https://github.com/gsucarrat/gets/issues/39")
    out <- list(selection = a, final = NULL)
  } else {
    # create indicators to run final model
    ISnames <- a$ISnames # is NULL if no indicators were selected
    #iisnames <- ISnames[grepl(x = ISnames, pattern = "^iis[1-9]+$")]
    #sisnames <- ISnames[grepl(x = ISnames, pattern = "^sis[1-9]+$")]
    #tisnames <- ISnames[grepl(x = ISnames, pattern = "^tis[1-9]+$")]
    # have length zero if no such pattern found

    indicators <- NULL
    if (!is.null(ISnames)) {

      creator <- factory_indicators(n = NROW(data))
      for (ind in seq_along(ISnames)) {
        indicator <- creator(name = ISnames[ind], uis = uis)
        indicators <- cbind(indicators, indicator)
      } # end for

    }

    vars_sel <- names(a$specific.spec)
    x1_sel <- intersect(setup$x1, vars_sel)
    x2_sel <- setup$x2 # do not select over endog. regressors, so stays the same
    x1_sel <- union(x1_sel, ISnames) # if NULL then nothing is added
    z1_sel <- x1_sel
    z2_sel <- setup$z2 # do not select over excluded instr., so stays the same
    x_sel <- paste(c("-1", x1_sel, x2_sel), sep = "", collapse = "+")
    z_sel <- paste(c("-1", z1_sel, z2_sel), sep = "", collapse = "+")
    fml_sel <- paste(c(setup$depvar, x_sel), sep = "", collapse = " ~ ")
    fml_sel <- paste(c(fml_sel, z_sel), sep = "", collapse = " | ")
    y <- setup$y
    x <- setup$x
    z <- setup$z
    d <- data.frame(cbind(y, x, indicators, z))
    fin <- ivreg::ivreg(formula = as.formula(fml_sel), data = d)
    out <- list(selection = a, final = fin)

  }

  class(out) <- "ivisat"
  return(out)

}



#' Indicator saturation modeling on an ivreg object
#'
#' \code{isat.ivreg} conducts indicator saturation model selection on an ivreg
#' object returned by [ivreg::ivreg()].
#'
#' @inheritParams ivisat
#' @param y An object of class \code{"ivreg"}, as returned by
#' [ivreg::ivreg()].
#' @param ... Further arguments passed to or from other methods.
#'
#' @inherit ivisat return
#'
#' @importFrom gets isat
#' @export

isat.ivreg <- function(
  y,
  # gets::isat() arguments
  # unused: y, mc, ar, ewma, mxreg, vcov.type, user.diagnostics, user.estimator,
  # gof.function, gof.method, LAPACK, include.gum (deprecated)
  iis = TRUE, sis = FALSE, tis = FALSE, uis = FALSE, blocks = NULL,
  ratio.threshold = 0.8, max.block.size = 30, t.pval = 1/NROW(data),
  wald.pval = t.pval, do.pet = FALSE, ar.LjungB = NULL, arch.LjungB = NULL,
  normality.JarqueB = NULL, info.method = c("sc","aic","hq"),
  include.1cut = FALSE, include.empty = FALSE,
  max.paths = NULL, parallel.options = NULL, turbo = FALSE, tol = 1e-07,
  max.regs = NULL, print.searchinfo = TRUE, plot = NULL, alarm = FALSE,
  # new arguments
  overid = NULL, weak = NULL,
  ...) {

  # R's method dispatch will already return error if no method defined for that class
  # so check is redundant, keep if people call explicitly isat.ivreg()
  if (!identical(class(y), "ivreg")) { # nocov start
    stop("Argument 'y' must be an ivreg object from the package
         ivreg.")
  } # nocov end
  if (is.null(y$model)) {
    stop("Please specify 'model = TRUE' in the original function call so the
         data is part of the model object.")
  }
  if (!is.null(y$weights)) {
    stop("GETS modelling currently does not support weights.")
  }

  # can build on the ivgets function, only need to extract the data and formula
  formula <- y$formula
  data <- y$model

  aux <- ivisat(formula = formula, data = data, iis = iis, sis = sis, tis = tis,
                uis = uis, blocks = blocks, ratio.threshold = ratio.threshold,
                max.block.size = max.block.size, t.pval = t.pval,
                wald.pval = wald.pval, do.pet = do.pet, ar.LjungB = ar.LjungB,
                arch.LjungB = arch.LjungB,
                normality.JarqueB = normality.JarqueB,
                info.method = info.method, include.1cut = include.1cut,
                include.empty = include.empty, max.paths = max.paths,
                parallel.options = parallel.options, turbo = turbo, tol = tol,
                max.regs = max.regs, print.searchinfo = print.searchinfo,
                plot = plot, alarm = alarm, overid = overid, weak = weak)

  return(aux)

}












