#' User estimator ivreg for getsFun() and isat()
#'
#' \code{ivregFun} calls [ivreg::ivreg()] in a format that is suitable for the
#' model selection function [gets::getsFun()] and for the indicator saturation
#' function [gets::isat()].
#'
#' @param y A numeric vector with no missing values.
#' @param x A matrix or \code{NULL}.
#' @param z A numeric vector or matrix.
#' @param formula A formula in the format \code{y ~ x1 + x2 | z1 + z2}.
#'
#' @return A list with entries needed for model selection via [gets::getsFun()]
#'   or [gets::isat()].
#'
#' @details For the required outputs of user-specified estimators, see the
#'   article "User-Specified General-to-Specific and Indicator Saturation
#'   Methods" by Genaro Sucarrat, published in the R Journal:
#'   <https://journal.r-project.org/archive/2021/RJ-2021-024/index.html>
#'
#' @export

ivregFun <- function(y, x, z, formula, tests) {

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

    if ("Intercept" %in% colnames(x)) {
      int.index <- which("Intercept" == colnames(x))
      df <- data.frame(cbind(y, x, z))
      colnames(df)[int.index + 1] <- "Intercept"
    } else {
      df <- data.frame(cbind(y, x, z))
    }

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
    if (tests == TRUE) {
      result$diag <- summary(tmp)$diagnostics
    } else {
      result$diag <- NULL
    }
    result$residuals <- residuals(tmp)
    result$std.residuals <- residuals(tmp) / tmp$sigma

  } else { # regressor matrix empty

    result$logl <- sum(stats::dnorm(y, sd = sqrt(var(y)), log = TRUE))
    result <- list(n = result$n, k = result$k, df = result$df,
                   coefficients = NULL, vcov = NULL, logl = result$logl,
                   diag = NULL, residuals = y, std.residuals = y / sqrt(var(y)))

  } # end if

  return(result)

}

#' User diagnostics for getsFun() and isat()
#'
#' \code{ivDiag} provides several diagnostic tests for 2SLS models that can be
#' used during model selection. Currently, a weak instrument F-test of the first
#' stage(s) and the Sargan test of overidentifying restrictions on the validity
#' of the instruments are implemented.
#'
#' @param x A list containing the estimation results of the 2SLS model. Must
#'   contain an entry \code{$diag} that contains the diagnostics provided by the
#'   [ivreg::ivreg()] command.
#' @param weak A logical value whether to conduct weak instrument tests.
#' @param overid A logical value whether to conduct the Sargan test of
#'   overidentifying restrictions.
#'
#' @return Returns a matrix with three columns named \code{"statistic"},
#'   \code{"df"}, and \code{"p-value"} and \emph{m} rows. Each row records these
#'   results for one of the tests, so the number of rows varies by the arguments
#'   specified and the model (e.g. how many first stages equations there are).
#'
#' @details The resulting matrix also has an attribute named
#'   \code{"is.reject.bad"}, which is a logical vector of length \emph{m}. Each
#'   entry records whether a rejection of the test means that the diagnostics
#'   have failed or vice versa. The first entry refers to the first row, the
#'   second entry to the second row etc. However, this attribute is not used in
#'   the following estimations. Instead, the decision rule is specified inside
#'   the \code{user.fun} argument of [gets::diagnostics()], which allows for a
#'   named entry \code{$is.reject.bad}.
#'
#' @export

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

  if (is.null(out)) {
    return(NULL)
  } else {
    out <- out[, c("statistic", "df1", "p-value", "df1", "df2"), drop = FALSE]
    colnames(out)[[2]] <- "df"
    # want to set df to NA for Weak instrument columns
    weakrows <- grepl(pattern = "^Weak", x = rownames(out))
    out[weakrows, "df"] <- NA
    out <- out[, 1:3, drop = FALSE]
    attr(out, "is.reject.bad") <- diagnostic.rule
    return(out)
  }

}
