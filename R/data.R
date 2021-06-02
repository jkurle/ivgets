#' Artificial data set for illustration.
#'
#' A data set containing dependent variable, endogenous and exogenous
#' regressors, and excluded instruments for 2SLS models. The structural error is
#' also stored even though not observed in practice.
#'
#' @format A data frame with 100 observations (rows) and 15 variables (columns):
#' \describe{
#'   \item{y}{dependent variable}
#'   \item{x1}{intercept}
#'   \item{x2}{relevant exogenous regressor}
#'   \item{x3 - x10}{irrelevant exogenous regressors}
#'   \item{x11}{relevant endogenous regressor}
#'   \item{u}{structural error (in practice unobserved)}
#'   \item{z11 - z12}{excluded instruments}
#' }
"artificial2sls"


#' Artificial data set with outliers for illustration.
#'
#' A data set containing dependent variable, endogenous and exogenous
#' regressors, and excluded instruments for 2SLS models. The structural error is
#' also stored even though not observed in practice. Some errors are
#' contaminated, making these observations outliers.
#'
#' @format A data frame with 100 observations (rows) and 15 variables (columns):
#' \describe{
#'   \item{y}{dependent variable}
#'   \item{x1}{intercept}
#'   \item{x2}{relevant exogenous regressor}
#'   \item{x3 - x10}{irrelevant exogenous regressors}
#'   \item{x11}{relevant endogenous regressor}
#'   \item{u}{structural error (in practice unobserved)}
#'   \item{z11 - z12}{excluded instruments}
#' }
#'
#' @details The data frame has two additional attributes that store the indices
#'   of the outliers, \code{"outliers"}, and their magnitudes
#'   \code{"magnitude"}.
#'
"artificial2sls_contaminated"

#' Artificial data set prepared for shiny application
#'
#' Similar to "artificial2sls" with a few minor additions.
#'
#' @format A data frame with 100 observations (rows) and 17 variables (columns)
"artificial2sls_shiny"

