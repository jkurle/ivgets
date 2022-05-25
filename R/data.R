#' Artificial data set for illustration.
#'
#' A data set containing dependent variable, endogenous and exogenous
#' regressors, and excluded instruments for 2SLS models. The structural error is
#' also stored even though not observed in practice.
#'
#' @format A data frame with 100 observations (rows) and 16 variables (columns):
#' \describe{
#'   \item{y}{dependent variable}
#'   \item{x1}{intercept}
#'   \item{x2}{relevant exogenous regressor}
#'   \item{x3 - x10}{irrelevant exogenous regressors}
#'   \item{x11}{relevant endogenous regressor}
#'   \item{u}{structural error (in practice unobserved)}
#'   \item{z11 - z12}{excluded instruments}
#'   \item{id}{unique observation identifier}
#' }
"artificial2sls"


#' Artificial data set with outliers for illustration.
#'
#' A data set containing dependent variable, endogenous and exogenous
#' regressors, and excluded instruments for 2SLS models. The structural error is
#' also stored even though not observed in practice. Some errors are
#' contaminated, making these observations outliers.
#'
#' @format A data frame with 100 observations (rows) and 16 variables (columns):
#' \describe{
#'   \item{y}{dependent variable}
#'   \item{x1}{intercept}
#'   \item{x2}{relevant exogenous regressor}
#'   \item{x3 - x10}{irrelevant exogenous regressors}
#'   \item{x11}{relevant endogenous regressor}
#'   \item{u}{structural error (in practice unobserved)}
#'   \item{z11 - z12}{excluded instruments}
#'   \item{id}{unique observation identifier}
#' }
#'
#' @details The data frame has two additional attributes that store the indices
#'   of the outliers, \code{"outliers"}, and their magnitudes
#'   \code{"magnitude"}.
#'
"artificial2sls_contaminated"

#' Artificial data set without outliers prepared for shiny application.
#'
#' @format A data frame with 100 observations (rows) and 17 variables (columns):
#' \describe{
#'   \item{y}{dependent variable}
#'   \item{x1}{intercept}
#'   \item{x2}{relevant exogenous regressor}
#'   \item{x3 - x10}{irrelevant exogenous regressors}
#'   \item{x11}{relevant endogenous regressor}
#'   \item{u}{structural error (in practice unobserved)}
#'   \item{z11 - z12}{excluded instruments}
#'   \item{id}{unique observation identifier}
#'   \item{is.outlier}{factor variable whether the observation is an outlier (\code{1}) or not (\code{0})}
#' }
"artificial2sls_shiny"

