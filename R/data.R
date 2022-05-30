#' Artificial data set for illustration.
#'
#' A data set containing dependent variable, endogenous and exogenous
#' regressors, and excluded instruments for 2SLS models. The structural error is
#' also stored even though not observed in practice.
#'
#' @format A data frame with 100 observations (rows) and 16 variables (columns):
#' \tabular{rl}{
#'   \strong{name} \tab \strong{variable description} \cr
#'   y \tab dependent variable \cr
#'   x1 \tab intercept \cr
#'   x2 \tab relevant exogenous regressor \cr
#'   x3 \tab irrelevant exogenous regressor \cr
#'   x4 \tab irrelevant exogenous regressor \cr
#'   x5 \tab irrelevant exogenous regressor \cr
#'   x6 \tab irrelevant exogenous regressor \cr
#'   x7 \tab irrelevant exogenous regressor \cr
#'   x8 \tab irrelevant exogenous regressor \cr
#'   x9 \tab irrelevant exogenous regressor \cr
#'   x10 \tab irrelevant exogenous regressor \cr
#'   x11 \tab relevant endogenous regressor \cr
#'   u \tab structural error (in practice unobserved) \cr
#'   z11 \tab excluded instrument \cr
#'   z12 \tab excluded instrument \cr
#'   id \tab unique observation identifier
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
#' \tabular{rl}{
#'   \strong{name} \tab \strong{variable description} \cr
#'   y \tab dependent variable \cr
#'   x1 \tab intercept \cr
#'   x2 \tab relevant exogenous regressor \cr
#'   x3 \tab irrelevant exogenous regressor \cr
#'   x4 \tab irrelevant exogenous regressor \cr
#'   x5 \tab irrelevant exogenous regressor \cr
#'   x6 \tab irrelevant exogenous regressor \cr
#'   x7 \tab irrelevant exogenous regressor \cr
#'   x8 \tab irrelevant exogenous regressor \cr
#'   x9 \tab irrelevant exogenous regressor \cr
#'   x10 \tab irrelevant exogenous regressor \cr
#'   x11 \tab relevant endogenous regressor \cr
#'   u \tab structural error (in practice unobserved) \cr
#'   z11 \tab excluded instrument \cr
#'   z12 \tab excluded instrument \cr
#'   id \tab unique observation identifier
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
#' \tabular{rl}{
#'   \strong{name} \tab \strong{variable description} \cr
#'   y \tab dependent variable \cr
#'   x1 \tab intercept \cr
#'   x2 \tab relevant exogenous regressor \cr
#'   x3 \tab irrelevant exogenous regressor \cr
#'   x4 \tab irrelevant exogenous regressor \cr
#'   x5 \tab irrelevant exogenous regressor \cr
#'   x6 \tab irrelevant exogenous regressor \cr
#'   x7 \tab irrelevant exogenous regressor \cr
#'   x8 \tab irrelevant exogenous regressor \cr
#'   x9 \tab irrelevant exogenous regressor \cr
#'   x10 \tab irrelevant exogenous regressor \cr
#'   x11 \tab relevant endogenous regressor \cr
#'   u \tab structural error (in practice unobserved) \cr
#'   z11 \tab excluded instrument \cr
#'   z12 \tab excluded instrument \cr
#'   id \tab unique observation identifier \cr
#'   is.outlier \tab factor variable whether the observation is an outlier (\code{1}) or not (\code{0})
#' }
"artificial2sls_shiny"

