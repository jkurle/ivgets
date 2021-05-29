
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ivgets

<!-- badges: start -->
<!-- badges: end -->

The *ivgets* package provides general-to-specific modeling functionality
for two-stage least squares (2SLS or TSLS) models. There are two main
uses. Starting from a generalized unrestricted model (GUM), the
algorithm searches over the set of exogenous regressors to find a
specification that can explain the data while trying to be as
parsimonious as possible. Second, the package can perform indicator
saturation methods to detect outliers and structural breaks in the data.

## Installation

You can install the released version of ivgets from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ivgets")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jkurle/ivgets")
```

## Dependencies

The *ivgets* package relies heavily on two packages. The estimation of
2SLS models is based on the [ivreg](https://john-d-fox.github.io/ivreg/)
package and model selection uses the
[gets](http://www.sucarrat.net/R/gets/) package.

### Versions

*ivgets* avoids specifying minimum or maximum versions of the
dependencies in the `DESCRIPTION` file if possible. This is to avoid
forcing users to update their packages and potentially break existing
code.

For reference, *ivgets* has been developed under the following versions:

-   R version: 4.1.0
-   ivreg: 0.5.0
-   gets: 0.27 (my own fork, pull request \#3, see `DESCRIPTION`)
-   stringr: 1.4.0
-   knitr: 1.33
-   rmarkdown: 2.8
-   testthat: 3.0.2

## Example

### Setup

Suppose we want to model the potential effect of some regressors
*x*<sub>1</sub> to *x*<sub>11</sub> on a dependent variable *y*. We are
worried that *x*<sub>11</sub> could be endogenous, so we use a 2SLS
model to estimate the parameters. For the exogenous regressors
*x*<sub>1</sub> to *x*<sub>10</sub> we are unsure whether they are
relevant but our theory tells us that they might be relevant. So we want
to include all of them in the original model and then use model
selection to determine which regressors are actually relevant.
Furthermore suppose we are concerned that the sample might contain
outlying observations and that these outliers are biasing our results.

Formally, our structural equation is

*y*<sub>*i*</sub> = *β*<sub>1</sub>*x*<sub>1*i*</sub> + *β*<sub>2</sub>*x*<sub>2*i*</sub> + ... + *β*<sub>11</sub>*x*<sub>11*i*</sub> + *u*<sub>*i*</sub> = *x*<sub>*i*</sub><sup>′</sup>*β* + *u*<sub>*i*</sub>
.

The first stage can be written as

*x*<sub>*i*</sub> = *Π*<sup>′</sup>*z*<sub>*i*</sub> + *r*<sub>*i*</sub>
,

where *z*<sub>*i*</sub> includes all the exogenous regressors
*x*<sub>1</sub> to *x*<sub>10</sub> and the excluded instruments
*z*<sub>11</sub> and *z*<sub>12</sub>.

### Indicator Saturation

Since we are concerned about outliers, we first do impulse indicator
saturation to detect observations with unusually large errors. We still
include all our potentially relevant exogenous regressors in the model.

``` r
library(ivgets)
# we specify "-1" in the formula because x1 is already an intercept in our data frame
fml <- y ~ -1+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11 | -1+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+z11+z12
base <- ivreg(formula = fml, data = artificial2sls_contaminated)
# do impulse indicator saturation
indicators <- isat(base, iis = TRUE, t.pval = 1/100, print.searchinfo = FALSE)
print(indicators$final)
#> 
#> Call:
#> ivreg(formula = as.formula(fml_sel), data = d)
#> 
#> Coefficients:
#>       x1        x2        x3        x4        x5        x6        x7        x8  
#>  5.26264  -4.89609   0.13239   0.12368  -0.14055  -0.13028   0.68213   0.13232  
#>       x9       x10      iis9     iis11     iis43     iis73       x11  
#>  0.03797  -0.12167   3.09339   3.39235   3.08462   2.91605   3.24507
```

For the selection of indicators, we use a significance level, `t.pval`,
of 1/100. The data set has 100 observations and we select over 100
impulse indicators. So we expect to falsely retain one indicator on
average. As the output shows, the algorithm has retained four
indicators: iis9, iis11, iis43, iis73. Since this is artificial data, we
know which observations were outliers. In this case, all of the retained
indicators correspond to actual outlying observations 9, 11, 43, and 73.
The algorithm has only missed one additional outlier, which was
observation 78.

The object `indicators` is a list with two entries. The first entry,
`$selection`, stores the information related to the search, such as the
number of estimations and all terminal models. The second entry,
`$final`, is an object of class `"ivreg"` and is the model result of the
final model.

### General-to-Specific Theory Modeling

Now that we have identified (some of the) outliers, we still want to
find out which of our theoretical exogenous regressors are actually
relevant. As before, we can simply pass the final model from the
previous step to the `gets()` method. Since we do not want to select
over the impulse indicators again, we need to specify this accordingly
in the function call. The names of the indicators are conveniently saved
in the `$selection$ISnames` entry.

``` r
selection <- gets(indicators$final, keep_exog = indicators$selection$ISnames, print.searchinfo = FALSE)
print(selection$final)
#> 
#> Call:
#> ivreg(formula = as.formula(fml_sel), data = d)
#> 
#> Coefficients:
#>     x1      x2    iis9   iis11   iis43   iis73     x11  
#>  5.864  -4.988   3.072   3.424   3.205   3.087   2.943
```

As before, the returned object is a list with entry `$selection`, which
stores information about the search, and `$final`, which is an `"ivreg"`
model object of the final model. There is an additional third entry
named `$keep` that specifies the names of all regressors in the second
stage that were not selected over. This can be used as a check that the
selection was done correctly.

``` r
# as specified, the impulse indicators and the endogenous regressor, x11, were not selected over
print(selection$keep)
#> [1] "iis9"  "iis11" "iis43" "iis73" "x11"
```

The final model has only retained the exogenous theory variables
*x*<sub>1</sub> and *x*<sub>2</sub>, which is the correct selection. The
data generating process only contained the variables *x*<sub>1</sub>,
*x*<sub>2</sub>, and *x*<sub>11</sub>. Their true parameters were
`c(6, -5, 3)`, so the estimates are quite close.
