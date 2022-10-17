# CRAN comments for: ivgets
#### 17 October 2022, version 0.1.1

### fix package to restore compatibility with gets version 0.37

## Test environments

* GitHub Actions:
  * Windows 10.0.20348, Build 1129, R version 4.2.1
  * MacOS 11.7, darwin-20.6.0, R version 4.2.1
  * Ubuntu 20.04.5, 5.15.0-1020-azure, R development version (2022-10-16 r83107)
  * Ubuntu 20.04.5, 5.15.0-1020-azure, R version 4.2.1 (release)
  * Ubuntu 20.04.5, 5.15.0-1020-azure, R version 4.1.3 (oldrelease)
* winbuilder:
  * Windows Server 2022 x86_64-w64-mingw32 (64-bit), R development version (2022-10-11 r83083)
  * Windows Server 2022 x86_64-w64-mingw32 (64-bit), R version 4.2.1 (release)
  * Windows Server 2008 x86_64-w64-mingw32 (64-bit), R version 4.1.3 (oldrelease)
* own machine:
  * Windows 11, version 21H2, Build 22621, x86_64-w64-mingw32/x64 (64-bit), R version 4.1.3

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

## Downstream Dependencies

* robust2sls: maintained by myself, still compatible [and will be updated soon]

## Past NOTE (v0.1.0):

* The title should contain "2SLS" and not "2sls" because this is a standard abbreviation in the econometrics and statistics literature. "2SLS" stands for "Two-Stage Least Squares". See also, for example, https://www.stata.com/manuals/rivregress.pdf

