# CRAN comments for: ivgets
#### 12 July 2024, version 0.1.2

### fix package to restore compatibility with incoming gets version 0.38

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
* own machine:
  * Windows 11, version 10.0.22631, Build 22631, x86_64-w64-mingw32/x64 (64-bit), R version 4.4.1

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

## Downstream Dependencies

* robust2sls: maintained by myself, still compatible

## Past NOTE (v0.1.0):

* The title should contain "2SLS" and not "2sls" because this is a standard abbreviation in the econometrics and statistics literature. "2SLS" stands for "Two-Stage Least Squares". See also, for example, https://www.stata.com/manuals/rivregress.pdf

