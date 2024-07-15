# CRAN comments for: ivgets
#### 15 July 2024, version 0.1.2

### fix package to restore compatibility with gets version 0.38

## Test environments

* GitHub Actions:
  * Windows 10.0.20348, Build 1129, R version 4.4.1
  * MacOS 14.5, darwin-23.5.0, R version 4.4.1
  * Ubuntu 20.04.4, 6.5.0-1023-azure, R development version 4.5.0 (2024-07-13 r86895)
  * Ubuntu 20.04.4, 6.5.0-1023-azure, R version 4.4.1 (release)
  * Ubuntu 20.04.4, 6.5.0-1023-azure, R version 4.3.3 (oldrelease)
* winbuilder:
  * Windows Server 2022 x86_64-w64-mingw32 (64-bit), R development version (2024-07-14 r86899 ucrt)
  * Windows Server 2022 x86_64-w64-mingw32 (64-bit), R version 4.4.1 (release)
* own machine:
  * Windows 11, version 10.0.22631, Build 22631, x86_64-w64-mingw32/x64 (64-bit), R version 4.4.1

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

## Downstream Dependencies

* robust2sls: maintained by myself, still compatible

## Past NOTE (v0.1.0):

* The title should contain "2SLS" and not "2sls" because this is a standard abbreviation in the econometrics and statistics literature. "2SLS" stands for "Two-Stage Least Squares". See also, for example, https://www.stata.com/manuals/rivregress.pdf

