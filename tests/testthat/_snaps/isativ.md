# ivisat() works correctly

    $selection
    
    Date: Tue May 25 14:09:19 2021 
    Dependent var.: y 
    Method: Ordinary Least Squares (OLS)
    Variance-Covariance: Ordinary 
    No. of observations (mean eq.): 50 
    Sample: 1 to 50 
    
    SPECIFIC mean equation:
    
              coef std.error  t-stat   p-value    
    cons   1.04674   0.12750  8.2095 1.711e-10 ***
    x1     1.66156   0.13146 12.6391 < 2.2e-16 ***
    x2    -1.04996   0.12300 -8.5364 5.788e-11 ***
    iis16  2.37449   0.88440  2.6849   0.01012 *  
    iis18 -2.31272   0.86844 -2.6631   0.01070 *  
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    
    Diagnostics and fit:
    
                      Statistic df p-value
    Ljung-Box AR(1)     1.77243  1  0.1831
    Ljung-Box ARCH(1)   0.95742  1  0.3278
                              
    SE of regression   0.83540
    R-squared          0.87338
    Log-lik.(n=50)   -59.32075
    
    $final
    
    Call:
    ivreg::ivreg(formula = as.formula(fml_sel), data = d)
    
    Coefficients:
      cons      x1   iis16   iis18      x2  
     1.047   1.662   2.374  -2.313  -1.050  
    
    
    attr(,"class")
    [1] "ivisat"

---

    $selection
    
    Date: Tue May 25 14:09:35 2021 
    Dependent var.: y 
    Method: Ordinary Least Squares (OLS)
    Variance-Covariance: Ordinary 
    No. of observations (mean eq.): 50 
    Sample: 1 to 50 
    
    SPECIFIC mean equation:
    
             coef std.error  t-stat   p-value    
    cons  1.05156   0.14129  7.4427 1.749e-09 ***
    x1    1.78997   0.14246 12.5644 < 2.2e-16 ***
    x2   -1.05753   0.13956 -7.5779 1.094e-09 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    
    Diagnostics and fit:
    
                      Statistic df p-value
    Ljung-Box AR(1)    0.087717  1  0.7671
    Ljung-Box ARCH(1)  1.104955  1  0.2932
                              
    SE of regression   0.94471
    R-squared          0.83088
    Log-lik.(n=50)   -66.55617
    
    $final
    
    Call:
    ivreg::ivreg(formula = as.formula(fml_sel), data = d)
    
    Coefficients:
      cons      x1      x2  
     1.052   1.790  -1.058  
    
    
    attr(,"class")
    [1] "ivisat"

