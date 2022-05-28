# ivregFun() works correctly

    $n
    [1] 10
    
    $k
    [1] 3
    
    $df
    [1] 7
    
    $coefficients
          cons         x1         x2 
    -0.6306690 -0.4191888 -1.8672243 
    
    $vcov
              cons        x1       x2
    cons 0.7838740 0.6720186 1.451202
    x1   0.6720186 1.4969863 2.318460
    x2   1.4512021 2.3184602 4.557398
    
    $logl
    [1] -18.02986
    
    $diag
                     df1 df2 statistic   p-value
    Weak instruments   1   7  1.025591 0.3449143
    Wu-Hausman         1   6  4.720789 0.0727866
    Sargan             0  NA        NA        NA
    
    $residuals
             1          2          3          4          5          6          7 
    -1.4105516  0.1443134  0.4415958 -0.6134291 -0.6401341 -0.0546468  2.8646149 
             8          9         10 
    -1.1723941 -1.8873403  2.3279719 
    
    $std.residuals
              1           2           3           4           5           6 
    -0.80380024  0.08223672  0.25164254 -0.34956141 -0.36477925 -0.03114038 
              7           8           9          10 
     1.63239549 -0.66808659 -1.07549735  1.32659047 
    

---

    $n
    [1] 10
    
    $k
    [1] 2
    
    $df
    [1] 8
    
    $coefficients
             x1          x2 
    -0.07516201 -1.04208012 
    
    $vcov
              x1        x2
    x1 0.4528927 0.5482672
    x2 0.5482672 0.9547040
    
    $logl
    [1] -14.75167
    
    $diag
                     df1 df2 statistic   p-value
    Weak instruments   1   8  2.445488 0.1564919
    Wu-Hausman         1   7  3.366820 0.1091600
    Sargan             0  NA        NA        NA
    
    $residuals
             1          2          3          4          5          6          7 
    -1.5812291 -0.4302805  0.5196523 -0.6807355 -0.5638314  0.0917034  1.3713769 
             8          9         10 
    -1.2530490 -1.8201674  0.8253777 
    
    $std.residuals
              1           2           3           4           5           6 
    -1.33696480 -0.36381185  0.43937770 -0.57557723 -0.47673215  0.07753729 
              7           8           9          10 
     1.15953005 -1.05948121 -1.53899256  0.69787543 
    

---

    $n
    [1] 10
    
    $k
    [1] 2
    
    $df
    [1] 8
    
    $coefficients
          cons         x2 
    -0.5949665 -1.5771480 
    
    $vcov
              cons        x2
    cons 0.4791219 0.6176424
    x2   0.6176424 1.4547862
    
    $logl
    [1] -16.94492
    
    $diag
                     df1 df2 statistic    p-value
    Weak instruments   1   8  1.891204 0.20634394
    Wu-Hausman         1   7  3.997164 0.08570893
    Sargan             0  NA        NA         NA
    
    $residuals
              1           2           3           4           5           6 
    -1.64962512  0.02101034  0.53551400 -0.48409441 -0.26152516 -0.35013344 
              7           8           9          10 
     2.37719687 -0.42820261 -1.88689670  2.12675622 
    
    $std.residuals
              1           2           3           4           5           6 
    -1.12010599  0.01426615  0.36361742 -0.32870320 -0.17757725 -0.23774284 
              7           8           9          10 
     1.61413186 -0.29075230 -1.28121491  1.44408106 
    

---

    $n
    [1] 10
    
    $k
    [1] 4
    
    $df
    [1] 6
    
    $coefficients
          iis3       cons         x1         x2 
     0.6530392 -0.6024583 -0.2870392 -1.5820248 
    
    $vcov
              iis3      cons       x1       x2
    iis3 4.9501225 0.6094575 1.633753 3.404255
    cons 0.6094575 0.7361781 0.744540 1.602357
    x1   1.6337532 0.7445400 1.781491 3.013889
    x2   3.4042546 1.6023575 3.013889 6.056984
    
    $logl
    [1] -16.64129
    
    $diag
                     df1 df2 statistic   p-value
    Weak instruments   1   6 0.5918302 0.4709024
    Wu-Hausman         1   5 3.1508118 0.1360592
    Sargan             0  NA        NA        NA
    
    $residuals
             1          2          3          4          5          6          7 
    -1.2959815  0.1307197  0.0000000 -0.4483871 -0.4166298  0.1620467  2.5316770 
             8          9         10 
    -0.9844591 -1.6836388  2.0046529 
    
    $std.residuals
              1           2           3           4           5           6 
    -0.78557881  0.07923774  0.00000000 -0.27179660 -0.25254646  0.09822705 
              7           8           9          10 
     1.53461428 -0.59674476 -1.02056314  1.21515070 
    

---

    $n
    [1] 10
    
    $k
    [1] 5
    
    $df
    [1] 5
    
    $coefficients
           iis3        sis8        cons          x1          x2 
     0.90430205 -0.55578925 -0.20023957 -0.05794749 -0.85561961 
    
    $vcov
              iis3        sis8       cons         x1         x2
    iis3 2.1709003  0.11384446  0.1040454 0.54726515  1.1057602
    sis8 0.1138445  0.97548486 -0.4447867 0.08913391 -0.2877374
    cons 0.1040454 -0.44478668  0.5007005 0.19983681  0.6674774
    x1   0.5472651  0.08913391  0.1998368 0.65085961  0.9824011
    x2   1.1057602 -0.28773744  0.6674774 0.98240111  2.1120337
    
    $logl
    [1] -12.26019
    
    $diag
                     df1 df2 statistic   p-value
    Weak instruments   1   5 0.7427762 0.4281620
    Wu-Hausman         1   4 1.2108346 0.3329367
    Sargan             0  NA        NA        NA
    
    $residuals
             1          2          3          4          5          6          7 
    -1.2029545 -0.1955912  0.0000000 -0.3464919 -0.2374782  0.5756838  1.4068320 
             8          9         10 
    -0.4917639 -0.8639945  1.3557584 
    
    $std.residuals
             1          2          3          4          5          6          7 
    -1.0316164 -0.1677330  0.0000000 -0.2971407 -0.2036539  0.4936885  1.2064554 
             8          9         10 
    -0.4217214 -0.7409348  1.1626562 
    

# ivDiag() works correctly

                     statistic df   p-value
    Weak instruments 0.4772754 NA 0.6421648
    attr(,"is.reject.bad")
    [1] FALSE

---

           statistic df   p-value
    Sargan 0.9811175  1 0.3219231
    attr(,"is.reject.bad")
    [1] TRUE

---

                     statistic df   p-value
    Weak instruments 0.4772754 NA 0.6421648
    Sargan           0.9811175  1 0.3219231
    attr(,"is.reject.bad")
    [1] FALSE  TRUE

---

                          statistic df   p-value
    Weak instruments (x2) 0.4772754 NA 0.6421648
    Weak instruments (x3) 0.1236715 NA 0.8858653
    attr(,"is.reject.bad")
    [1] FALSE FALSE
