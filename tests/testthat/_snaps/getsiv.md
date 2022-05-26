# ivgets() produces correct output

    $selection
    $selection$call
    a <- gets::getsFun(y = setup$y, x = setup$x, untransformed.residuals = NULL,
                           user.estimator = userest, gum.result = gum.result,
                           t.pval = t.pval, wald.pval = wald.pval, do.pet = do.pet,
                           ar.LjungB = ar.LjungB, arch.LjungB = arch.LjungB,
                           normality.JarqueB = normality.JarqueB,
                           user.diagnostics = NULL,
                           gof.function = list(name = "infocrit"),
                           gof.method = "min", keep = setup$keep,
                           include.gum = include.gum, include.1cut = include.1cut,
                           include.empty = include.empty, max.paths = max.paths,
                           turbo = turbo, tol = tol, LAPACK = FALSE,
                           max.regs = max.regs, print.searchinfo = print.searchinfo,
                           alarm = alarm)
    
    $selection$no.of.estimations
    [1] 17
    
    $selection$paths
    $selection$paths[[1]]
    [1] 3 4 5 6
    
    $selection$paths[[2]]
    [1] 4 3 5 6
    
    $selection$paths[[3]]
    [1] 5 4 3 6
    
    $selection$paths[[4]]
    [1] 6 3 4 5
    
    
    $selection$terminals
    $selection$terminals[[1]]
    [1] 1 2 7
    
    
    $selection$terminals.results
            info(sc)      logl  n k
    spec 1: 2.896968 -66.55617 50 3
    
    $selection$best.terminal
    [1] 1
    
    $selection$specific.spec
    cons   x1   x6 
       1    2    7 
    
    
    $final
    
    Call:
    ivreg::ivreg(formula = as.formula(fml_sel), data = d)
    
    Coefficients:
      cons      x1      x6  
     1.052   1.790  -1.058  
    
    
    $keep
    [1] "x6"
    
    attr(,"class")
    [1] "ivgets"

---

    $selection
    $selection$call
    a <- gets::getsFun(y = setup$y, x = setup$x, untransformed.residuals = NULL,
                           user.estimator = userest, gum.result = gum.result,
                           t.pval = t.pval, wald.pval = wald.pval, do.pet = do.pet,
                           ar.LjungB = ar.LjungB, arch.LjungB = arch.LjungB,
                           normality.JarqueB = normality.JarqueB,
                           user.diagnostics = NULL,
                           gof.function = list(name = "infocrit"),
                           gof.method = "min", keep = setup$keep,
                           include.gum = include.gum, include.1cut = include.1cut,
                           include.empty = include.empty, max.paths = max.paths,
                           turbo = turbo, tol = tol, LAPACK = FALSE,
                           max.regs = max.regs, print.searchinfo = print.searchinfo,
                           alarm = alarm)
    
    $selection$no.of.estimations
    [1] 10
    
    $selection$paths
    $selection$paths[[1]]
    [1] 4 5 6
    
    $selection$paths[[2]]
    [1] 5 4 6
    
    $selection$paths[[3]]
    [1] 6 4 5
    
    
    $selection$terminals
    $selection$terminals[[1]]
    [1] 1 2 3 7
    
    
    $selection$terminals.results
            info(sc)      logl  n k
    spec 1: 2.975791 -66.57073 50 4
    
    $selection$best.terminal
    [1] 1
    
    $selection$specific.spec
    cons   x1   x2   x6 
       1    2    3    7 
    
    
    $final
    
    Call:
    ivreg::ivreg(formula = as.formula(fml_sel), data = d)
    
    Coefficients:
        cons        x1        x2        x6  
     1.04876   1.79059   0.01234  -1.05835  
    
    
    $keep
    [1] "cons" "x1"   "x2"   "x6"  
    
    attr(,"class")
    [1] "ivgets"

---

    $selection
    $selection$call
    a <- gets::getsFun(y = setup$y, x = setup$x, untransformed.residuals = NULL,
                           user.estimator = userest, gum.result = gum.result,
                           t.pval = t.pval, wald.pval = wald.pval, do.pet = do.pet,
                           ar.LjungB = ar.LjungB, arch.LjungB = arch.LjungB,
                           normality.JarqueB = normality.JarqueB,
                           user.diagnostics = userdia,
                           gof.function = list(name = "infocrit"),
                           gof.method = "min", keep = setup$keep,
                           include.gum = include.gum, include.1cut = include.1cut,
                           include.empty = include.empty, max.paths = max.paths,
                           turbo = turbo, tol = tol, LAPACK = FALSE,
                           max.regs = max.regs, print.searchinfo = print.searchinfo,
                           alarm = alarm)
    
    $selection$no.of.estimations
    [1] 17
    
    $selection$paths
    $selection$paths[[1]]
    [1] 3 4 5 6
    
    $selection$paths[[2]]
    [1] 4 3 5 6
    
    $selection$paths[[3]]
    [1] 5 4 3 6
    
    $selection$paths[[4]]
    [1] 6 3 4 5
    
    
    $selection$terminals
    $selection$terminals[[1]]
    [1] 1 2 7
    
    
    $selection$terminals.results
            info(sc)      logl  n k
    spec 1: 2.896968 -66.55617 50 3
    
    $selection$best.terminal
    [1] 1
    
    $selection$specific.spec
    cons   x1   x6 
       1    2    7 
    
    
    $final
    
    Call:
    ivreg::ivreg(formula = as.formula(fml_sel), data = d)
    
    Coefficients:
      cons      x1      x6  
     1.052   1.790  -1.058  
    
    
    $keep
    [1] "cons" "x1"   "x6"  
    
    attr(,"class")
    [1] "ivgets"

---

    $selection
    $selection$call
    a <- gets::getsFun(y = setup$y, x = setup$x, untransformed.residuals = NULL,
                           user.estimator = userest, gum.result = gum.result,
                           t.pval = t.pval, wald.pval = wald.pval, do.pet = do.pet,
                           ar.LjungB = ar.LjungB, arch.LjungB = arch.LjungB,
                           normality.JarqueB = normality.JarqueB,
                           user.diagnostics = userdia,
                           gof.function = list(name = "infocrit"),
                           gof.method = "min", keep = setup$keep,
                           include.gum = include.gum, include.1cut = include.1cut,
                           include.empty = include.empty, max.paths = max.paths,
                           turbo = turbo, tol = tol, LAPACK = FALSE,
                           max.regs = max.regs, print.searchinfo = print.searchinfo,
                           alarm = alarm)
    
    $selection$no.of.estimations
    [1] 17
    
    $selection$paths
    $selection$paths[[1]]
    [1] 3 4 5 6
    
    $selection$paths[[2]]
    [1] 4 3 5 6
    
    $selection$paths[[3]]
    [1] 5 4 3 6
    
    $selection$paths[[4]]
    [1] 6 3 4 5
    
    
    $selection$terminals
    $selection$terminals[[1]]
    [1] 1 2 7
    
    
    $selection$terminals.results
            info(sc)      logl  n k
    spec 1: 2.896968 -66.55617 50 3
    
    $selection$best.terminal
    [1] 1
    
    $selection$specific.spec
    cons   x1   x6 
       1    2    7 
    
    
    $final
    
    Call:
    ivreg::ivreg(formula = as.formula(fml_sel), data = d)
    
    Coefficients:
      cons      x1      x6  
     1.052   1.790  -1.058  
    
    
    $keep
    [1] "cons" "x1"   "x6"  
    
    attr(,"class")
    [1] "ivgets"

---

    $selection
    $selection$call
    a <- gets::getsFun(y = setup$y, x = setup$x, untransformed.residuals = NULL,
                           user.estimator = userest, gum.result = gum.result,
                           t.pval = t.pval, wald.pval = wald.pval, do.pet = do.pet,
                           ar.LjungB = ar.LjungB, arch.LjungB = arch.LjungB,
                           normality.JarqueB = normality.JarqueB,
                           user.diagnostics = NULL,
                           gof.function = list(name = "infocrit"),
                           gof.method = "min", keep = setup$keep,
                           include.gum = include.gum, include.1cut = include.1cut,
                           include.empty = include.empty, max.paths = max.paths,
                           turbo = turbo, tol = tol, LAPACK = FALSE,
                           max.regs = max.regs, print.searchinfo = print.searchinfo,
                           alarm = alarm)
    
    $selection$no.of.estimations
    [1] 14
    
    $selection$paths
    $selection$paths[[1]]
    [1] 3 4 5
    
    $selection$paths[[2]]
    [1] 4 3 5
    
    $selection$paths[[3]]
    [1] 5 4 3
    
    $selection$paths[[4]]
    [1] 6 3 4 5
    
    
    $selection$terminals
    $selection$terminals[[1]]
    [1] 1 2 6 7
    
    $selection$terminals[[2]]
    [1] 1 2 7
    
    
    $selection$terminals.results
            info(sc)      logl  n k
    spec 1: 2.786394 -61.83581 50 4
    spec 2: 2.896968 -66.55617 50 3
    
    $selection$best.terminal
    [1] 1
    
    $selection$specific.spec
    cons   x1   x5   x6 
       1    2    6    7 
    
    
    $final
    
    Call:
    ivreg::ivreg(formula = as.formula(fml_sel), data = d)
    
    Coefficients:
       cons       x1       x5       x6  
     1.0094   1.7794  -0.3159  -0.9927  
    
    
    $keep
    [1] "x6"
    
    attr(,"class")
    [1] "ivgets"

---

    $selection
    $selection$call
    a <- gets::getsFun(y = setup$y, x = setup$x, untransformed.residuals = NULL,
                           user.estimator = userest, gum.result = gum.result,
                           t.pval = t.pval, wald.pval = wald.pval, do.pet = do.pet,
                           ar.LjungB = ar.LjungB, arch.LjungB = arch.LjungB,
                           normality.JarqueB = normality.JarqueB,
                           user.diagnostics = NULL,
                           gof.function = list(name = "infocrit"),
                           gof.method = "min", keep = setup$keep,
                           include.gum = include.gum, include.1cut = include.1cut,
                           include.empty = include.empty, max.paths = max.paths,
                           turbo = turbo, tol = tol, LAPACK = FALSE,
                           max.regs = max.regs, print.searchinfo = print.searchinfo,
                           alarm = alarm)
    
    $selection$no.of.estimations
    [1] 17
    
    $selection$paths
    $selection$paths[[1]]
    [1] 3 4 5 6
    
    $selection$paths[[2]]
    [1] 4 3 5 6
    
    $selection$paths[[3]]
    [1] 5 4 3 6
    
    $selection$paths[[4]]
    [1] 6 3 4 5
    
    
    $selection$terminals
    $selection$terminals[[1]]
    [1] 1 2 7
    
    
    $selection$terminals.results
            info(sc)      logl  n k
    spec 1: 2.896968 -66.55617 50 3
    
    $selection$best.terminal
    [1] 1
    
    $selection$specific.spec
    cons   x1   x6 
       1    2    7 
    
    
    $final
    
    Call:
    ivreg::ivreg(formula = as.formula(fml_sel), data = d)
    
    Coefficients:
      cons      x1      x6  
     1.052   1.790  -1.058  
    
    
    $keep
    [1] "cons" "x1"   "x6"  
    
    attr(,"class")
    [1] "ivgets"

