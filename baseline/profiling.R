load("./data/artificial2sls_contaminated.rda")
fml <- y ~ -1+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11 | -1+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+z11+z12
base <- ivreg(formula = fml, data = artificial2sls_contaminated)
isat(base, iis = TRUE, t.pval = 1/100, print.searchinfo = FALSE)



profvis(isat(base, iis = TRUE, t.pval = 1/100, print.searchinfo = FALSE))
