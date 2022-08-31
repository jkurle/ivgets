library(robust2sls)
set.seed(10)
p <- generate_param(1, 1, 1, beta = c(2, 4), sigma = 1,
                    mean_z = 0, cov_z = matrix(1),
                    Sigma2_half = matrix(1), Omega2 = matrix(3/4),
                    Pi = t(matrix(c(1, 0, 0, 1), nrow = 2)))
d <- generate_data(parameters = p, n = 50)$data
rownames_orig <- rownames(d)
rownames(d) <- as.character(1:NROW(d))
formula <- y ~ -1+x1+x2 | -1+x1+z2
complete <- nonmissing(data = d, formula = formula)
gamma <- 0.05
y_var <- "y"

### complete data
iismodel1 <- ivgets::ivisat(formula = formula, data = d, iis = TRUE,
                            sis = FALSE, tis = FALSE, uis = FALSE,
                            blocks = NULL, ratio.threshold = 0.8,
                            max.block.size = 30, t.pval = gamma,
                            wald.pval = gamma, do.pet = FALSE,
                            ar.LjungB = NULL, arch.LjungB = NULL,
                            normality.JarqueB = NULL,
                            info.method = "sc", include.1cut = FALSE,
                            include.empty = FALSE, max.paths = NULL,
                            parallel.options = NULL, turbo = FALSE,
                            tol = 1e-07, max.regs = NULL,
                            print.searchinfo = FALSE, plot = NULL,
                            alarm = FALSE, overid = NULL, weak = NULL)
