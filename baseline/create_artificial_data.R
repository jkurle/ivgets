library(r2sls)

p <- generate_param(10, 1, 2, beta = c(6, -5, 0, 0, 0, 0, 0, 0, 0, 0, 3), sigma = 1)
d <- generate_data(p, 100)
df <- d$data
cor(df$u, df$r11)
cor(df$z11, df$u)

df[, c(14:23)] <- NULL
df[, c(16:26)] <- NULL
artificial2sls <- df

use_data(artificial2sls)

# contaminate the sample
set.seed(14)
outliers <- sample(1:100, size = 5)
out <- sample(c(-3,3), size = length(outliers), replace = TRUE)
df[outliers, "y"] <- df[outliers, "y"] - df[outliers, "u"] + out
df[outliers, "u"] <- df[outliers, "u"] - df[outliers, "u"] + out

m <- ivreg::ivreg(y ~ -1+x1+x2+x11 | -1+x1+x2+z11+z12, data = df)
isat(m)

artificial2sls_contaminated <- df
use_data(artificial2sls_contaminated)

m <- ivreg::ivreg(y ~ -1+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11 | -1+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+z11+z12,
                  data = artificial2sls)
gets(m, t.pval = 1/100)

m <- ivreg::ivreg(y ~ -1+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11 | -1+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+z11+z12,
                  data = artificial2sls_contaminated)
gets(m, t.pval = 1/100)


