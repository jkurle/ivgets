# compare estimation speed of ivreg.fit versus 2sls.fit

set.seed(1234)
y <- matrix(rnorm(1000), ncol = 1)
x <- matrix(rnorm(1000*10), nrow = 1000, ncol = 10)
z2 <- matrix(rnorm(1000), ncol = 1)
colnames(y) <- "y"
colnames(x) <- paste0("x", 1:10)
colnames(z2) <- "z"
z <- cbind(x[, -1], z2)

library(microbenchmark)
library(ivreg)
library(ivgets)

out1 <- ivreg.fit(x = x, y = y, z = z)
out2 <- twosls.fit(x = x, y = y, z = z)
identical(out1$coefficients, out2$coefficients)
identical(out1$cov.unscaled, out2$cov.unscaled)

f1 <- function(y, x, z) {
  ivreg.fit(x = x, y = y, z = z)
}

f2 <- function(y, x, z) {
  twosls.fit(x = x, y = y, z = z)
}

comparison <- microbenchmark(f1(x=x,y=y,z=z), f2(x=x,y=y,z=z), times = 5000)
# f1 mean is 4000ms, f2 mean is 1800ms -> so my implementation could be twice as fast
comparison2 <- microbenchmark(f1(x=x,y=y,z=z), f2(x=x,y=y,z=z), times = 10000)

# check what the return values are of the ivreg function and what is required for my continued calculations

# what does ivreg::ivreg return?
data <- as.data.frame(cbind(y, x, z2))
model1 <- ivreg(formula = y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10|x2+x3+x4+x5+x6+x7+x8+x9+x10+z, data = data)
model2 <- twosls(formula = y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10|x2+x3+x4+x5+x6+x7+x8+x9+x10+z, data = data)
identical(coef(model1), coef(model2))
identical(vcov(model1), vcov(model2))

slow <- ivisat(formula = y~-1+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10|-1+x2+x3+x4+x5+x6+x7+x8+x9+x10+z,
               data = data[1:100, ], iis = TRUE, sis = FALSE, tis = FALSE)
fast <- ivisat(formula = y~-1+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10|-1+x2+x3+x4+x5+x6+x7+x8+x9+x10+z,
               data = data[1:100, ], iis = TRUE, sis = FALSE, tis = FALSE, fast = TRUE)
library(waldo)
compare(slow, fast, max_diffs = 20)
# only differences in attributes or the fast argument

microbenchmark(ivisat(formula = y~-1+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10|-1+x2+x3+x4+x5+x6+x7+x8+x9+x10+z,
                      data = data[1:100, ], iis = TRUE, sis = FALSE, tis = FALSE, print.searchinfo = FALSE),
               ivisat(formula = y~-1+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10|-1+x2+x3+x4+x5+x6+x7+x8+x9+x10+z,
                      data = data[1:100, ], iis = TRUE, sis = FALSE, tis = FALSE, print.searchinfo = FALSE, turbo = TRUE),
               ivisat(formula = y~-1+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10|-1+x2+x3+x4+x5+x6+x7+x8+x9+x10+z,
                      data = data[1:100, ], iis = TRUE, sis = FALSE, tis = FALSE, print.searchinfo = FALSE, fast = TRUE),
               ivisat(formula = y~-1+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10|-1+x2+x3+x4+x5+x6+x7+x8+x9+x10+z,
                      data = data[1:100, ], iis = TRUE, sis = FALSE, tis = FALSE, print.searchinfo = FALSE, turbo = TRUE, fast = TRUE),
               times = 10)
# old data
# baseline 47.4
# baseline + turbo 29.4
# fast 37.7
# fast + turbo 23.2
# new data (two runs, separated by ;)
microbenchmark(ivisat(formula = y~-1+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10|-1+x2+x3+x4+x5+x6+x7+x8+x9+x10+z,
                      data = data[1:100, ], iis = TRUE, sis = FALSE, tis = FALSE, print.searchinfo = FALSE),
               ivisat(formula = y~-1+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10|-1+x2+x3+x4+x5+x6+x7+x8+x9+x10+z,
                      data = data[1:100, ], iis = TRUE, sis = FALSE, tis = FALSE, print.searchinfo = FALSE, fast = TRUE),
               times = 10)
# baseline 53; 48
# fast 41.7; 36

profvis::profvis(ivisat(formula = y~-1+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10|-1+x2+x3+x4+x5+x6+x7+x8+x9+x10+z,
                        data = data[1:100, ], iis = TRUE, sis = FALSE, tis = FALSE, print.searchinfo = FALSE))
# 11700 in ivreg::ivreg
profvis::profvis(ivisat(formula = y~-1+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10|-1+x2+x3+x4+x5+x6+x7+x8+x9+x10+z,
                        data = data[1:100, ], iis = TRUE, sis = FALSE, tis = FALSE, print.searchinfo = FALSE, fast = TRUE))
# 7360 in ivgets::twosls




# compare ivreg, twosls, and twosls.alt in detail
set.seed(1234)
y <- matrix(rnorm(1000), ncol = 1)
x <- matrix(rnorm(1000*10), nrow = 1000, ncol = 10)
z2 <- matrix(rnorm(1000), ncol = 1)
colnames(y) <- "y"
colnames(x) <- paste0("x", 1:10)
colnames(z2) <- "z"
z <- cbind(x[, -1], z2)

library(microbenchmark)
library(ivreg)
library(ivgets)
data <- as.data.frame(cbind(y, x, z2))
formula <- y~-1+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10|-1+x2+x3+x4+x5+x6+x7+x8+x9+x10+z

microbenchmark(ivreg(formula = formula, data = data),
               twosls(formula = formula, data = data),
               twosls.alt(formula = formula, data = data),
               times = 10000)
# ivreg: 11.39 milliseconds
# twosls: 6.72 milliseconds
# twosls.alt: 7.12 milliseconds
# -> no further potential to speed estimation up

# compare 100 vs 500 obs
set.seed(1234)
y <- matrix(rnorm(1000), ncol = 1)
x <- matrix(rnorm(1000*10), nrow = 1000, ncol = 10)
z2 <- matrix(rnorm(1000), ncol = 1)
colnames(y) <- "y"
colnames(x) <- paste0("x", 1:10)
colnames(z2) <- "z"
z <- cbind(x[, -1], z2)

library(microbenchmark)
library(ivreg)
library(ivgets)
data <- as.data.frame(cbind(y, x, z2))
formula <- y~-1+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10|-1+x2+x3+x4+x5+x6+x7+x8+x9+x10+z

microbenchmark(ivisat(formula = formula, data = data[1:100, ], t.pval = 0.01, iis = TRUE, turbo = TRUE, fast = TRUE),
               ivisat(formula = formula, data = data[1:500, ], t.pval = 0.01, iis = TRUE, turbo = TRUE, fast = TRUE),
               times = 5)
# mean 19s
# mean 96s
microbenchmark(ivisat(formula = formula, data = data[1:500, ], t.pval = 0.01, iis = TRUE, turbo = TRUE, fast = TRUE, blocks = 100),
               times = 5)


ivisat(formula = formula, data = data[1:100, ], t.pval = 0.01, iis = TRUE, turbo = TRUE, fast = TRUE, blocks = 5)
ivisat(formula = formula, data = data[1:500, ], t.pval = 0.01, iis = TRUE, turbo = TRUE, fast = TRUE, blocks = 100, print.searchinfo = FALSE)


microbenchmark(ivisat(formula = formula, data = data[1:500, ], t.pval = 0.01, iis = TRUE, turbo = TRUE, fast = TRUE, blocks = 100, print.searchinfo = FALSE),
               ivisat(formula = formula, data = data[1:500, ], t.pval = 0.01, iis = TRUE, turbo = TRUE, fast = TRUE, print.searchinfo = FALSE),
               times = 3)
# with more blocks takes longer, 278s instead of only 116s




