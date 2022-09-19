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
# baseline 47.4
# baseline + turbo 29.4
# fast 37.7
# fast + turbo 23.2

profvis::profvis(ivisat(formula = y~-1+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10|-1+x2+x3+x4+x5+x6+x7+x8+x9+x10+z,
                        data = data[1:100, ], iis = TRUE, sis = FALSE, tis = FALSE, print.searchinfo = FALSE))
# 11700 in ivreg::ivreg
profvis::profvis(ivisat(formula = y~-1+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10|-1+x2+x3+x4+x5+x6+x7+x8+x9+x10+z,
                        data = data[1:100, ], iis = TRUE, sis = FALSE, tis = FALSE, print.searchinfo = FALSE, fast = TRUE))
# 7360 in ivgets::twosls
profvis::profvis(twosls.fit(x = x, y = y, z = z)) # no result because mostly in C
