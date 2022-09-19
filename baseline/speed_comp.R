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

twosls(formula = y~-1+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10|x2+x3+x4+x5+x6+x7+x8+x9+x10+z, data = data)




