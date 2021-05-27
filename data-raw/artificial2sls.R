## code to prepare `artificial2sls` dataset goes here

# requires r2sls but is not released yet; so is part of .Rbuildignore

p <- r2sls::generate_param(10, 1, 2, beta = c(6, -5, 0, 0, 0, 0, 0, 0, 0, 0, 3),
                    sigma = 1)
d <- r2sls::generate_data(p, 100)
df <- d$data
cor(df$u, df$r11) # quick check that have endogeneity
cor(df$z11, df$u) # quick check that instrument is approx. exogenous

df[, c(14:23)] <- NULL
df[, c(16:26)] <- NULL
artificial2sls <- df

usethis::use_data(artificial2sls, overwrite = TRUE)
