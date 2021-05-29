## code to prepare `artificial2sls` dataset goes here

# requires r2sls but is not released yet; so is part of .Rbuildignore

load(artificial2sls)
artificial2sls$is.outlier <- factor(0, levels = c("0", "1"))


artificial2sls_shiny <- artificial2sls

usethis::use_data(artificial2sls_shiny, overwrite = TRUE)
