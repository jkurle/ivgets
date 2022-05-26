# code to prepare `artificial2sls` dataset

load("./data/artificial2sls.rda")
artificial2sls$is.outlier <- factor(0, levels = c("0", "1"))


artificial2sls_shiny <- artificial2sls

usethis::use_data(artificial2sls_shiny, overwrite = TRUE)
