## code to prepare `artificial2sls_contaminated` dataset goes here

load(file = "./data/artificial2sls.rda")
df <- artificial2sls

set.seed(14)
# randomly decide location of outliers
outliers <- sample(1:100, size = 5)
outliers <- outliers[order(outliers)]
# magnitude of outliers is 3 in absolute, randomly determine whether pos or neg
out <- sample(c(-3,3), size = length(outliers), replace = TRUE)
df[outliers, "y"] <- df[outliers, "y"] - df[outliers, "u"] + out
df[outliers, "u"] <- df[outliers, "u"] - df[outliers, "u"] + out

# store which observations are outliers and their magnitude as attributes
attr(df, "outliers") <- outliers
attr(df, "magnitude") <- out

artificial2sls_contaminated <- df

usethis::use_data(artificial2sls_contaminated, overwrite = TRUE)
