library(githubinstall)
library(devtools)
#gh_install_packages("jkurle/gets")
#gh_list_packages(username = "gsucarrat")
remove.packages("gets")
#install_github("jkurle/gets", ref = "2sls-v2", subdir = "gets")
install_github("jkurle/gets", ref = "2sls-v3", subdir = "gets")

remove.packages("ivgets")
install_github("jkurle/ivgets")




deployApp("C:/Users/jonas/OneDrive - OnTheHub - The University of Oxford/Documents Sync/PhD/DPhil_R/ivgets/baseline")
