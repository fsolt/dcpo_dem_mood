library(rstan)
library(tidyverse)
library(beepr)

source("R/claassen_xvt.R")

rstan_options(auto_write = TRUE)

demsup <-
    read_csv("data/claassen_replication.csv", col_types = "dcdcddcdd")

demsup_data <- DCPOtools::format_claassen(demsup %>%
                                              DCPOtools::with_min_yrs(3))


claassen_demsup_kfold <- purrr::map(1:10, function(x) {
    claassen_xvt(demsup_data,
                 fold_number = 10, # number_of_folds = 10 is the default，
                 iter = 500)
})

xvt_claassen_kfold <-
    get_claassen_xvt_results(claassen_demsup_kfold)

save(
    list = ls() %>% str_subset("^(?!demsup)"),
    file = file.path("data", "kfold", "claassen_demsup_kfold.rda")
)
