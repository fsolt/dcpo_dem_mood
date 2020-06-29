library(rstan)
library(tidyverse)
library(beepr)

source("R/claassen_xvt.R")

rstan_options(auto_write = TRUE)

download.file("https://github.com/fsolt/dcpo_dem_mood/raw/master/data/claassen_input.rda", "data/claassen_input.rda")

load("data/claassen_input.rda")

claassen_demsup_kfold <- purrr::map(1:10, function(x) {
  claassen_xvt(claassen_input,
               fold_number = 10, # number_of_folds = 10 is the default,
               iter = 500)
})

xvt_claassen_kfold <-
  get_claassen_xvt_results(claassen_demsup_kfold)

save(
  list = ls() %>% str_subset("^(?!demsup)"),
  file = file.path("data", "kfold", "claassen_demsup_kfold.rda")
)