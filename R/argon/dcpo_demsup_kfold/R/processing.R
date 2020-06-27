install.packages("DCPO", repos = "https://cloud.r-project.org/")

library(tidyverse)
library(DCPO)

fold <- commandArgs(trailingOnly=TRUE) %>%
    as.numeric()

print(fold)

download.file("https://github.com/fsolt/dcpo_dem_mood/raw/master/data/dcpo_replication_input.rda", "data/dcpo_replication_input.rda")

load("data/dcpo_replication_input.rda")


dcpo_kfold <- dcpo_xvt(dcpo_replication_input,
                       fold_number = fold, 
                       number_of_folds = 10,
                       iter = 2000)

save(dcpo_kfold, 
     file = str_c("data/dcpo_fold_",
                  fold,
                  ".rda"))
