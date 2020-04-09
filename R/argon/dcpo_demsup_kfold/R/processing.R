remotes::install_github("fsolt/DCPOtools")
remotes::install_github("fsolt/DCPO")

library(tidyverse)
library(DCPOtools)
library(DCPO)

fold <- commandArgs(trailingOnly=TRUE) %>%
  as.numeric()

demsup <- read_csv(system.file("extdata", "all_data_demsupport.csv", package = "DCPOtools"))

print(fold)

demsup_data <- format_dcpo(demsup %>% with_min_yrs(3),
                       scale_q = "church_21",
                       scale_cp = 2)

demsup_kfold <- dcpo_xvt(demsup_data,
                          fold_number = fold, 
			  number_of_folds = 10,
                          iter = 500)

save(demsup_kfold, 
     file = str_c("data/fold_",
		  fold,
                  ".rda"))
