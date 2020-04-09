remotes::install_github("fsolt/DCPOtools")
remotes::install_github("fsolt/DCPO")

library(tidyverse)
library(DCPOtools)
library(DCPO)

demsup <- read_csv("https://github.com/fsolt/DCPOtools/raw/master/inst/extdata/all_data_demsupport.csv")

demsup_data <- format_dcpo(demsup %>% with_min_yrs(2),
                       scale_q = "church_21",
                       scale_cp = 2)

out1 <- dcpo(demsup_data,
                 iter = 2000,
		 chains = 4,
		 thin = 2,
                 pars = c("sd_delta","sd_theta_evolve", "sd_sigma_evolve", "sigma","phi","beta","alpha","delta","theta","y_r_pred","log_lik"))

save(demsup_data, out1,
     file = str_c("data/church_21_2k_",
                  str_replace_all(Sys.time(), "[ :]", "-") %>%
                    str_replace("2019-", "") %>%
                    str_replace("-\\d{2}$", ""),
                  ".rda"))
