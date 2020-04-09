remotes::install_github("fsolt/DCPOtools")
remotes::install_github("stan-dev/rstantools")
remotes::install_github("fsolt/DCPO")

library(tidyverse)
library(DCPOtools)
library(rstan)

demsup <- read_csv("https://github.com/fsolt/DCPOtools/raw/master/inst/extdata/all_data_demsupport.csv")

demsup_claassen <- format_claassen(demsup)

claassen_m5 <- stan(file = 'R/supdem.stan.mod5.stan',
                       data = demsup_claassen,
                       iter = 2000,
                       chains= 4,
                       cores = 4,
		       thin = 2,
                       pars = c("mu_lambda","sigma_lambda","sigma_delta","sigma_theta","phi","lambda","delta","theta",
            "x_pred","log_lik"),
                       control = list(adapt_delta=0.99, stepsize=0.02, max_treedepth=11))

save(demsup_claassen, claassen_m5, 
     file = str_c("data/claassen_m5_2k_", 
                  str_replace_all(Sys.time(), "[ :]", "-") %>%
                    str_replace("2019-", "") %>%
                    str_replace("-\\d{2}$", ""),
                  ".rda"))