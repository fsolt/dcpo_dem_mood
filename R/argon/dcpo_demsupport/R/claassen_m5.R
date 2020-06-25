library(tidyverse)
library(rstan)

download.file("https://github.com/fsolt/dcpo_dem_mood/raw/master/data/claassen_input.Rda", "data/claassen_input.Rda")

load("data/claassen_input.Rda")

claassen_m5 <- stan(file = 'R/supdem.stan.mod5.stan',
                    data = claassen_input,
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
                      str_replace("20\\d{2}-", "") %>%
                      str_replace("-\\d{2}$", ""),
                  ".rda"))