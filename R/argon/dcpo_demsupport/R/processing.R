install.packages("DCPO", repos = "https://cloud.r-project.org/")

library(tidyverse)
library(DCPO)

download.file("https://github.com/fsolt/dcpo_dem_mood/raw/master/data/dcpo_replication_input.rda", "data/dcpo_replication_input.rda")

load("data/dcpo_replication_input.rda")

dcpo_replication <- dcpo(dcpo_replication_input,
                         iter = 2000,
                         chains = 4,
                         thin = 2,
                         pars = c("sd_delta","sd_theta_evolve", "sd_sigma_evolve", "sigma","phi","beta","alpha","delta","theta","y_r_pred","log_lik"))

save(dcpo_replication_input, dcpo_replication,
     file = str_c("data/church_lb2_2k_",
                  str_replace_all(Sys.time(), "[ :]", "-") %>%
                      str_replace("20\\d{2}-", "") %>%
                      str_replace("-\\d{2}$", ""),
                  ".rda"))
