library(tidyverse)
if (!require("DCPO")) install.packages("DCPO", repos = "https://cloud.r-project.org/"); library(DCPO)


download.file("https://github.com/fsolt/dcpo_dem_mood/raw/master/data/dcpo_input_con.rda", "data/dcpo_input_con.rda")

load("data/dcpo_input_con.rda")

iter <- 3000

dcpo_output_con <- dcpo(dcpo_input_con,
                    iter = iter,
                    chains = 4,
                    thin = iter/500,
                    pars = c("sd_delta","sd_theta_evolve", "sd_sigma_evolve", "sigma","phi","beta","alpha","delta","theta","y_r_pred","log_lik"))

save(dcpo_input_con, dcpo_output_con,
     file = str_c("data/church4_", round(iter/1000), "k_",
                  str_replace_all(Sys.time(), "[ :]", "-") %>%
                      str_replace("20\\d{2}-", "") %>%
                      str_replace("-\\d{2}$", ""),
                  ".rda"))
