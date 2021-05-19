###dcpo_rubin

library(tidyverse)

set.seed(313)

#####analysis for rubin's rule
cntrl_ajps_uncertainty <- readRDS("data/cntrl_ajps_uncertainty.rds")
temp_df <- cntrl_ajps_uncertainty %>%
  filter(iter_se == 1) %>%
  select(country, year, Vdem_regime,muslism_prop_2010,dependence_pc_di,
         lg_imp_mdpgdp,mdprgdp_grwth) %>%
  mutate(regime = Vdem_regime > 1)

libdem_list_temp <- readRDS("data/libdem_list.rda")

load("data/theta_sigma_list.rda")

list_temp <- theta_list %>%
  map(~.x %>%
        left_join(temp_df, by=c("country","year")) 
  )

dcpo_ajps_rubin <- list()
dcpo_ajps_rubin <- purrr::map(1:900, function(anEntry) {
  dcpo_ajps_rubin[[anEntry]] <- list_temp[[anEntry]] %>%
    left_join(libdem_list_temp[[anEntry]],by = c("year", "country"))
  return(dcpo_ajps_rubin)
}) 

save(dcpo_ajps_rubin, file = "data/dcpo_ajps_rubin.rda")



