
###Posterior distribution: codes from R/v10_posterior.R
library(tidyverse)
library(dplyr)
library(tidyr)

set.seed(313)

cntrl_ajps_se <- read.csv("data/cntrl_ajps_se_1985.csv",encoding = "UTF-8",stringsAsFactors  = F)


##libdem
pdist <- cntrl_ajps_se %>%
  select(Vdem_libdem,libdem_sd) %>%
  pmap(function(Vdem_libdem,libdem_sd,...) rnorm(900, Vdem_libdem, libdem_sd))  #posterior distribution 900 draws

pdist_df <- pdist %>% 
  map_df(as_tibble)

pdist_df$iter_se <- rep(1:900, length.out=nrow(pdist_df)) 
pdist_df <- data.frame(pdist_df)

cntrl_ajps_se_pdist <- cntrl_ajps_se %>% group_by(country, year) %>% 
  expand(iter_cn = 1:900)  %>%
  group_by(country,year) %>% 
  arrange(iter_cn,.by_group = TRUE)  #arrange by country year iter

cntrl_ajps_se_pdist <- data.frame(cntrl_ajps_se_pdist)

cntrl_ajps_libdem <- cbind(cntrl_ajps_se_pdist, pdist_df)
identical(cntrl_ajps_libdem$iter_cn,cntrl_ajps_libdem$iter_se)

cntrl_ajps_libdem <- cntrl_ajps_libdem %>%
  rename(Vdem_libdem_post = value)

#saveRDS(cntrl_ajps_libdem, "data/cntrl_ajps_libdem.rds")

###polyarchi
pdist_poly <- cntrl_ajps_se %>%
  select(Vdem_polyarchy,polyarchy_sd) %>%
  pmap(function(Vdem_polyarchy,polyarchy_sd,...) rnorm(900, Vdem_polyarchy,polyarchy_sd))  #posterior distribution 900 draws

pdist_poly_df <- pdist_poly %>% 
  map_df(as_tibble)

pdist_poly_df$iter_se <- rep(1:900, length.out=nrow(pdist_poly_df)) 

pdist_poly_df <- data.frame(pdist_poly_df)

cntrl_ajps_poly_postse <- cbind(cntrl_ajps_se_pdist, pdist_poly_df)
identical(cntrl_ajps_poly_postse$iter_cn,cntrl_ajps_poly_postse$iter_se)

cntrl_ajps_poly_postse <- cntrl_ajps_poly_postse %>%
  rename(Vdem_polyarchy_post = value)

#saveRDS(cntrl_ajps_poly_postse, "data/cntrl_ajps_polypost.rds")

cntrl_ajps_error <- left_join(cntrl_ajps_libdem,cntrl_ajps_poly_postse, by = c("country","year","iter_cn","iter_se"))

cntrl_ajps_temp <- left_join(cntrl_ajps_error,cntrl_ajps_se, by = c("country", "year")) %>%
  select(country, year, starts_with("Vdem"),everything())


cntrl_ajps_uncertainty <- cntrl_ajps_temp %>%
  group_by(country, iter_se) %>%
  arrange(year, .by_group = TRUE) %>% 
  mutate(Vdem_libdem_postm1 = lag(Vdem_libdem_post),
         Vdem_libdem_postm2 = lag(Vdem_libdem_post,2)) %>%
  ungroup() %>%
  mutate(Chgdem_post = Vdem_libdem_post - Vdem_libdem_postm1,
         upchgdem_post = ifelse(Chgdem_post > 0, Chgdem_post,0),
         downchgdem_post = ifelse(Chgdem_post < 0, Chgdem_post*(-1),0)) %>%
  ungroup() %>%
  group_by(Region_UN,year,iter_cn) %>%
  mutate(Region_libdem_post = mean(Vdem_libdem_post, na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(country,iter_cn) %>%
  arrange(year, .by_group = TRUE) %>% 
  mutate(Region_libdem_postm1 = lag(Region_libdem_post))  %>%
  ungroup() %>%
  group_by(Region_UN,year, iter_cn)  %>%
  select(Region_UN, year, country, iter_se, Region_libdem_post,Region_libdem_postm1, starts_with("Vdem"),everything())

saveRDS(cntrl_ajps_uncertainty, "data/cntrl_ajps_uncertainty.rds")


#####data structure for rubin's rule
load("data/theta_sigma_list.rda") #theta and sigma

temp_df <- cntrl_ajps_uncertainty %>%
  select(country, year, Vdem_regime,muslism_prop_2010,dependence_pc_di,
         lg_imp_mdpgdp,mdprgdp_grwth,iter_se) %>%
  filter(iter_se == 1) %>%
  mutate(regime = Vdem_regime > 1)

data_temp <- cntrl_ajps_libdem %>% 
  group_by(iter_cn,country) %>%
  arrange(year,.by_group = TRUE) 

libdem_list <- list()
for (i in 1:900) {
  libdem_list[[i]] <- data_temp %>% 
    filter(iter_cn ==  i)
}  #loop runs so slow, and there should be more efficient way to do it

libdem_cntrl_list <- libdem_list %>%
  map(~.x %>%
        rename(Vdem_libdem = Vdem_libdem_post) %>%
        select(-iter_se) %>%
        left_join(temp_df,by=c("country","year")) %>%
        mutate(Vdem_libdem_m1 = lag(Vdem_libdem),
               Vdem_libdem_m2 = lag(Vdem_libdem,2)) %>%
        mutate(Chgdem = Vdem_libdem - Vdem_libdem_m1,
               upchgdem = ifelse(Chgdem > 0, Chgdem,0),
               downchgdem = ifelse(Chgdem < 0, Chgdem*(-1),0)) %>%
        group_by(Region_UN,year) %>%
        mutate(Region_libdem = mean(Vdem_libdem, na.rm=TRUE)) %>%
        ungroup() %>%
        group_by(country) %>%
        mutate(Region_libdem_m1 = lag(Region_libdem)) %>%
        ungroup() %>%
        select(-iter_cn.x,-iter_cn.y)
  ) 

#save(libdem_cntrl_list, file = "C:/Users/skywa/Dropbox/claassen/uncertainty_data/libdem_cntrl_list.rda")

list_temp <- theta_list %>%
  map(~.x %>%
        left_join(temp_df, by=c("country","year"))  %>%
        select(-iter_cn, -iter_se)
  )

dcpo_ajps_rubin <- purrr::map(1:900, function(anEntry) {
  list_temp[[anEntry]] %>% 
    left_join(libdem_cntrl_list[[anEntry]],by = c("year", "country","Region_UN")) 
}) 

save(dcpo_ajps_rubin, file = "data/dcpo_ajps_rubin.rda")

