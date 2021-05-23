library(tidyverse)
library(dplyr)
library(purrr)
library(countrycode)
set.seed(313)

################################################################
######## Create lists for elements in analysis data  ###########
################################################################
load("data/cls_ajps_cntrl.rda")  #cls ajps variables without errors. 
load("data/cls_theta_list.rda")  #cls's theta list
load("data/cls_libdem_list.rda")  #vdem8 libdem list
load("data/country_regionUN.rda") 

libdem_ajps_list <- cls_libdem_list %>%
    map(~.x %>%
            mutate(Libdem_VD = Vdem_libdem*100) %>%    ###negative values, after 100, so big. 
            left_join(cntry_regionUN, by=c("country")) %>% 
            group_by(country) %>% 
            arrange(year, .by_group = TRUE) %>% 
            mutate(Libdem_m1 = lag(Libdem_VD),
                   Libdem_m2 = lag(Libdem_VD,2)) %>%
            ungroup() %>%
            mutate(ChgDem = Libdem_VD - Libdem_m1,
                   UpChgDem = ifelse(ChgDem > 0, ChgDem,0),
                   DwnChgDem = ifelse(ChgDem < 0, ChgDem*(-1),0)) %>%
            ungroup() %>%
            group_by(Region_UN,year) %>%
            mutate(Libdem_regUN = mean(Libdem_VD, na.rm=TRUE)) %>%
            ungroup() %>%
            group_by(country) %>%
            arrange(year, .by_group = TRUE) %>% 
            mutate(Libdem_regUN_m1 = lag(Libdem_regUN))
    ) 

saveRDS(libdem_ajps_list, "data/cls_libdem_ajps_list.rds")
#include vdem8 libdem related variables such Libdem, Region_libdem, ChDem

cls_ajps_cntrl_list <-  purrr::map(1:900, function(anEntry) {
    libdem_ajps_list[[anEntry]] %>% 
        left_join(cls_ajps_cntrl,by = c("year", "country")) 
}) 
#create full control variables for cls ajps

cls_ajps <- purrr::map(1:900, function(anEntry) {
    cls_theta_list[[anEntry]] %>% 
        left_join(cls_ajps_cntrl_list[[anEntry]],by = c("year", "country"))  %>% 
        mutate(SupDem_trim = ifelse(year<firstyear, NA, theta),
               theta_dem = ifelse(is.na(SupDem_trim),NA,
                                  ifelse(Regime_VD > 1 & !is.na(SupDem_trim), theta, 0)),
               theta_aut = ifelse(is.na(SupDem_trim),NA,
                                  ifelse(Regime_VD < 2& !is.na(SupDem_trim), theta, 0)))
}) 
#merge with cls_theta and create trim variables. 

save(cls_ajps, file = "data/cls_ajps.rda")