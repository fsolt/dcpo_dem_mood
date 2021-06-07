if (!require(pacman)) install.packages("pacman")
library(pacman)

# load all the packages you will use below 
p_load(
    here,
    purrr,
    countrycode,
    tidyverse
) 

# Functions preload
set.seed(313)


################################################################
######## Create lists for elements in analysis data  ###########
########### AJPS: cls_libdem_ajps_list,cls_ajps  ###############
################################################################
load(here("data","cls_ajps_cntrl.rda"))  #cls ajps variables without errors. 
load(here("data","cls_theta_list.rda"))  #cls's theta list
load(here("data","cls_libdem_list.rda"))  #vdem8 libdem list
load(here("data","country_regionUN.rda"))

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

saveRDS(libdem_ajps_list, file = here("data","cls_libdem_ajps_list.rds"))
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
               theta_dem_trim = ifelse(is.na(SupDem_trim),NA,
                                  ifelse(Regime_VD > 1 & !is.na(SupDem_trim), theta, 0)),
               theta_aut_trim = ifelse(is.na(SupDem_trim),NA,
                                  ifelse(Regime_VD < 2& !is.na(SupDem_trim), theta, 0))) %>%
        select(country, year, theta, contains("trim"), everything())
}) 

#merge with cls_theta and create trim variables. 

save(cls_ajps, file = here("data","cls_ajps.rda"))

################################################################
######## Create lists for elements in analysis data  ###########
########### APSR: cls_libdem_ajps_list,cls_ajps  ###############
################################################################
#load(here("data","cls_ajps_cntrl.rda"))
#load(here("data","cls_theta_list.rda"))
#load(here("data","cls_libdem_list.rda"))
load(here("data","cls_apsr_cntrl.rda"))
load(here("data","cls_poly_list.rda"))
load(here("data","cls_liberal_list.rda"))
load(here("data","cpi_list.rda"))

## Standardize cls_libdem_list Libdem_z
cls_libdem_z <- cls_libdem_list %>%
    map(~.x %>%
            mutate(Libdem_z = as.vector(scale(Vdem_libdem)) 
            ))

## Standardize cls_poly_list  Polyarchy_z
cls_polyarchy_z <- cls_poly_list %>%
    map(~.x %>%
            mutate(Polyarchy_z = as.vector(scale(poly_post))) %>%
            select(-poly_post)
    )

## Standardize cls_liberal_list Liberal_z
cls_lib_z <- cls_liberal_list %>%
    map(~.x %>%
            mutate(Liberal_z = as.vector(scale(liberal_post))) %>%
            select(-liberal_post)
    )

## corruption_z Corrup_TI_z
cpi_list_z <- cpi_list %>%
    map(~.x %>%
            rename(Corrup_TI_z = cpi_post_z) %>%
            select(-cpi_post)
    )


libdem_apsr_list <- cls_libdem_z %>%
    map(~.x %>%
            group_by(country) %>% 
            arrange(year, .by_group = TRUE) %>% 
            mutate(Libdem_m1 = lag(Libdem_z)) %>%
            ungroup() %>%
            mutate(ChgDem = Libdem_z - Libdem_m1) %>%
            ungroup() 
    ) 


# then merge with poly, liberal, and  cls_apsr_cntrl variables. 
cls_apsr_cntrl_list <-  purrr::map(1:900, function(anEntry) {
    libdem_apsr_list[[anEntry]] %>% 
        left_join(cls_apsr_cntrl,by = c("year"="Year", "country")) %>%
        left_join(cls_polyarchy_z[[anEntry]],by = c( "country","year", "liter" ="piter")) %>%
        left_join(cls_lib_z[[anEntry]],by = c("year", "country","liter" ="iter")) %>%
        left_join(cpi_list_z[[anEntry]],by = c("year", "country","liter"= "iter_cn"))
}) 


# merge with theta variable and produce trim data. 
cls_apsr <- purrr::map(1:900, function(anEntry) {
    cls_theta_list[[anEntry]] %>% 
        left_join(cls_apsr_cntrl_list[[anEntry]],by = c("year", "country"))  %>% 
        mutate(SupDem_trim = ifelse(year< First_yr, NA, theta)) %>%
        select(-Vdem_libdem,-liter,-iter_se) %>%
        select(country, year, First_yr,theta, SupDem_trim,contains("z"),everything())
}) 

save(cls_apsr, file = here("data","cls_apsr.rda"))