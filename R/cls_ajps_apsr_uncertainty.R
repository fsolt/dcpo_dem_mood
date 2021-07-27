if (!require(pacman)) install.packages("pacman")
library(pacman)

# load all the packages you will use below 
p_load(
    here,
    purrr,
    countrycode,
    tidyverse,
    rstan,
    osfr
) 

osf_retrieve_file("kg7z4") %>%
     osf_download(here::here("data"))  ##pure corrected


# Functions preload
set.seed(313)


reformat_dcpo_output <- function(x, parameter_name) {
    df_temp <- x %>% 
        as_tibble(.name_repair = ~ls_country) %>% 
        mutate(year = first_year + row_number() - 1) %>% 
        pivot_longer(cols = all_of(ls_country),
                     names_to = "country",
                     values_to = parameter_name) %>%
        arrange(country, year)
    return(df_temp)
}


#load(here::here("data", "exp_claassen_m5_3k_06-13-17-09.rda")) 

#load(here("data", "pure_claassen_m5.rda")) 

load(here("data", "claassen_m5_3k_07-05-11-16.rda")) 

#claassen_m5_theta <- rstan::extract(pure_claassen_m5, pars = "theta")

claassen_m5_theta <- rstan::extract(claassen_m5, pars = "theta")  ##derived from claassen_m5_3k_07-05-11-16
##however, theta has 136 country instead of 137. 

#cnt.names = as.character(sort(unique(sddem2$Country)))

supdem <- read_csv(here::here("data", "supdem raw survey marginals.tab"), col_types = "cdcddcdc")
pure_claassen_input <- supdem %>%                                             # 1390 obs
    janitor::clean_names() %>% 
    mutate(old_country = country,
           country = countrycode::countrycode(old_country, "country.name", "country.name")) %>%
    DCPOtools::with_min_yrs(2) %>% 
    dplyr::select(country, year, project) %>% 
    unique()

length(unique(pure_claassen_input$country))

ls_year <- 1988:2017
ls_country <- pure_claassen_input$country %>% unique()
first_year <- min(pure_claassen_input$year)

pure_cls_theta_list <- purrr::map(1:900, function(anEntry) {
    claassen_m5_theta$theta[anEntry,,] %>%
        reformat_dcpo_output("theta") 
}
)
#year 30, country 137
save(pure_cls_theta_list,file = here("data","pure_cls_theta_list.rda"))

################################################################
######## Create lists for elements in analysis data  ###########
########### AJPS: cls_libdem_ajps_list,pure_cls_ajps ############
################################################################

load(here("data","cls_ajps_cntrl.rda"))  #cls ajps variables without errors. 
#load(here("data","cls_theta_list.rda"))  #cls's theta list
load(here("data","pure_cls_theta_list.rda"))
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

pure_cls_ajps <- purrr::map(1:900, function(anEntry) {
    pure_cls_theta_list[[anEntry]] %>% 
        left_join(cls_ajps_cntrl_list[[anEntry]],by = c("year", "country"))  %>% 
        mutate(SupDem_trim = ifelse(year<firstyear, NA, theta),
               theta_dem_trim = ifelse(is.na(SupDem_trim),NA,
                                       ifelse(Regime_VD > 1 & !is.na(SupDem_trim), theta, 0)),
               theta_aut_trim = ifelse(is.na(SupDem_trim),NA,
                                       ifelse(Regime_VD < 2& !is.na(SupDem_trim), theta, 0))) %>%
        select(country, year, theta, contains("trim"), everything())
}) 

#merge with cls_theta and create trim variables. 


save(pure_cls_ajps, file = here("data","pure_cls_ajps.rda"))

################################################################
######## Create lists for elements in analysis data  ###########
########### APSR: cls_libdem_apsr_list,pure_cls_apsr  ###########
################################################################

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
pure_cls_apsr <- purrr::map(1:900, function(anEntry) {
    pure_cls_theta_list[[anEntry]] %>% 
        left_join(cls_apsr_cntrl_list[[anEntry]],by = c("year", "country"))  %>% 
        mutate(SupDem_trim = ifelse(year< First_yr, NA, theta)) %>%
        select(-Vdem_libdem,-liter,-iter_se) %>%
        select(country, year, First_yr,theta, SupDem_trim,contains("z"),everything())
}) 


save(pure_cls_apsr, file = here("data","pure_cls_apsr.rda"))



###############Expanded_Claassen and Analysis Data##################
###############Input:  exp_claassen_m5 ############################
##############Output: exp_cls_theta_list ############################
exp_claassen_m5_theta <- rstan::extract(exp_claassen_m5, pars = "theta")

ls_year <- 1988:2017
ls_country <- expanded_claassen_input$data$country %>% unique()
first_year <- min(expanded_claassen_input$data$year)


exp_cls_theta_list <- purrr::map(1:900, function(anEntry) {
    exp_claassen_m5_theta$theta[anEntry,,] %>%
        reformat_dcpo_output("theta") 
}
)
#year 33, country 145
save(exp_cls_theta_list,file = here("data","exp_cls_theta_list.rda"))


################################################################
######## Create lists for elements in analysis data  ###########
########### AJPS: cls_libdem_ajps_list,exp_cls_ajps ############
################################################################

load(here("data","cls_ajps_cntrl.rda"))  #cls ajps variables without errors. 
#load(here("data","cls_theta_list.rda"))  #cls's theta list
load(here("data","exp_cls_theta_list.rda"))
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

exp_cls_ajps <- purrr::map(1:900, function(anEntry) {
    exp_cls_theta_list[[anEntry]] %>% 
        left_join(cls_ajps_cntrl_list[[anEntry]],by = c("year", "country"))  %>% 
        mutate(SupDem_trim = ifelse(year<firstyear, NA, theta),
               theta_dem_trim = ifelse(is.na(SupDem_trim),NA,
                                  ifelse(Regime_VD > 1 & !is.na(SupDem_trim), theta, 0)),
               theta_aut_trim = ifelse(is.na(SupDem_trim),NA,
                                  ifelse(Regime_VD < 2& !is.na(SupDem_trim), theta, 0))) %>%
        select(country, year, theta, contains("trim"), everything())
}) 

#merge with cls_theta and create trim variables. 


save(exp_cls_ajps, file = here("data","exp_cls_ajps.rda"))

################################################################
######## Create lists for elements in analysis data  ###########
########### APSR: cls_libdem_apsr_list,exp_cls_apsr  ###########
################################################################

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
exp_cls_apsr <- purrr::map(1:900, function(anEntry) {
    exp_cls_theta_list[[anEntry]] %>% 
        left_join(cls_apsr_cntrl_list[[anEntry]],by = c("year", "country"))  %>% 
        mutate(SupDem_trim = ifelse(year< First_yr, NA, theta)) %>%
        select(-Vdem_libdem,-liter,-iter_se) %>%
        select(country, year, First_yr,theta, SupDem_trim,contains("z"),everything())
}) 


save(exp_cls_apsr, file = here("data","exp_cls_apsr.rda"))
osf_retrieve_user("me") %>%
    osf_ls_nodes()

demsup <- osf_retrieve_node("tnp2a")

osf_upload(demsup, c(here("data","exp_cls_apsr.rda")))
osf_upload(demsup, c(here("data","pure_cls_apsr.rda")))

osf_upload(demsup, c(here("data","exp_cls_ajps.rda")))
osf_upload(demsup, c(here("data","pure_cls_ajps.rda")))
