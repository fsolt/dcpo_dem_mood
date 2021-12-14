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

osf_retrieve_file("uvn4g") %>%
    osf_download(here::here("data"))  ##claassen_m5_3k_07-27-15-40.rda


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


####################################################
################Creating Correct Data ##############
####################################################


######Creating Correct_theta_list################

load(here("data", "claassen_m5_3k_07-27-15-40.rda")) 
load(here::here("data","claassen_replication_input.rda"))
claassen_m5_theta <- rstan::extract(claassen_m5, pars = "theta")  ##137*30 

claassen_replication <- claassen_replication_input$data %>%
    janitor::clean_names() %>% 
    DCPOtools::with_min_yrs(2) 

length(unique(claassen_replication$country)) ##137
min(claassen_replication$year) #1988
max(claassen_replication$year) #2017

ls_year <- 1988:2017
ls_country <- claassen_replication$country %>% unique()
first_year <- min(claassen_replication$year)
correct_cls_theta_list <- purrr::map(1:900, function(anEntry) {
    claassen_m5_theta$theta[anEntry,,] %>%
        reformat_dcpo_output("theta") 
}
)

#year 30, country 137
save(correct_cls_theta_list,file = here("data","correct_cls_theta_list.rda"))


######## Create Correct Analysis Data for Models in AJPS ###########
#load(here("data","correct_cls_theta_list.rda"))
load(here("data","cls_libdem_list.rda"))  
load(here("data","cls_cntrl.rda")) 
load(here("data","vdem8_regime.rda")) 
load(here("data","country_regionUN.rda"))

#merge variables
cls_ajps_cntrl_list <-  purrr::map(1:900, function(anEntry) {
    cls_libdem_list[[anEntry]] %>% 
        mutate(Libdem_VD = Vdem_libdem*100) %>%    ###negative values, after 100, so big. 
        left_join(country_regionUN, by=c("country")) %>% 
        group_by(country) %>% 
        mutate(Libdem_m1 = dplyr::lag(Libdem_VD,n =1, order_by = year),
               Libdem_m2 = dplyr::lag(Libdem_VD,n=2,order_by = year )) %>%
        ungroup() %>%
        mutate(ChgDem = Libdem_VD - Libdem_m1,
               UpChgDem = ifelse(ChgDem > 0, ChgDem,0),
               DwnChgDem = ifelse(ChgDem < 0, ChgDem*(-1),0)) %>%
        ungroup() %>%
        group_by(Region_UN,year) %>%
        mutate(Libdem_regUN = mean(Libdem_VD, na.rm=TRUE)) %>%
        ungroup() %>%
        group_by(country) %>%
        mutate(Libdem_regUN_m1 = dplyr::lag(Libdem_regUN,n =1, order_by = year )) %>%
        ungroup() %>%
        left_join(cls_cntrl[[anEntry]],by = c("year", "country", "Region_UN")) %>% 
        filter(year > 1986) %>% 
        filter(country %in% correct_cls_theta_list[[1]]$country)
}) 

#create trimmed variables. 
correct_cls_ajps <- purrr::map(1:900, function(anEntry) {
    cls_ajps_cntrl_list[[anEntry]] %>% 
        left_join(correct_cls_theta_list[[anEntry]],by = c("year", "country"))  %>% 
        left_join(vdem8_regime %>%  rename(Regime_VD = vdem_regime), by = c("country","year")) %>% 
        mutate(SupDem_trim = ifelse(year < firstyear, NA, theta),
               theta_dem_trim = ifelse(is.na(SupDem_trim),NA,
                                       ifelse(Regime_VD > 1 & !is.na(SupDem_trim), theta, 0)),
               theta_aut_trim = ifelse(is.na(SupDem_trim),NA,
                                       ifelse(Regime_VD < 2& !is.na(SupDem_trim), theta, 0))) %>%
        select(country, year, theta, contains("trim"), everything())
}) 

save(correct_cls_ajps, file = here("data","correct_cls_ajps.rda"))


######## Create Correct Analysis Data for Models in APSR ###########
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


# merge variables  
cls_apsr_cntrl_list <-  purrr::map(1:900, function(anEntry) {
    cls_libdem_z[[anEntry]] %>% 
        group_by(country) %>% 
        mutate(Libdem_m1 = dplyr::lag(Libdem_z, 1, order_by =  year)) %>%
        ungroup() %>%
        mutate(ChgDem = Libdem_z - Libdem_m1) %>%
        ungroup()  %>%
        left_join(cls_cntrl[[anEntry]],by = c("year", "country")) %>%
        left_join(cls_polyarchy_z[[anEntry]],by = c( "country","year", "liter" ="piter")) %>%
        left_join(cls_lib_z[[anEntry]],by = c("year", "country","liter" ="iter")) %>%
        left_join(cpi_list_z[[anEntry]],by = c("year", "country","liter"= "iter_cn")) %>% 
        filter(year > 1986) %>% 
        filter(country %in% correct_cls_theta_list[[1]]$country)
}) 

# create trimmed data
correct_cls_apsr <- purrr::map(1:900, function(anEntry) {
    cls_apsr_cntrl_list[[anEntry]] %>% 
        left_join(correct_cls_theta_list[[anEntry]],by = c("year", "country"))  %>% 
        mutate(SupDem_trim = ifelse(year< firstyear, NA, theta)) %>%
        select(-Vdem_libdem,-liter,-iter_se) %>%
        select(country, year, firstyear,theta, SupDem_trim,contains("z"),everything())
}) 
save(correct_cls_apsr, file = here("data","correct_cls_apsr.rda"))



####################################################
################Creating Expanded Correct Data ##############
####################################################


###########Creating Expanded Correct Theta_list##############
osf_retrieve_file("gk7nu") %>%
    osf_download(here::here("data"))  #exp_claassen_m5_3k_07-23-10-50.rda

load(here("data", "exp_claassen_m5_3k_07-23-10-50.rda")) 
load(here("data","exp_claassen_input.rda"))

exp_claassen_m5_theta <- rstan::extract(claassen_m5, pars = "theta")
ls_year <- 1988:2020
ls_country <- exp_claassen_input$data$country %>% unique() #145
first_year <- min(exp_claassen_input$data$year) #1988

expcor_cls_theta_list <- purrr::map(1:900, function(anEntry) {
    exp_claassen_m5_theta$theta[anEntry,,] %>%
        reformat_dcpo_output("theta") 
}
)
#year 33, country 145
save(expcor_cls_theta_list,file = here("data","expcor_cls_theta_list.rda"))


#######Create Expanded Correct Analysis Data for Models in AJPS ###########

#load(here("data","expcor_cls_theta_list.rda"))
load(here("data","libdem_list.rda"))
load(here("data","exp_cntrl.rda")) 
load(here("data","vdem10_regime.rda")) 
load(here("data","country_regionUN.rda"))

#merge variables  
exp_ajps_cntrl_list <-  purrr::map(1:900, function(anEntry) {
    libdem_list[[anEntry]] %>% 
        rename(Vdem_libdem = value) %>%
        mutate(Libdem_VD = Vdem_libdem*100) %>%    
        left_join(country_regionUN, by=c("country")) %>% 
        group_by(country) %>% 
        mutate(Libdem_m1 = dplyr::lag(Libdem_VD,n =1, order_by = year),
               Libdem_m2 = dplyr::lag(Libdem_VD,n=2,order_by = year )) %>%
        ungroup() %>%
        mutate(ChgDem = Libdem_VD - Libdem_m1,
               UpChgDem = ifelse(ChgDem > 0, ChgDem,0),
               DwnChgDem = ifelse(ChgDem < 0, ChgDem*(-1),0)) %>%
        ungroup() %>%
        dplyr::group_by(Region_UN,year) %>%
        mutate(Libdem_regUN = mean(Libdem_VD, na.rm=TRUE)) %>%
        ungroup() %>%
        dplyr::group_by(country) %>%
        mutate(Libdem_regUN_m1 = dplyr::lag(Libdem_regUN,n =1, order_by = year )) %>%
        ungroup() %>%
        left_join(exp_cntrl[[anEntry]],by = c("year", "country", "Region_UN")) %>% 
        filter(year > 1986) %>% 
        filter(country %in% expcor_cls_theta_list[[1]]$country)
}) 


#create trimmed variables. 
expcor_cls_ajps <- purrr::map(1:900, function(anEntry) {
    exp_ajps_cntrl_list[[anEntry]] %>% 
        left_join(expcor_cls_theta_list[[anEntry]],by = c("year", "country"))  %>% 
        left_join(vdem10_regime %>%  rename(Regime_VD = Vdem_regime), by = c("country","year")) %>%
        mutate(SupDem_trim = ifelse(year < firstyear, NA, theta),
               theta_dem_trim = ifelse(is.na(SupDem_trim),NA,
                                       ifelse(Regime_VD > 1 & !is.na(SupDem_trim), theta, 0)),
               theta_aut_trim = ifelse(is.na(SupDem_trim),NA,
                                       ifelse(Regime_VD < 2 & !is.na(SupDem_trim), theta, 0))) %>%
        select(country, year, theta, contains("trim"), everything())
})  
save(expcor_cls_ajps, file = here::here("data","expcor_cls_ajps.rda"))


######## Create Expanded Correct Analysis Data for Models in APSR  ###########

load(here("data","libdem_list.rda"))  
load(here("data","poly_list.rda"))
load(here("data","lib_list.rda"))
load(here("data","cpi_list.rda"))

## Standardize Vdem_libdem Vdem_libdem_z
libdem_list_z <- libdem_list %>%
    map(~.x %>%
            rename(Vdem_libdem = value) %>%
            mutate(Vdem_libdem_z = as.vector(scale(Vdem_libdem)) 
            ))
## Standardize Vdem_polyarchy_z  Vdem_liberal_z
polyarchy_list_z <- poly_list %>%
    map(~.x %>%
            rename(Vdem_polyarchy = Vdem_polyarchy_post) %>%
            mutate(Vdem_polyarchy_z = as.vector(scale(Vdem_polyarchy))
            ))
## Standardize Vdem_liberal_z
lib_list_z <- lib_list %>%
    map(~.x %>%
            rename(Vdem_liberal = Vdem_liberal_post) %>%
            mutate(Vdem_liberal_z = as.vector(scale(Vdem_liberal))
            ))
## corruption_z
cpi_list_z <- cpi_list %>%
    map(~.x %>%
            rename(corruption_z = cpi_post_z) %>%
            select(-cpi_post)
    )


# merge variables  
exp_apsr_cntrl_list <- purrr::map(1:900, function(anEntry) {
    libdem_list_z[[anEntry]] %>% 
        group_by(country) %>% 
        mutate(Libdem_m1 = dplyr::lag(Vdem_libdem_z, 1, order_by =  year)) %>%
        ungroup() %>%
        mutate(ChgDem = Vdem_libdem_z - Libdem_m1) %>%
        left_join(polyarchy_list_z[[anEntry]],by = c("year", "country","iter_cn","iter_se")) %>% 
        left_join(lib_list_z[[anEntry]],by = c("year", "country","iter_cn","iter_se")) %>% 
        left_join(cpi_list_z[[anEntry]],by = c("year", "country","iter_cn","iter_se")) %>%
        select(-Vdem_libdem, -Vdem_polyarchy,-Vdem_liberal,-iter_se) %>%
        left_join(exp_cntrl[[anEntry]],by = c("year", "country")) %>% 
        filter(year > 1986) %>% 
        filter(country %in% expcor_cls_theta_list[[1]]$country) %>%
        rename( Libdem_z = Vdem_libdem_z,
                Polyarchy_z = Vdem_polyarchy_z,
                Liberal_z = Vdem_liberal_z,
                Corrup_TI_z = corruption_z)  
}) 


# create trimmed data
expcor_cls_apsr <- purrr::map(1:900, function(anEntry) {
    exp_apsr_cntrl_list[[anEntry]] %>% 
        left_join(expcor_cls_theta_list[[anEntry]],by = c("year", "country"))  %>% 
        mutate(SupDem_trim = ifelse(year< firstyear, NA, theta)) %>%
        select(country, year, firstyear,theta, SupDem_trim,contains("z"),everything())
    
})  


save(expcor_cls_apsr, file = here::here("data","expcor_cls_apsr.rda"))

