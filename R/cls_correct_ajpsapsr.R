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

#osf_retrieve_file("kg7z4") %>%
#     osf_download(here::here("data"))  ##pure corrected

osf_retrieve_file("uvn4g") %>%
    osf_download(here::here("data"))  ##corrected


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


################################################################
######## Create lists for elements in analysis data  ###########
########### AJPS: cls_libdem_ajps_list,correct_cls_ajps ########
################################################################

load(here("data","cls_libdem_list.rda"))  #vdem8 libdem list
load(here("data","country_regionUN.rda"))
libdem_ajps_list <- readRDS(here::here("data","cls_libdem_ajps_list.rds"))  
#created by cls_libdem_list,country_regionUN
load(here("data","cls_ajps_cntrl.rda"))  #cls ajps variables without errors.
load(here("data","correct_cls_theta_list.rda"))
######process to creat libdem_ajps_list
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
#######include vdem8 libdem related variables such Libdem, Region_libdem, ChDem
cls_ajps_cntrl_list <-  purrr::map(1:900, function(anEntry) {
    libdem_ajps_list[[anEntry]] %>% 
        left_join(cls_ajps_cntrl,by = c("year", "country")) 
}) 
#create full control variables for cls ajps
correct_cls_ajps <- purrr::map(1:900, function(anEntry) {
    correct_cls_theta_list[[anEntry]] %>% 
        left_join(cls_ajps_cntrl_list[[anEntry]],by = c("year", "country"))  %>% 
        mutate(SupDem_trim = ifelse(year<firstyear, NA, theta),
               theta_dem_trim = ifelse(is.na(SupDem_trim),NA,
                                       ifelse(Regime_VD > 1 & !is.na(SupDem_trim), theta, 0)),
               theta_aut_trim = ifelse(is.na(SupDem_trim),NA,
                                       ifelse(Regime_VD < 2& !is.na(SupDem_trim), theta, 0))) %>%
        select(country, year, theta, contains("trim"), everything())
}) 
#merge with correct_cls_theta and create trim variables. 
save(correct_cls_ajps, file = here("data","correct_cls_ajps.rda"))


################################################################
######## Create lists for elements in analysis data  ###########
########### APSR: cls_libdem_apsr_list,correct_cls_apsr  #######
################################################################

load(here("data","cls_libdem_list.rda"))  #vdem8 libdem list
load(here("data","cls_apsr_cntrl.rda"))
load(here("data","cls_poly_list.rda"))
load(here("data","cls_liberal_list.rda"))
load(here("data","cpi_list.rda"))

cls_apsr_cntrl_list <- readRDS(here::here("data","cls_apsr_cntrl_list.rds"))

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
saveRDS(cls_apsr_cntrl_list, file = here("data","cls_apsr_cntrl_list.rds"))
# merge with theta variable and produce trim data. 
correct_cls_apsr <- purrr::map(1:900, function(anEntry) {
    correct_cls_theta_list[[anEntry]] %>% 
        left_join(cls_apsr_cntrl_list[[anEntry]],by = c("year", "country"))  %>% 
        mutate(SupDem_trim = ifelse(year< First_yr, NA, theta)) %>%
        select(-Vdem_libdem,-liter,-iter_se) %>%
        select(country, year, First_yr,theta, SupDem_trim,contains("z"),everything())
}) 
save(correct_cls_apsr, file = here("data","correct_cls_apsr.rda"))
###Upload to osf
osf_retrieve_user("me") %>%
    osf_ls_nodes()

demsup <- osf_retrieve_node("tnp2a")

osf_upload(demsup, c(here("data","correct_cls_ajps.rda")),conflicts = "overwrite")
osf_upload(demsup, c(here("data","correct_cls_apsr.rda")),conflicts = "overwrite")


################################################################
###############Expanded_Claassen and Analysis Data##############
###############Input:  exp_claassen_m5_3k_07-23-10-50.rda ######
##############Output: expcor_cls_theta_list ####################
################################################################

load(here("data", "exp_claassen_m5_3k_07-23-10-50.rda")) 
load(here("data","exp_claassen_input.rda"))
load(here("data","dcpo_ajps_cntrl.rda"))
load(here("data","dcpo_apsr_cntrl.rda"))
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


################################################################################
######## Create lists for elements in analysis data  ###########################
########### dcpo_ajps_cntrl,libdem_cntrl_list,expcor_cls_theta_list ############
########### AJPS:expcor_cls_ajps ###############################################
################################################################################

load(here("data","dcpo_ajps_cntrl.rda"))  ##
load(here("data","libdem_cntrl_list.rda")) ## created in dcpo_ajps_apsr_un.R 
#note: need to clean all R. files again later. 
cls_theta_cntrl <- expcor_cls_theta_list %>%
    map(~.x %>%
            left_join(dcpo_ajps_cntrl, by=c("country","year")) 
    )
expcor_cls_ajps <- purrr::map(1:900, function(anEntry) {
    cls_theta_cntrl[[anEntry]] %>% 
        left_join(libdem_cntrl_list[[anEntry]],by = c("year", "country","Region_UN"))  %>% 
        select(-iter_cn,-iter_se) %>% 
        mutate(theta_dem = theta * regime,
               theta_aut = theta * (1 - regime),
               theta_trim = ifelse(year < first_year, NA, theta)) %>% 
        mutate(theta_dem_trim = ifelse(is.na(theta_trim),NA,theta_dem),
               theta_aut_trim = ifelse(is.na(theta_trim),NA,theta_aut))  %>%
        select(country, year, theta, contains("trim"), everything())
}) 
expcor_cls_ajps <- purrr::map(1:900, function(anEntry) {
    expcor_cls_ajps[[anEntry]] %>% 
        rename(SupDem_trim = theta_trim,
               Regime_VD = Vdem_regime,
               Pr_Muslim = muslism_prop_2010,
               Res_cp_WDI_di = dependence_pc_di,
               lnGDP_imp = lg_imp_mdpgdp,
               GDP_imp_grth = mdprgdp_grwth,
               Regime_VD = Vdem_regime,
               firstyear = first_year,
               Libdem_VD = Vdem_libdem,
               Libdem_m1 = Vdem_libdem_m1,
               Libdem_m2 = Vdem_libdem_m2,
               ChgDem = Chgdem,
               UpChgDem = upchgdem,
               DwnChgDem = downchgdem,
               Libdem_regUN = Region_libdem,
               Libdem_regUN_m1 = Region_libdem_m1,) 
})  ##adjust names to be consistent with correct_cls_ajps
save(expcor_cls_ajps, file = here::here("data","expcor_cls_ajps.rda"))


################################################################
######## Create lists for elements in analysis data  ###########
########### APSR: expcor_cls_apsr  #############################
################################################################

load(here("data","libdem_list.rda"))  ##created from dcpo_ajps_apsr_uncertainty
load(here("data","poly_list.rda"))
load(here("data","lib_list.rda"))
load(here("data","cpi_list.rda"))
load(here("data","dcpo_apsr_cntrl.rda"))
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
exp_apsr_vdemcpi <- purrr::map(1:900, function(anEntry) {
    libdem_list_z[[anEntry]] %>% 
        left_join(polyarchy_list_z[[anEntry]],by = c("year", "country","iter_cn","iter_se")) %>% 
        left_join(lib_list_z[[anEntry]],by = c("year", "country","iter_cn","iter_se")) %>% 
        left_join(cpi_list_z[[anEntry]],by = c("year", "country","iter_cn","iter_se")) %>%
        select(-Vdem_libdem, -Vdem_polyarchy,-Vdem_liberal,-iter_se)
}) 
theta_cntrl_list <- expcor_cls_theta_list %>%
    map(~.x %>%
            left_join(dcpo_apsr_cntrl, by=c("country","year"))  %>%
            select(-iter_cn,-contains('lag1')) 
    )
expcor_cls_apsr <- purrr::map(1:900, function(anEntry) {
    theta_cntrl_list[[anEntry]] %>% 
        left_join(exp_apsr_vdemcpi[[anEntry]],by = c("year", "country")) %>%
        select(-iter_cn) %>%
        mutate(theta_trim = ifelse(year < first_year, NA, theta)) %>%
        select(country, year, first_year,theta, theta_trim, contains("z"),everything())
}) 
colnames(correct_cls_apsr[[1]])
colnames(expcor_cls_apsr[[1]])
expcor_cls_apsr <- purrr::map(1:900, function(anEntry) {
    expcor_cls_apsr[[anEntry]] %>% 
        rename( First_yr = first_year,
                SupDem_trim = theta_trim,
                Libdem_z = Vdem_libdem_z,
                Polyarchy_z = Vdem_polyarchy_z,
                Liberal_z = Vdem_liberal_z,
                Corrup_TI_z = corruption_z,
                lnGDP_imp = lg_imp_mdpgdp,
                Educ_yrs_UN = edu_year) 
})  ##adjust names to be consistent with correct_cls_apsr
save(expcor_cls_apsr, file = here::here("data","expcor_cls_apsr.rda"))
##upload to osf
osf_retrieve_user("me") %>%
    osf_ls_nodes()
demsup <- osf_retrieve_node("tnp2a")
osf_upload(demsup, c(here("data","expcor_cls_apsr.rda")),conflicts = "overwrite")
osf_upload(demsup, c(here("data","expcor_cls_ajps.rda")),conflicts = "overwrite")

####num.of observation
cor_cls_ajps_trim <- correct_cls_ajps[[1]] %>%
    group_by(country) %>% 
    arrange(year, .by_group = TRUE) %>% 
    mutate(SupDem_m1 = lag(SupDem_trim))

cor_cls_ajps_trim %>%
    filter(!is.na(Libdem_VD) & !is.na(SupDem_m1)) %>%
    nrow()
#2435
length(unique(cor_cls_ajps_trim$country))
#137
cor_cls_ajps_trim %>%
    filter(!is.na(Libdem_VD) & !is.na(SupDem_m1)) %>%
    distinct(country) %>%
    nrow()
#135

expcor_cls_ajps_trim <- expcor_cls_ajps[[1]] %>%
    group_by(country) %>% 
    arrange(year, .by_group = TRUE) %>% 
    mutate(SupDem_m1 = lag(SupDem_trim))

expcor_cls_ajps_trim %>%
    filter(!is.na(Libdem_VD) & !is.na(SupDem_m1)) %>%
    nrow()
#2787
length(unique(expcor_cls_ajps_trim$country))
#145

expcor_cls_ajps_trim %>%
    filter(!is.na(Libdem_VD) & !is.na(SupDem_m1)) %>%
    distinct(country) %>%
    nrow()
#140