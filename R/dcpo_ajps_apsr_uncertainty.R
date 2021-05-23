###Posterior distribution: codes from R/v10_posterior.R
library(tidyverse)
library(dplyr)
library(tidyr)
library(purrr)

set.seed(313)

################################################################
######## Create lists for elements in analysis data  ###########
########### theta_sigma_list,lib_list.rda, cpi_list  ###########
################    libdem_list,  poly_list    #################
####################### libdem_cntrl_list     ##################


load("data/dcpo_input.rda")
load("data/dcpo_theta.rda")
load("data/dcpo_sigma.rda")
load("data/dcpo_ajps_cntrl.rda")
load("data/dcpo_apsr_cntrl.rda")
cntrl_ajps_se <- readRDS("data/cntrl_ajps_se_1985.rds")
cpi_post <- readRDS("data/cpi_post.rds")


############Theta_sigma_list
ls_country <- levels(dcpo_input$data$kk)
first_year <- min(dcpo_input$data$year)

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

theta_sigma_list <- purrr::map(1:900, function(anEntry) {
  dcpo_theta$theta[anEntry,,] %>%
    reformat_dcpo_output("theta") %>%
    left_join(reformat_dcpo_output(dcpo_sigma$sigma[anEntry,,], "sigma"),
              by = c("year", "country"))
}) 

#save(theta_sigma_list,file = "data/theta_sigma_list.rda")


########libdem_list
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

data_temp <- cntrl_ajps_libdem %>% 
  group_by(iter_cn,country) %>%
  arrange(year,.by_group = TRUE) 

libdem_list <- list()
for (i in 1:900) {
  libdem_list[[i]] <- data_temp %>% 
    filter(iter_cn ==  i)
}

#save(libdem_list, file = "data/libdem_list.rda")


#########libdem_cntrl_list

libdem_cntrl_list <- libdem_list %>%
  map(~.x %>%
        rename(Vdem_libdem = Vdem_libdem_post) %>%
        select(-iter_se) %>%
        left_join(dcpo_ajps_cntrl,by=c("country","year")) %>%
        group_by(country) %>%
        mutate(Vdem_libdem_m1 = dplyr::lag(Vdem_libdem),
               Vdem_libdem_m2 = dplyr::lag(Vdem_libdem,2)) %>%
        ungroup() %>%
        mutate(Chgdem = Vdem_libdem - Vdem_libdem_m1,
               upchgdem = ifelse(Chgdem > 0, Chgdem,0),
               downchgdem = ifelse(Chgdem < 0, Chgdem*(-1),0)) %>%
        ungroup() %>%
        group_by(Region_UN,year) %>%
        mutate(Region_libdem = mean(Vdem_libdem, na.rm=TRUE)) %>%
        ungroup() %>%
        group_by(country) %>%
        mutate(Region_libdem_m1 = lag(Region_libdem)) %>%
        ungroup() %>%
        select(-iter_cn.x,-iter_cn.y,-iter_se)
  ) 

#save(libdem_cntrl_list, file = "data/libdem_cntrl_list.rda")


##############polyarchi_list
pdist_poly <- cntrl_ajps_se %>%
  select(Vdem_polyarchy,polyarchy_sd) %>%
  pmap(function(Vdem_polyarchy,polyarchy_sd,...) rnorm(900, Vdem_polyarchy,polyarchy_sd))  #posterior distribution 900 draws

pdist_poly_df <- pdist_poly %>% 
  map_df(as_tibble)

pdist_poly_df$iter_se <- rep(1:900, length.out=nrow(pdist_poly_df)) 

pdist_poly_df <- data.frame(pdist_poly_df)

cntrl_poly <- cbind(cntrl_ajps_se_pdist, pdist_poly_df)
identical(cntrl_poly$iter_cn,cntrl_poly$iter_se)

cntrl_poly <- cntrl_poly %>%
  rename(Vdem_polyarchy = value)

#saveRDS(cntrl_poly, "data/cntrl_poly.rds")

data_temp <- cntrl_poly %>% 
  group_by(iter_cn,country) %>%
  arrange(year,.by_group = TRUE) 

poly_list <- list()
for (i in 1:900) {
  poly_list[[i]] <- data_temp %>% 
    filter(iter_cn == i)
}

#save(poly_list, file = "data/poly_list.rda")


############liberal_ist
libpdist <- cntrl_ajps_se %>%
  select(Vdem_liberal,liberal_sd) %>%
  pmap(function(Vdem_liberal,liberal_sd,...) rnorm(900, Vdem_liberal, liberal_sd))  #posterior distribution 900 draws

libpdist_df <- libpdist %>% 
  map_df(as_tibble)

libpdist_df$iter_se <- rep(1:900, length.out=nrow(libpdist_df)) 
libpdist_df <- data.frame(libpdist_df)

cntrl_liberal <- cbind(cntrl_ajps_se_pdist, libpdist_df)
identical(cntrl_liberal$iter_cn,cntrl_liberal$iter_se)
cntrl_liberal <- cntrl_liberal %>%
  rename(Vdem_liberal = value)
#saveRDS(cntrl_liberal, "data/cntrl_liberal.rds")

data_temp <- cntrl_liberal %>% 
  group_by(iter_cn,country) %>%
  arrange(year,.by_group = TRUE) 

lib_list <- list()
for (i in 1:900) {
  lib_list[[i]] <- data_temp %>% 
    filter(iter_cn == i)
}

#save(lib_list, file = "data/lib_list.rda")

############Cpi_list

cpi_post_z <- cpi_post %>%
  select(country, year,iter_cn,iter_se,cpi_post,cpi_post_z)

data_temp <- cpi_post_z %>% 
  group_by(iter_cn,country) %>%
  arrange(year,.by_group = TRUE) 

cpi_list <- list()
for (i in 1:900) {
  cpi_list[[i]] <- data_temp %>% 
    filter(iter_cn ==  i)
}

#save(cpi_list, file = "data/cpi_list.rda")


#####################################################
#####     Create analysis data for dcpo ajps  #######
#####################################################
#load("data/libdem_cntrl_list.rda")
#load("data/theta_sigma_list.rda") #theta and sigma

dcpo_ajps <- purrr::map(1:900, function(anEntry) {
  theta_sigma_list[[anEntry]] %>% 
    left_join(libdem_cntrl_list[[anEntry]],by = c("year", "country"))  
}) 

#save(dcpo_ajps, file = "data/dcpo_ajps.rda")

#####################################################
#####     Create analysis data for dcpo apsr  #######
#####################################################

#load("data/libdem_list.rda")
#load("data/poly_list.rda")
#load("data/lib_list.rda")
#load("data/cpi_list.rda")

## Standardize Vdem_libdem Vdem_libdem_z
libdem_list_z <- libdem_list %>%
  map(~.x %>%
        rename(Vdem_libdem = Vdem_libdem_post) %>%
        mutate(Vdem_libdem_z = as.vector(scale(Vdem_libdem)) 
        ))

## Standardize Vdem_polyarchy_z  Vdem_liberal_z
polyarchy_list_z <- poly_list %>%
  map(~.x %>%
        mutate(Vdem_polyarchy_z = as.vector(scale(Vdem_polyarchy))
        ))

## Standardize Vdem_liberal_z
lib_list_z <- lib_list %>%
  map(~.x %>%
        mutate(Vdem_liberal_z = as.vector(scale(Vdem_liberal))
        ))

## corruption_z
cpi_list_z <- cpi_list %>%
  map(~.x %>%
        rename(corruption_z = cpi_post_z) %>%
        select(-cpi_post)
  )

dcpo_apsr_vdemcpi <- purrr::map(1:900, function(anEntry) {
  libdem_list_z[[anEntry]] %>% 
    left_join(polyarchy_list_z[[anEntry]],by = c("year", "country","iter_cn","iter_se")) %>% 
    left_join(lib_list_z[[anEntry]],by = c("year", "country","iter_cn","iter_se")) %>% 
    left_join(cpi_list_z[[anEntry]],by = c("year", "country","iter_cn","iter_se"))
}) 


theta_cntrl_list <- theta_sigma_list %>%
  map(~.x %>%
        left_join(dcpo_apsr_cntrl, by=c("country","year"))  %>%
        select(-iter_cn,-contains('lag1')) 
  )

dcpo_apsr <- purrr::map(1:900, function(anEntry) {
  theta_cntrl_list[[anEntry]] %>% 
    left_join(dcpo_apsr_vdemcpi[[anEntry]],by = c("year", "country")) %>%
    select(-iter_cn,-iter_se)
}) 

#save(dcpo_apsr, file = "data/dcpo_apsr.rda")








