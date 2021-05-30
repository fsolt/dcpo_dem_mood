###Posterior distribution: codes from R/v10_posterior.R
library(tidyverse)
library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(janitor)
library(countrycode)
library(stringr)
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
load("data/country_regionUN.rda")  ##this file can be used when we need region variables

#cpi_post <- readRDS("data/cpi_post.rds") created in R/cpi_error.R. 
# For the convenience, create cpi_post here too.  


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

save(theta_sigma_list,file = "data/theta_sigma_list.rda")


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

#saveRDS(cntrl_ajps_libdem, "data/cntrl_ajps_libdem.rds")

data_temp <- cntrl_ajps_libdem %>% 
  group_by(iter_cn,country) %>%
  arrange(year,.by_group = TRUE) 

libdem_list <- list()
for (i in 1:900) {
  libdem_list[[i]] <- data_temp %>% 
    filter(iter_cn ==  i)
}

save(libdem_list, file = "data/libdem_list.rda")
#load("data/libdem_list.rda")

#########libdem_cntrl_list

libdem_cntrl_list <- libdem_list %>%
  map(~.x %>%
        rename(Vdem_libdem = value) %>%
        select(-iter_se) %>%
        left_join(cntry_regionUN,by=c("country")) %>%
        group_by(country) %>%
        arrange(year, .by_group =TRUE) %>%
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
        ungroup()  %>%
        select(-iter_cn)
  ) 


save(libdem_cntrl_list, file = "data/libdem_cntrl_list.rda")


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
  rename(Vdem_polyarchy_post = value)

#saveRDS(cntrl_poly, "data/cntrl_poly.rds")

data_temp <- cntrl_poly %>% 
  group_by(iter_cn,country) %>%
  arrange(year,.by_group = TRUE) 

poly_list <- list()
for (i in 1:900) {
  poly_list[[i]] <- data_temp %>% 
    filter(iter_cn == i)
}

save(poly_list, file = "data/poly_list.rda")


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
  rename(Vdem_liberal_post = value)
#saveRDS(cntrl_liberal, "data/cntrl_liberal.rds")

data_temp <- cntrl_liberal %>% 
  group_by(iter_cn,country) %>%
  arrange(year,.by_group = TRUE) 

lib_list <- list()
for (i in 1:900) {
  lib_list[[i]] <- data_temp %>% 
    filter(iter_cn == i)
}

save(lib_list, file = "data/lib_list.rda")

############create cpi_post and Cpi_list
cpi_95_19_update <- readRDS("data/cpi95_19_update.rds")
cpi_2012error <- read_excel("data/CPI2019.xlsx", sheet ="CPI Timeseries 2012 - 2019")

cpi_2012error_clean <- cpi_2012error %>%
  row_to_names(row_number = 2) %>%
  select(-contains("Rank"),-contains("Source"),-Region, -contains("CPI"),-ISO3) %>%
  rename(country = Country) %>%
  gather("year", "cpi_error", 2:9) %>%
  mutate(year = str_replace_all(year, "[A-Za-z,]",""),
         year = as.numeric(year),
         cpi_error = as.numeric(cpi_error)) %>%
  mutate(ISO = countrycode(country, origin = 'country.name', destination = 'iso3c')) %>%
  mutate(country_name = countrycode::countrycode(ISO, "iso3c","country.name")) %>%
  mutate(country_name = ifelse(country == "Kosovo","Kosovo",
                               ifelse(country == "Yugoslavia","Yugoslavia",country_name))) %>%
  rename(country_text = country,
         country = country_name) %>%
  select(-country_text,-ISO)

cpi_95_19_error <- left_join(cpi_95_19_update,cpi_2012error_clean, by= c("country","year"))

cpi_95_19_error <- cpi_95_19_error %>%
  mutate(relative_error = cpi_error/cpi_100) %>%
  select(-cpi) %>%
  group_by(country) %>%
  mutate(h_re = max(relative_error,na.rm = T),
         h_re = ifelse(h_re =="-Inf",NA,h_re)) %>%
  ungroup() %>%
  mutate(cpi_error = ifelse(is.na(cpi_error), cpi_100*h_re,cpi_error)) %>%
  group_by(country) %>%
  mutate(cpi_100_m1 = lag(cpi_100)) %>%
  arrange(country,year)

cpi_pdist <- cpi_95_19_error %>%
  select(cpi_100,cpi_error) %>%
  pmap(function(cpi_100,cpi_error,...) rnorm(900, cpi_100,cpi_error))  #posterior distribution 900 draws

cpi_pdist_df <- cpi_pdist %>% 
  map_df(as_tibble)

cpi_pdist_df$iter_se <- rep(1:900, length.out=nrow(cpi_pdist_df)) 

cpi_95_19_error_900 <- cpi_95_19_error %>% group_by(country, year) %>% 
  expand(iter_cn = 1:900)  %>%
  group_by(country,year) %>% 
  arrange(iter_cn,.by_group = TRUE)  #arrange by country year iter

cpi_95_19_error_900 <- data.frame(cpi_95_19_error_900)
cpi_pdist_df <- data.frame(cpi_pdist_df)

cpi_se <- cbind(cpi_95_19_error_900, cpi_pdist_df)
cpi_se <- cpi_se %>%
  rename(cpi_post = value) %>%
  mutate(cpi_post = ifelse(is.nan(cpi_post),NA,cpi_post))

cpi <- left_join(cpi_se, cpi_95_19_error,by =c("country","year"))

cpi_post <- cpi %>%
  select(-h_re,-relative_error) %>%
  mutate(cpi_z = as.vector(scale(cpi_100*-1)), # reversed, positive--more corruption
         cpi_m1_z = as.vector(scale(cpi_100_m1*-1)), 
         cpi_post_z = as.vector(scale(cpi_post*-1))) %>%
  group_by(country, iter_se) %>%
  arrange(year, .by_group = TRUE) %>% 
  mutate(cpi_postm1 = lag(cpi_post),
         cpi_postm1_z = as.vector(scale(cpi_postm1)))

saveRDS(cpi_post, "data/cpi_post.rds")

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

save(cpi_list, file = "data/cpi_list.rda")


#####################################################
#####     Create analysis data for dcpo ajps  #######
#####################################################
#load("data/libdem_cntrl_list.rda")
load("data/theta_sigma_list.rda") #theta and sigma
load("data/dcpo_ajps_cntrl.rda")

theta_cntrl <- theta_sigma_list %>%
  map(~.x %>%
        left_join(dcpo_ajps_cntrl, by=c("country","year")) 
  )


dcpo_ajps <- purrr::map(1:900, function(anEntry) {
  theta_cntrl[[anEntry]] %>% 
    left_join(libdem_cntrl_list[[anEntry]],by = c("year", "country","Region_UN"))  %>% 
    select(-iter_cn,-iter_se) %>% 
    mutate(theta_dem = theta * regime,
           theta_aut = theta * (1 - regime),
           theta_trim = ifelse(year < first_year, NA, theta)) 
}) 

save(dcpo_ajps, file = "data/dcpo_ajps.rda")

#####################################################
#####     Create analysis data for dcpo apsr  #######
#####################################################

load("data/libdem_list.rda")
load("data/poly_list.rda")
load("data/lib_list.rda")
load("data/cpi_list.rda")
load("data/dcpo_apsr_cntrl.rda")


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


dcpo_apsr_vdemcpi <- purrr::map(1:900, function(anEntry) {
  libdem_list_z[[anEntry]] %>% 
    left_join(polyarchy_list_z[[anEntry]],by = c("year", "country","iter_cn","iter_se")) %>% 
    left_join(lib_list_z[[anEntry]],by = c("year", "country","iter_cn","iter_se")) %>% 
    left_join(cpi_list_z[[anEntry]],by = c("year", "country","iter_cn","iter_se")) %>%
    select(-Vdem_libdem, -Vdem_polyarchy,-Vdem_liberal,-iter_se)
}) 


theta_cntrl_list <- theta_sigma_list %>%
  map(~.x %>%
        left_join(dcpo_apsr_cntrl, by=c("country","year"))  %>%
        select(-iter_cn,-contains('lag1')) 
  )

dcpo_apsr <- purrr::map(1:900, function(anEntry) {
  theta_cntrl_list[[anEntry]] %>% 
    left_join(dcpo_apsr_vdemcpi[[anEntry]],by = c("year", "country")) %>%
    select(-iter_cn) %>%
    mutate(theta_trim = ifelse(year < first_year, NA, theta)) %>%
    select(country, year, first_year,theta,sigma, theta_trim, contains("z"),everything())
}) 

save(dcpo_apsr, file = "data/dcpo_apsr.rda")








