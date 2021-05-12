####Create v-dem8 with posterior and merge with Claassen's  orginal control variables. 
####variables with "post" are calculated by using 900 posterior distribution. 


library(tidyverse)
library(countrycode)
library(haven)
library(stringr)


vdem8_1985_post <- readRDS("data/vdem8_1985_post.rds")

long_poly <- vdem8_1985_post %>% 
    select(!starts_with("libdem_"))  %>%
    select(!starts_with("liberal_v")) %>%
    gather(poly_iter, poly_post, polyarchy_v11:polyarchy_v900) %>%
    select(country, year, starts_with("poly"),everything() ) %>%
    mutate(piter = as.numeric(str_replace(poly_iter, "\\D+", ""))) %>%
    mutate(piter = piter -1 ) %>%
    select(-poly_iter) %>%
    select(country, year, starts_with("poly"), piter,everything())

long_libdem <- vdem8_1985_post %>% 
    select(!starts_with("polyarchy_v"))  %>%
    select(!starts_with("liberal_v")) %>%
    gather(libdem_iter, libdem_post, libdem_v11:libdem_v900) %>%
    select(country, year,starts_with("libdem")) %>%
    mutate(liter = as.numeric(str_replace(libdem_iter, "\\D+", ""))) %>%
    mutate(liter = liter -1 ) %>%
    select(-libdem_iter) %>%
    select(country, year, starts_with("libdem"), liter,everything())

vdem8_1985_uncertainty <- left_join(long_poly,long_libdem, by = c("country","year", "piter" = "liter"))
saveRDS(vdem8_1985_uncertainty, file = "data/vdem8_1985_uncertainty.rds")


#########Using claassen's control variables, but adding key ivs including  
#########Vdem_libdem_postm1, Vdem_libdem_postm2, Chgdem_post,upchgdem_post,
#########downchgdem_post,Region_libdem_post,Region_libdem_postm1. 
#vdem8_1985_uncertainty <- readRDS("C:/Users/skywa/Dropbox/claassen/uncertainty_data/vdem8_1985_uncertainty.rds")

cls_vdem <- read.csv("data/Support_democracy_ajps.csv",stringsAsFactors = F)


cls_vdem <- cls_vdem %>%
    mutate(ISO_code = ifelse(is.na(ISO_code),
                             countrycode::countrycode(Country, "country.name", destination = 'iso3c'),ISO_code)) %>% 
    mutate(country_name = countrycode::countrycode(ISO_code, 
                                                   "iso3c", destination = 'country.name'))  %>%
    rename(cls_country = Country,
           country = country_name)

cls_unregion <- cls_vdem %>%
    select(country, Region_UN, ISO_code) %>%
    filter(!is.na(ISO_code)) %>%
    distinct(country, Region_UN)

vdem8_1985_simple <- vdem8_1985_uncertainty %>%
    select(country,year,poly_post,piter, libdem_post)  %>%
    rename(iter_se = piter) %>%
    mutate(Vdem_poly_post = poly_post*100,
           Vdem_libdem_post = libdem_post*100) %>%
    select(-poly_post,-libdem_post) 

vdem8_unregion <- left_join(vdem8_1985_simple,cls_unregion,by= c("country"))


vdem8_unregion <- vdem8_unregion %>%
    mutate(Region_UN = ifelse(country %in% c("Angola", "Central African Republic",
                                             "Chad","Congo - Brazzaville", "Congo - Kinshasa", 
                                             "Equatorial Guinea"), "Middle Africa",
                              ifelse(country %in% c("Bahrain","Oman","Qatar","Syria",
                                                    "United Arab Emirates","Yemen People's Republic"), "Western Asia",
                                     ifelse(country %in% c("Barbados","Cuba"),"Caribbean",
                                            ifelse(country %in% c("Bhutan" ,"Laos","Maldives","Afghanistan"),"Southern Asia",
                                                   ifelse(country %in% c("Comoros", "Djibouti" ,"Eritrea","Seychelles","Somalia","South Sudan"),"Eastern Africa",
                                                          ifelse(country %in% c("Fiji","Papua New Guinea","Solomon Islands","Vanuatu"),"Melanesia",
                                                                 ifelse(country %in% c("Gambia","Guinea-Bissau","Mauritania"),"Western Africa",
                                                                        ifelse(country %in% c("German Democratic Republic"), "Western Europe",
                                                                               ifelse(country %in% c("Hong Kong SAR China" ,"North Korea"),"Eastern Asia",
                                                                                      ifelse(country %in% c("Timor-Leste"), "South-Eastern Asia",
                                                                                             ifelse(country %in% c("Turkmenistan"),"Central Asia",
                                                                                                    Region_UN)))))))))
                              )))


##### regional, and lag. then done 

vdem8_lag <- vdem8_unregion %>%
    group_by(country, iter_se) %>%
    arrange(year, .by_group = TRUE) %>% 
    mutate(Vdem_libdem_postm1 = lag(Vdem_libdem_post),
           Vdem_libdem_postm2 = lag(Vdem_libdem_post,2)) %>%
    ungroup() %>%
    mutate(Chgdem_post = Vdem_libdem_post - Vdem_libdem_postm1,
           upchgdem_post = ifelse(Chgdem_post > 0, Chgdem_post,0),
           downchgdem_post = ifelse(Chgdem_post < 0, Chgdem_post*(-1),0)) %>%
    ungroup() %>%
    group_by(Region_UN,year,iter_se) %>%
    mutate(Region_libdem_post = mean(Vdem_libdem_post, na.rm=TRUE)) %>%
    ungroup() %>%
    group_by(country,iter_se) %>%
    arrange(year, .by_group = TRUE) %>% 
    mutate(Region_libdem_postm1 = lag(Region_libdem_post))  %>%
    ungroup() %>%
    group_by(Region_UN,year,iter_se)  %>%
    select(country,Region_UN, year, iter_se, Region_libdem_post,Region_libdem_postm1, starts_with("Vdem"),everything()) 

glimpse(vdem8_lag)

cls_vdem_cntrl <- cls_vdem %>% group_by(country, Year) %>% 
    expand(iter_cn = 1:900)  %>%
    group_by(country,Year) %>% 
    arrange(iter_cn,.by_group = TRUE)  %>% 
    left_join(cls_vdem, by = c("country","Year"))

cls_ajps_uncertainty <- vdem8_lag %>%
    rename(Year = year)  %>%
    inner_join(cls_vdem_cntrl, by =c("country","Year", "iter_se" = "iter_cn","Region_UN")) %>%
    select(country, Year, starts_with("Vdem"),iter_se,everything())  %>%
    select(-Cnt_code)

glimpse(cls_ajps_uncertainty)

saveRDS(cls_ajps_uncertainty, "data/cls_ajps_uncertainty.rds")
