library(DCPOtools)
library(tidyverse)

supdem <- dataverse::get_file("supdem raw survey marginals.tab", "doi:10.7910/DVN/HWLW0J") %>% 
    read_csv(col_types = "cdcddcdc")

supdem_cy <- supdem %>%                                             # 1390 obs
    janitor::clean_names() %>% 
    mutate(old_country = country,
           country = countrycode::countrycode(old_country, "country.name", "country.name")) %>%
    dplyr::select(country, year, project) %>% 
    unique()

claassen_input_cy <- read_csv("data/claassen_input_raw.csv",
                              col_types = "cdcddcd") %>%    # 1560 obs
    mutate(p_dcpo = str_extract(survey, "^[a-z]+"), 
           project = case_when(p_dcpo == "afrob" ~ "afb",
                               p_dcpo == "amb" ~ "lapop",
                               p_dcpo == "arabb" ~ "arb",
                               p_dcpo == "asiab" ~ "asb",
                               p_dcpo == "asianb" ~ "asnb",
                               p_dcpo == "neb" ~ "ndb",
                               p_dcpo == "sasianb" ~ "sab",
                               TRUE ~ p_dcpo),
           y_dcpo = year) %>%
    dplyr::select(country, year, y_dcpo, survey, project) %>% 
    unique()

no_problems <- inner_join(supdem_cy, claassen_input_cy)             # 1297 obs

needed <- anti_join(supdem_cy, claassen_input_cy)                   # 93 obs

available <- anti_join(claassen_input_cy, supdem_cy)                # 263 obs

year_fixes <- left_join(needed, available, by = c("country", "project")) %>% # 89 obs
    mutate(diff = year.x - year.y) %>% 
    group_by(country, project, year.x) %>% 
    mutate(closest_to_claassen = min(abs(diff))) %>% 
    ungroup() %>% 
    group_by(country, project, year.y) %>% 
    mutate(closest_to_dcpo = min(abs(diff))) %>% 
    ungroup() %>% 
    filter(closest_to_claassen == abs(diff) & closest_to_dcpo == abs(diff) & abs(diff) <= 3) %>% 
    filter(!(country == "Egypt" & year.x == 2014 & survey == "afrob5")) # double match (it's really afrob6)

cys_crosswalk <- year_fixes %>% 
    select(country, y_dcpo, y_claassen = year.x, survey)

still_needed <- anti_join(needed, year_fixes,  by = c("country", "year" = "year.x", "project")) # 4 obs; listed in issue #5 

cys_to_drop <- anti_join(available, year_fixes, by = c("country", "year" = "year.y", "project")) %>% 
    select(-y_dcpo)

save(cys_crosswalk, cys_to_drop,
     file = "data/cys_crosswalk.RData")
