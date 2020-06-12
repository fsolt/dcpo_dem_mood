library(DCPOtools)
library(tidyverse)

supdem <- dataverse::get_file("supdem raw survey marginals.tab", "doi:10.7910/DVN/HWLW0J") %>% 
    read_csv(col_types = "cdcddcdc")

supdem_cy <- supdem %>%                                             # 1390 obs
    janitor::clean_names() %>% 
    mutate(old_country = country,
           country = countrycode::countryname(old_country)) %>%
    dplyr::select(country, year, project) %>% 
    unique()

claassen_input_cy <- read_csv("data/claassen_input_raw.csv") %>%    # 1504 obs
    mutate(p_dcpo = str_extract(survey, "^[a-z]+"), 
           project = case_when(p_dcpo == "afrob" ~ "afb",
                               p_dcpo == "amb" ~ "lapop",
                               p_dcpo == "arabb" ~ "arb",
                               p_dcpo == "asiab" ~ "asb",
                               p_dcpo == "asianb" ~ "asnb",
                               p_dcpo == "neb" ~ "ndb",
                               p_dcpo == "sasianb" ~ "sab",
                               TRUE ~ p_dcpo),
           y_dcpo = year,
           old_country = country,
           country = countrycode::countryname(old_country)) %>%
    dplyr::select(country, year, y_dcpo, survey, project) %>% 
    unique()

no_problems <- inner_join(supdem_cy, claassen_input_cy)             # 1259 obs

needed <- anti_join(supdem_cy, claassen_input_cy)                   # 131 obs

available <- anti_join(claassen_input_cy, supdem_cy)                # 245 obs

try <- left_join(needed, available, by = c("country", "project"))

