library(tidyverse)
library(DCPOtools)

# claassen_input_raw <- DCPOtools:::claassen_setup(vars = read_csv(here::here("data-raw", "mood_dem.csv"),
#                                                                  col_types = "cccccc"),
#                                                  file = "data/claassen_input_raw.csv")

claassen_input_raw <- read_csv(here::here("data", "claassen_input_raw.csv"),
                               col_types = "cdcddcd")

claassen_input_raw1 <- claassen_input_raw %>% 
    filter(!((str_detect(item, "army_wvs") & # WVS obs identified as problematic by Claassen 
                  ((country=="Albania" & year==1998) |
                       (country=="Indonesia" & (year==2001 | year==2006)) |
                       (country=="Iran" & year==2000) |
                       (country=="Pakistan" & (year==1997 | year==2001)) | # 1996 in Claassen
                       (country=="Vietnam" & year==2001))) |
                 (str_detect(item, "strong_wvs") &
                      ((country=="Egypt" & year==2012) |
                           (country=="Iran" & (year==2000 | year==2007)) | # 2005 in Claassen
                           (country=="India") |
                           (country=="Pakistan" & (year==1997 | year==2001)) | # 1996 in Claassen
                           (country=="Kyrgyzstan" & (year==2003 | year==2011)) |
                           (country=="Romania" & (year==1998 | year==2005 | year==2012)) |
                           (country=="Vietnam" & year==2001))) |
                 (country %in% c("Puerto Rico", "Northern Ireland", 
                                 "SrpSka Republic", "Hong Kong SAR China")))) %>%
    with_min_yrs(2)

exp_claassen_input <- DCPOtools:::format_claassen(claassen_input_raw1)
save(exp_claassen_input, file = here::here("data", "exp_claassen_input.rda"))
