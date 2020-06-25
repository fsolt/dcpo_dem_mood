# setup_cls (for claassen m.5 replication) --------------------------------------------------


library(tidyverse)
library(countrycode)
library(DCPOtools)


load("data/cys_crosswalk.RData")

claassen_input_raw1 <-
    read_csv("data/claassen_input_raw.csv", col_types = "cdcddcd") %>%
    filter(!(str_detect(item, "army_wvs") &
                 # WVS obs identified as problematic by Claassen
                 ((country == "Albania" & year == 1998) |
                      (country == "Indonesia" &
                           (year == 2001 | year == 2006)) |
                      (country == "Iran" & year == 2000) |
                      (country == "Pakistan" &
                           (year == 1997 | year == 2001)) |
                      #in DCPO,it is 1997 instead of 1996
                      (country == "Vietnam" & year == 2001)
                 ) |
                 (
                     str_detect(item, "strong_wvs") &
                         ((country == "Egypt" & year == 2012) |
                              (country == "Iran" &
                                   (year == 2000 |
                                        year == 2007)) | #in DCPO, the year is 2007 rather than 2005
                              (country == "India") |
                              (country == "Pakistan" &
                                   (year == 1997 | year == 2001)) |
                              #in DCPO,it is 1997 instead of 1996
                              (country == "Kyrgyzstan" &
                                   (year == 2003 | year == 2011)) |
                              (country == "Romania" &
                                   (year == 1998 | year == 2005 | year == 2012)) |
                              (country == "Vietnam" &
                                   year == 2001)
                         )
                 ))) %>%
    anti_join(cys_to_drop, by = c("country", "year", "survey")) %>% # surveys unused by Claassen
    left_join(cys_crosswalk, by = c("country", "year" = "y_dcpo", "survey")) %>%
    mutate(year = if_else(!is.na(y_claassen), y_claassen, year)) %>% # use Claassen's year codings
    mutate(item = if_else(item == "strong_amb_1" &
                              year == 2004, "strong_amb_2", item)) # items conflated in amb_combo file

#claassen_input <- DCPOtools::format_claassen(claassen_input_raw1)

write.csv(claassen_input_raw1, "data/claassen_replication.csv")




