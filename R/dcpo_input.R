library(tidyverse)
library(DCPOtools)

# dcpo_input_raw <- DCPOtools::dcpo_setup(vars = read_csv(here::here("data-raw", "mood_dem.csv"),
#                                                         col_types = "cccccc"),
#                                         file = "data/dcpo_input_raw.csv")

dcpo_input_raw1 <- read_csv(here::here("data", "dcpo_input_raw.csv"), 
                            col_types = "cdcddcd") %>%
                            with_min_yrs(2)

dcpo_input_update22 <- DCPOtools::format_dcpo(dcpo_input_raw1,
                                              scale_q = "church_wvs",
                                              scale_cp = 2)
save(dcpo_input_update22, file = here::here("data", "dcpo_input_update22.rda"))
