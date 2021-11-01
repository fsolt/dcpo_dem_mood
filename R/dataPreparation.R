## ----dcpo_input_raw--------------------------------
dcpo_input_raw <-
    DCPOtools::dcpo_setup(vars = read_csv(here::here("data-raw", "mood_dem.csv"),
                                          col_types = "cccccc"),
                          file = "data/dcpo_input_raw.csv")

## ----dcpo_input, eval=TRUE-------------------------
dcpo_input_raw1 <-
    read_csv(here::here("data", "dcpo_input_raw.csv"), col_types = "cdcddcd") %>%
    filter(!(
        str_detect(survey, "army_wvs") &
            # WVS obs identified as problematic by Claassen
            ((country == "Albania" & year == 1998) |
                 (country == "Indonesia" &
                      (year == 2001 | year == 2006)) |
                 (country == "Iran" & year == 2000) |
                 (country == "Pakistan" &
                      (year == 1997 | year == 2001)) | # 1996 in Claassen
                 (country == "Vietnam" & year == 2001)
            ) |
            (str_detect(item, "strong_wvs") &
                 ((country == "Egypt" & year == 2012) |
                      (country == "Iran" &
                           (year == 2000 | year == 2007)) | # 2005 in Claassen
                      (country == "India") |
                      (country == "Pakistan" &
                           (year == 1997 | year == 2001)) | # 1996 in Claassen
                      (country == "Kyrgyzstan" &
                           (year == 2003 | year == 2011)) |
                      (country == "Romania" &
                           (year == 1998 | year == 2005 | year == 2012)) |
                      (country == "Vietnam" & year == 2001)
                 )) |
            (
                country %in% c(
                    "Puerto Rico",
                    "Northern Ireland",
                    "SrpSka Republic",
                    "Hong Kong SAR China"
                )
            )
    )) %>%
    with_min_yrs(2)

dcpo_input <- DCPOtools::format_dcpo(dcpo_input_raw1,
                                     scale_q = "church_lb",
                                     scale_cp = 2)
save(dcpo_input, file = here::here("data", "dcpo_input.rda"))

## ----dcpo------------------------------------------
iter <- 3000

dcpo_output <- dcpo(dcpo_input,
                    iter = iter,
                    chains = 4,
                    thin = iter/500, # this yields 250 draws per chain, 1000 draws total
                    pars = c("sd_delta","sd_theta_evolve", "sd_sigma_evolve", "sigma","phi","beta","alpha","delta","theta","y_r_pred","log_lik"))

save(dcpo_output, file = here::here("data", "dcpo_output.rda"))

dcpo_theta <- rstan::extract(dcpo_output, pars = "theta")

save(dcpo_theta, file = here::here("data", "dcpo_theta.rda"))

dcpo_sigma <- rstan::extract(dcpo_output, pars = "sigma")

save(dcpo_sigma, file = here::here("data", "dcpo_sigma.rda"))

dcpo_y_r_pred <- rstan::extract(dcpo_output, pars = "y_r_pred")

save(dcpo_input, dcpo_y_r_pred, file = here::here("data", "dcpo_y_r_pred.rda"))

## ----data-dcpoUncertainAPSR------------------------
ls_country <- levels(dcpo_input$data$kk)
first_year <- min(dcpo_input$data$year)

df_dcpoUncertainAPSR <- purrr::map(1:1000, function(anEntry) {
    dcpo_theta$theta[anEntry,,] %>%
        reformat_dcpo_output("theta") %>%
        left_join(reformat_dcpo_output(dcpo_sigma$sigma[anEntry,,], "sigma"),
                  by = c("year", "country")) %>%
        left_join(df_controls, by = c("country", "year")) %>%
        plm::pdata.frame(index = c("country", "year"))
})

saveRDS(df_dcpoUncertainAPSR, here("data", "df_dcpoUncertainAPSR.rds"))



## ----country-years---------------------------------
path_df <-
    list.files(
        "../data",
        pattern = ".*_theta.rda",
        recursive = TRUE,
        full.names = TRUE
    ) %>%
    .[str_which(., "claassen")]

load(path_df)

df_thetaDraw <- map_df(seq(claassen_m5_theta$theta[1, , 1]),
                       function(year) {
                           map_df(seq(claassen_m5_theta$theta[1, 1, ]),
                                  function(country) {
                                      df_temp <-
                                          sample(claassen_m5_theta$theta[, year, country],
                                                 size = 100,
                                                 replace = TRUE) %>%
                                          matrix(ncol = 100) %>%
                                          as_tibble()
                                      df_temp$yearID <- year
                                      df_temp$countryID <- country
                                      return(df_temp)
                                  })
                       }) %>% 
    pivot_longer(
        cols = starts_with("V"),
        names_to = "draw",
        values_to = "theta"
    ) %>% 
    group_split(draw)

