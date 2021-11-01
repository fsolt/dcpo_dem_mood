## ----setup, include=FALSE--------------------------
options(tinytex.verbose = TRUE)

knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  eval = FALSE,
  dpi = 300
)

# If you haven't install `DCPOtools`
# remotes::install_github("fsolt/DCPOtools")

if (!require(pacman)) install.packages("pacman")
library(pacman)

# load all the packages you will use below 
p_load(
  # data scrapping
  dataverse, 
  DCPOtools, mitools,
  boot, 
  here,
  # analysis
  plm, 
  rstan, 
  # presentation
  flextable, 
  broomExtra, 
  kableExtra, 
  modelsummary, 
  gridExtra,
  latex2exp,
  drhutools,
  dotwhisker, 
  # data wrangling
  tidyverse, 
  janitor, 
  glue, 
  qs
) 

# Functions preload
set.seed(313)

source(here("R", "tidy_pgmm.r"))
source(here("R", "moc_function.R")) 
source(here("R", "moc_summary.R")) 

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Theme setup
theme_set(theme_minimal())

# Data input

supdem <- read.csv("../data/Support_democracy_ajps.csv")
v_dem <- read.csv("../data/vdem15.csv")
sd.plm <- pdata.frame(supdem, index = c("Country", "Year")) 

claassen_replication <- readRDS("../data/claassen_replication.rds") %>% 
  janitor::clean_names() %>% 
  DCPOtools::with_min_yrs(2)
load(here("data", "claassen_m5_theta.rda"))
load(here("data", "dem_mood_apsr.RData"))
load(here("data", "dcpo_input.rda"))
load(here("data", "dcpo_theta.rda"))
load(here("data", "dcpo_sigma.rda"))
load(here("data", "dcpo_ajps_rubin.rda"))

## Beck-Katz panel-corrected standard errors
vcovBK_se <- function(x) {
  plm::vcovBK(x, cluster = "time") %>% 
    diag() %>% 
    sqrt()
}


vcovHC_se <-  function(x) {
  plm::vcovHC(x, method="arellano", cluster="group") %>%  #default setting
    diag() %>% 
    sqrt()
}


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


## ----compare_n, eval=TRUE--------------------------
# if (!file.exists(here::here("data", "supdem raw survey marginals.tab"))) {
#   dataverse::get_file("supdem raw survey marginals.tab", "doi:10.7910/DVN/HWLW0J") %>% 
#     read_csv(col_types = "cdcddcdc") %>% 
#     write_csv("data/supdem raw survey marginals.tab")
# }

supdem <- read_csv(here::here("data", "supdem raw survey marginals.tab"),
                   col_types = "cdcddcdc") %>% 
  janitor::clean_names() %>% 
  DCPOtools::with_min_yrs(2)

supdem_ncy <- supdem %>%
  dplyr::select(country, year) %>% 
  distinct() %>% 
  nrow()

supdem_surv <- supdem %>%
  dplyr::select(country, year, project) %>%
  distinct() %>%
  nrow()

supdem_ncyi <- supdem %>%
  dplyr::select(country, year, item) %>% 
  distinct() %>% 
  nrow()

dcpo_input_nc <- dcpo_input_raw1 %>% 
  dplyr::select(country) %>% 
  distinct() %>% 
  nrow()

dcpo_input_surv <- dcpo_input_raw1 %>%
  dplyr::select(country, year, survey) %>%
  distinct() %>%
  separate(survey, c("surv1", "surv2"), sep=",", fill = "left") %>%
  pivot_longer(cols = starts_with("surv")) %>%
  filter(!is.na(value)) %>% 
  nrow()

dcpo_input_ncy <- dcpo_input_raw1 %>% 
  dplyr::select(country, year) %>% 
  distinct() %>% 
  nrow()

dcpo_input_ncyi <- dcpo_input_raw1 %>% 
  dplyr::select(country, year, item, survey) %>% 
  distinct() %>% 
  separate(survey, c("surv1", "surv2"), sep=",", fill = "left") %>%
  pivot_longer(cols = starts_with("surv")) %>%
  filter(!is.na(value)) %>% 
  nrow()

increase_cy <- sprintf("%2.1f", (dcpo_input_ncy/supdem_ncy - 1) * 100, 1)
increase_cyi <- sprintf("%2.1f", (dcpo_input_ncyi/supdem_ncyi - 1) * 100, 1)
increase_surv <- sprintf("%2.1f", (dcpo_input_surv/supdem_surv - 1) * 100, 1)


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


## ----claassenAJPS, eval=TRUE-----------------------
ls_iv <- c("plm::lag(SupDem_trim, 1)",
           "plm::lag(SupDem_Democ, 1) + plm::lag(SupDem_Autoc, 1)")

ls_method <- c("plm", "pgmm")

ls_eq <- glue("Libdem_VD ~ plm::lag(Libdem_VD, 1:2) + {ls_iv} + plm::lag(lnGDP_imp, 1) + plm::lag(GDP_imp_grth, 1) + plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1) + plm::lag(Res_cp_WDI_di, 1)") 

ls_mod <- c(
  glue("plm({ls_eq}, model = 'pooling', data = sd.plm)"),
  glue(
    "pgmm({ls_eq} | plm::lag(Libdem_VD, 3:5), data = sd.plm, effect='individual', model='onestep', transformation='ld', indexes=c('Country', 'Year'))"
  )
)

df_result_cls <- map(ls_mod, function(mod) {
  result <- eval(parse(text = mod))
})

tidy_cls <- map(df_result_cls, function(result) {
  df_result <- tidy(result, conf.int = TRUE)
  
  if (class(result) == "plm") {
     mutate(df_result, std.error = vcovBK_se(result))
  } else{
    mutate(df_result, std.error = vcovHC_se(result))
  }
  
})

glance_cls <- map(df_result_cls, function(aResult){
  if(class(aResult) == "plm"){ 
    glance.plm(aResult) # using the customized glance function
  }else{
      glance(aResult)
    }
})

result_cls <- map2(tidy_cls, glance_cls, ~ list(.x, .y))
names(result_cls) <- c("pooled", "pooled-regime", "gmm", "gmm-regime")

saveRDS(result_cls, file = here::here("output", "estimates_clsMeanAJPS.RDS"))


## ----claassenUncertaintyAJPS-----------------------
ls_country <- unique(claassen_replication$country)
first_year <- min(claassen_replication$year)

sd_control <- sd.plm %>%
  mutate(
    country = as.character(Country),
    year = as.numeric(as.character(Year)),
    regime = ifelse(Regime_VD > 1, 1, 0)
  )

df_cls <- purrr::map(1:1000, function(anEntry) {
  claassen_m5_theta$theta[anEntry,,] %>% 
    reformat_dcpo_output("theta") %>% 
    # left_join(reformat_dcpo_output(dcpo_sigma$sigma[anEntry,,], "sigma"),
    #           by = c("year", "country")) %>%
    left_join(sd_control, by = c("country", "year")) %>% 
    mutate(theta_dem = theta * regime,
           theta_aut = theta * (1 - regime)) %>% 
    plm::pdata.frame(index = c("country", "year"))
})

ls_iv <- c("plm::lag(theta, 1)", 
    "plm::lag(theta_dem, 1) + plm::lag(theta_aut, 1) ") 

ls_method <- c("plm", "pgmm")

ls_eq <- glue("Libdem_VD ~ plm::lag(Libdem_VD, 1:2) + {ls_iv} + plm::lag(lnGDP_imp, 1) + plm::lag(GDP_imp_grth, 1) + plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1) + plm::lag(Res_cp_WDI_di, 1)") 

ls_mod <- c(
    glue("plm({ls_eq}, model = 'pooling', data = aData)"),
    glue(
        "pgmm({ls_eq} | plm::lag(Libdem_VD, 3:5), data = aData, effect='individual', model='onestep', transformation='ld', indexes=c('country', 'year'))"
    )
)

result_clsUncertain <- map(ls_mod, function(aMod){ # reading a model
  result <- map(df_cls, function(aData){ # applying to the 1000 datasets
    eval(parse(text = aMod))
  })
  
  result_betas <- mitools::MIextract(result, fun = coef)
  
  ## Not work with ifelse
  if(class(result[[1]])[1] == "plm"){
      result_vars <- mitools::MIextract(result, fun = vcovBK_se)
  }else{
      result_vars <- mitools::MIextract(result, fun = vcovHC_se)
  }
  
  
  tidy_clsUncertain <- summary(mitools::MIcombine(results = result_betas, 
                             variance = result_vars)) %>% 
    rownames_to_column(var = "term") %>%
        rename(conf.low = `(lower`,
               conf.high = `upper)`,
               estimate = results) %>%
        filter(!str_detect(term, "(Intercept)"))
  
  if(class(result[[1]]) == "plm"){ 
    glance_clsUncertain <- glance.plm(result[[1]]) # using the customized glance function
  }else{
    glance_clsUncertain <- glance(result[[1]])
  }
  
  result_clsUncertain <- list(tidy_clsUncertain, glance_clsUncertain)
})


names(result_clsUncertain) <- c("pooled", "pooled-regime", "gmm", "gmm-regime")
   
saveRDS(result_clsUncertain, file = here::here("output", "estimates_clsAJPS.RDS"))


## ----dcpoMean--------------------------------------
ls_country <- levels(dcpo_input$data$kk)
first_year <- min(dcpo_input$data$year)

df_dcpoMean <- purrr::map_df(1:1000, function(anEntry) {
  dcpo_theta$theta[anEntry,,] %>%
    reformat_dcpo_output("theta") %>%
    left_join(reformat_dcpo_output(dcpo_sigma$sigma[anEntry,,], "sigma"),
              by = c("year", "country"))
}) %>%
  group_by(country, year) %>%
  summarise(theta = mean(theta),
            sigma = mean(sigma))

saveRDS(df_dcpoMean, file = here("data", "df_dcpoMean.RDS"))


## ----dcpoPointAJPS---------------------------------
df_controls <- readRDS(here("data", "control_variables_ajps.rds")) %>% 
  mutate(country = countrycode::countrycode(country, "country.name", "country.name"),
         regime = Vdem_regime > 1) %>% 
  filter(year >= first_year) # using trimmed data as Claassen 

df_dcpoMeanAJPS <- df_dcpoMean %>% 
  right_join(df_controls, by = c("country", "year")) %>% 
  mutate(theta_dem = theta * regime,
         theta_aut = theta * (1 - regime)) %>% 
    plm::pdata.frame(index = c("country", "year"))

ls_iv <- c("plm::lag(theta, 1)", 
           "plm::lag(theta_dem, 1) + plm::lag(theta_aut, 1) ")

ls_method <- c("plm", "pgmm")

ls_eq <- glue("Vdem_libdem ~ plm::lag(Vdem_libdem, 1:2) + {ls_iv} + plm::lag(lg_imp_mdpgdp, 1) + plm::lag(mdprgdp_grwth, 1) + plm::lag(Region_libdem, 1) + plm::lag(muslism_prop_2010, 1) + plm::lag(dependence_pc_di, 1)") 

ls_mod <- c(
  glue("plm({ls_eq}, model = 'pooling', data = df_dcpoMeanAJPS)"),
  glue(
    "pgmm({ls_eq} | plm::lag(Vdem_libdem, 3:5), data = df_dcpoMeanAJPS, effect='individual', model='onestep', transformation='ld', indexes=c('country', 'year'))"
  )
)

#There's a problem to run this part---------------------

df_result_dcpo <- map(ls_mod, function(mod) {
  result <- eval(parse(text = mod))
})

tidy_dcpo <- map(df_result_dcpo, function(result) {
  df_result <- tidy(result, conf.int = TRUE)
  
  if (class(result) == "plm") {
     mutate(df_result, std.error = vcovBK_se(result))
  } else{
    mutate(df_result, std.error = vcovHC_se(result))
  }
  
})

glance_dcpo <- map(df_result_dcpo, function(aResult){
  if(class(aResult) == "plm"){ 
    glance.plm(aResult) # using the customized glance function
  }else{
      glance(aResult)
    }
})

result_dcpo <- map2(tidy_dcpo, glance_dcpo, ~ list(.x, .y))
names(result_dcpo) <- c("pooled", "pooled-regime", "gmm", "gmm-regime")

saveRDS(result_dcpo, file = here::here("output", "estimates_dcpoMeanAJPS.RDS"))


## ----dcpoUncertaintyAJPS---------------------------
df_dcpo <- dcpo_ajps_rubin %>% map2(1:length(dcpo_ajps_rubin), ~ {
  .x[[.y]] %>%
    mutate(theta_dem = theta * regime,
           theta_aut = theta * (1 - regime)) %>%
    plm::pdata.frame(index = c("country", "year"))
}) # The code is to fit the embedded format of dcpo_ajps_rubin

# df_dcpo <- purrr::map(1:1000, function(anEntry) {
#   dcpo_theta$theta[anEntry,,] %>% 
#     reformat_dcpo_output("theta") %>% 
#     left_join(reformat_dcpo_output(dcpo_sigma$sigma[anEntry,,], "sigma"),
#               by = c("year", "country")) %>%
#     left_join(df_controls, by = c("country", "year")) %>% 
#     mutate(theta_dem = theta * regime,
#            theta_aut = theta * (1 - regime)) %>% 
#     plm::pdata.frame(index = c("country", "year"))
# })

ls_iv <- c("plm::lag(theta, 1)", 
    "plm::lag(theta_dem, 1) + plm::lag(theta_aut, 1) ") 

ls_method <- c("plm", "pgmm")

ls_eq <- glue("Vdem_libdem ~ plm::lag(Vdem_libdem, 1:2) + {ls_iv} + plm::lag(lg_imp_mdpgdp, 1) + plm::lag(mdprgdp_grwth, 1) + plm::lag(Region_libdem, 1) + plm::lag(muslism_prop_2010, 1) + plm::lag(dependence_pc_di, 1)") 

ls_mod <- c(
    glue("plm({ls_eq}, model = 'pooling', data = aData)"),
    glue(
        "pgmm({ls_eq} | plm::lag(Vdem_libdem, 3:5), data = aData, effect='individual', model='onestep', transformation='ld', indexes=c('country', 'year'))"
    )
)

result_dcpoUncertain <- map(ls_mod, function(aMod){ # reading a model
  mod_eval <- function(aData) eval(parse(text = aMod))
  
  result <- map(df_dcpo, mod_eval)
  
  result_betas <- mitools::MIextract(result, fun = coef)

  ## Not work with ifelse
  if(class(result[[1]])[1] == "plm"){
      result_vars <- mitools::MIextract(result, fun = vcovBK_se)
  }else{
      result_vars <- mitools::MIextract(result, fun = vcovHC_se)
  }


  tidy_dcpoUncertain <- summary(mitools::MIcombine(results = result_betas,
                             variance = result_vars)) %>%
    rownames_to_column(var = "term") %>%
        rename(conf.low = `(lower`,
               conf.high = `upper)`,
               estimate = results) %>%
        filter(!str_detect(term, "(Intercept)"))

  if(class(result[[1]]) == "plm"){
    glance_dcpoUncertain <- glance.plm(result[[1]]) # using the customized glance function
  }else{
    glance_dcpoUncertain <- glance(result[[1]])
  }

  result_dcpoUncertain <- list(tidy_dcpoUncertain, glance_dcpoUncertain)
})

names(result_dcpoUncertain) <- c("pooled", "pooled-regime", "gmm", "gmm-regime")

saveRDS(result_dcpoUncertain, file = here::here("output", "estimates_dcpoAJPS.RDS"))


## ----visualizeAJPS, fig.cap= "The Effect of Public Support on Democracy", eval = TRUE, fig.width=7.5, fig.height=10----

# Very weird phenomenon that the same type of rds files needs different ways to read
# Suggesting using the qs or csv data type in future.
result_cls <- readRDS(here::here("output", "estimates_clsMeanAJPS.RDS"))
result_clsUncertain <- readRDS(here::here("output", "estimates_clsAJPS.RDS"))
result_dcpoUncertain <- readRDS(here::here("output", "estimates_dcpoAJPS.RDS"))

result_pooled <- bind_rows(
  mutate(result_cls$pooled[[1]], model = "Claassen 2020a"),
  mutate(result_clsUncertain$pooled[[1]], model = "Claassen w/ Uncertainty"),
  mutate(result_dcpoUncertain$pooled[[1]], model = "DCPO")
) %>% 
  mutate(type = "Pooled")

result_gmm <- bind_rows(
  mutate(result_cls$gmm[[1]], model = "Claassen 2020a"),
  mutate(result_clsUncertain$gmm[[1]], model = "Claassen w/ Uncertainty"),
  mutate(result_dcpoUncertain$gmm[[1]], model = "DCPO")
) %>% 
  mutate(type = "GMM")

result_pooledAJPS <- bind_rows(result_pooled, result_gmm) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = rep(
      c("Democracy\n (t-1)",
        "Democracy\n (t-2)",
      "Support\n (t-1)",
      "Log GDP\n per capita\n (t-1)",
      "GDP per\n capita growth\n (t-1)",
      "Regional\n democracy\n (t-1)",
      "Percent\n Muslim\n (t-1)",
      "Resource\n dependence\n (t-1)"),
    time = 6), 
    type = factor(type, levels = c("Pooled", "GMM")),
    model = factor(model, levels = c("Claassen 2020a", "Claassen w/ Uncertainty", "DCPO")))

terms <- factor(rev(c("Democracy[t-1]",
        "Democracy[t-2]",
      "Support[t-1]",
      "Log~GDP~per~capita[t-1]",
      "GDP~per~capita~growth[t-1]",
      "Regional~democracy[t-1]",
      "Percent~Muslim[t-1]",
      "Resource~dependence[t-1]")), levels =
      rev(c("Democracy[t-1]",
        "Democracy[t-2]",
      "Support[t-1]",
      "Log~GDP~per~capita[t-1]",
      "GDP~per~capita~growth[t-1]",
      "Regional~democracy[t-1]",
      "Percent~Muslim[t-1]",
      "Resource~dependence[t-1]")))

# ggplot produces reversed plot, and embarrassed to say, I can't fix, again. Any thought?
# What I've tried
# 1. using guide_legend(reverse = TRUE) will change the legend's order not the plot.
# 2. ordering models with factor levels using the original names or even c("1", "2", "3")
# 2. redefining the order of model in the dataframe or in dwplot 
# 3. checking the dwplot codes, model works in line 453, but nothing looks helpful
# 4. Every post online is to reverse 

plot_AJPS1 <- dwplot(result_pooledAJPS, dodge_size = 0.8, by_2sd = FALSE) +
  scale_y_discrete(labels = parse(text = levels(terms))) +
  scale_color_grey(start = 0.4, end = 0.8) +
  labs(
    title = "DV: V-Dem Liberal Democracy Index",
    subtitle = "Regime Compacted"
  ) +
  facet_wrap(~type, scales = "free_x") + 
  theme(legend.position = "none")

# the attemp to change the order of legends failed. Both factorize the model ahead or using the  guides(fill = guide_legend(reverse=TRUE)) fails
# The trick is  to use guide = guide_legend(reverse = TRUE) within scale_color_*(); see below in plotAJPS2. --FS

result_regime <- bind_rows(
  mutate(result_cls$`pooled-regime`[[1]], model = "Claassen 2020a"),
  mutate(result_clsUncertain$`pooled-regime`[[1]], model = "Claassen w/ Uncertainty"),
  mutate(result_dcpoUncertain$`pooled-regime`[[1]], model = "DCPO")
) %>% 
  mutate(type = "Pooled")

result_regimeGmm <- bind_rows(
  mutate(result_cls$`gmm-regime`[[1]], model = "Claassen 2020a"),
  mutate(result_clsUncertain$`gmm-regime`[[1]], model = "Claassen w/ Uncertainty"),
  mutate(result_dcpoUncertain$`gmm-regime`[[1]], model = "DCPO")
) %>% 
  mutate(type = "GMM")

result_regimeAJPS <- bind_rows(result_regime, result_regimeGmm) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = rep(
      c("Democracy\n (t-1)",
        "Democracy\n (t-2)",
      "Support\n demo only\n (t-1)",
      "Support\n auto only\n (t-1)",
      "Log GDP\n per capita\n (t-1)",
      "GDP per\n capita growth\n (t-1)",
      "Regional\n democracy\n (t-1)",
      "Percent\n Muslim\n (t-1)",
      "Resource\n dependence\n (t-1)"),
    time = 6), 
    type = factor(type, levels = c("Pooled", "GMM")),
    model = factor(model, levels = c("Claassen 2020a", "Claassen w/ Uncertainty", "DCPO")))

terms2 <- factor(rev(c("Democracy[t-1]",
                       "Democracy[t-2]",
                       "Support[t-1] (democracies~only)",
                       "Support[t-1] (autocracies~only)",
                       "Log~GDP~per~capita[t-1]",
                       "GDP~per~capita~growth[t-1]",
                       "Regional~democracy[t-1]",
                       "Percent~Muslim[t-1]",
                       "Resource~dependence[t-1]")), levels =
                   rev(c("Democracy[t-1]",
                         "Democracy[t-2]",
                         "Support[t-1] (democracies~only)",
                         "Support[t-1] (autocracies~only)",
                         "Log~GDP~per~capita[t-1]",
                         "GDP~per~capita~growth[t-1]",
                         "Regional~democracy[t-1]",
                         "Percent~Muslim[t-1]",
                         "Resource~dependence[t-1]")))

plot_AJPS2 <- dwplot(result_regimeAJPS, dodge_size = 0.8, by_2sd = FALSE) +
    scale_y_discrete(labels = parse(text = levels(terms2))) +
  scale_color_grey(start = 0.4, end = 0.8, guide = guide_legend(reverse = TRUE)) +
  labs(
    title = "",
    subtitle = "Regime Specific"
  ) +
  facet_wrap(~type, scales = "free_x") +
  theme(legend.position = "bottom", 
        legend.title = element_blank())

legend <- get_legend(plot_AJPS2)

plot_AJPS2 <- plot_AJPS2 + theme(legend.position = "none")

grid.arrange(plot_AJPS1, plot_AJPS2, legend, ncol = 1)



## ----visualizedUncertainAJPS, fig.cap= "The Effect of Public Support on Democracy with Uncertainty"----

df_plot <- df_result_dcpoMeanAJPS[[1]] %>% 
  mutate(model = "DCPO\n (Point Estimates)") 
 
df_plot <- df_result_dcpoAJPS[[1]] %>%
  mutate(model = "DCPO\n (w. Uncertainty)") %>%
  rename(std.error = se) %>%
  bind_rows(df_plot)
 
plot_pooled <- filter(df_plot, model != "Claassen 2019") %>% 
  dwplot(by_2sd = FALSE) %>%
  relabel_predictors(
    c(
      `plm::lag(Vdem_libdem, 1:2)1` = "Democracy(t-1)",
      `plm::lag(Libdem_VD, 1:2)1` = "Democracy(t-1)",
      `plm::lag(Vdem_libdem, 1:2)2` = "Democracy(t-2)",
      `plm::lag(Libdem_VD, 1:2)2` = "Democracy(t-2)",
      `plm::lag(theta, 1)` = "Democratic Mood(t-1)",
      `plm::lag(SupDem_trim, 1)` = "Democratic Mood(t-1)",
      `plm::lag(lg_imp_mdpgdp, 1)` = "Log GDP per capita(t-1)",
      `plm::lag(lnGDP_imp, 1)` = "Log GDP per capita(t-1)",
      `plm::lag(mdprgdp_grwth, 1)` = "GDP per capita Growth(t-1)",
      `plm::lag(GDP_imp_grth, 1)` = "GDP per capita Growth(t-1)",
      `plm::lag(Region_libdem, 1)` = "Regional democracy(t-1)",
      `plm::lag(Libdem_regUN, 1)` = "Regional democracy(t-1)",
      `plm::lag(muslism_prop_2010, 1)` = "Percent Muslim (t-1)",
      `plm::lag(Pr_Muslim, 1)` = "Percent Muslim (t-1)",
      `plm::lag(dependence_pc_di, 1)` = "Resource Dependence(t-1)",
      `plm::lag(Res_cp_WDI_di, 1)` = "Resource Dependence(t-1)"
    )
  ) +
  geom_vline(xintercept = 0,
             colour = "grey80",
             linetype = 2) +
  theme_minimal() +
  ggtitle("DV: Liberal Democracy (Vdem)") +
  xlab("Coefficient Estimate") +
  theme(
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )

df_plot <- df_result_dcpoMeanAJPS[[2]] %>% 
  mutate(model = "DCPO\n (Point Estimates)")

df_plot <- df_result_dcpoAJPS[[2]] %>%
  mutate(model = "DCPO\n (w. Uncertainty)") %>%
  rename(std.error = se) %>%
  bind_rows(df_plot)
 
plot_pooled2 <- filter(df_plot, model != "Claassen 2019") %>% 
  dwplot(by_2sd = FALSE) %>%
  relabel_predictors(
    c(
      `plm::lag(Vdem_libdem, 1:2)1` = "Democracy(t-1)",
      `plm::lag(Vdem_libdem, 1:2)2` = "Democracy(t-2)",
      `plm::lag(theta_dem, 1)` = "Mood Democracy(t-1)",
      `plm::lag(theta_aut, 1)` = "Mood Autocracy(t-1)",
      `plm::lag(lg_imp_mdpgdp, 1)` = "Log GDP per capita(t-1)",
      `plm::lag(mdprgdp_grwth, 1)` = "GDP per capita Growth(t-1)",
      `plm::lag(Region_libdem, 1)` = "Regional democracy(t-1)",
      `plm::lag(muslism_prop_2010, 1)` = "Percent Muslim (t-1)",
      `plm::lag(dependence_pc_di, 1)` = "Resource Dependence(t-1)"
    )
  ) +
  geom_vline(xintercept = 0,
             colour = "grey80",
             linetype = 2) +
  theme_minimal() +
  ggtitle("DV: Liberal Democracy (Vdem)") +
  xlab("Coefficient Estimate") +
  theme(
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )

gridExtra::grid.arrange(plot_pooled,plot_pooled2, ncol = 2)


## ----claassenAPSR, eval = TRUE---------------------
load(here::here("data", "dem_mood_apsr.RData"))
sd.plm <- pdata.frame(x, index = c("Country", "Year")) 

ls_ivECM <- c("diff(Libdem_z, lag = 1) + plm::lag(Libdem_z, 1)", 
              "diff(Polyarchy_z, lag = 1) + plm::lag(Polyarchy_z, 1) + diff(Liberal_z, lag = 1) + plm::lag(Liberal_z, 1)")
ls_ctrlECM <- c("", " + diff(Corrup_TI_z, lag = 1) + plm::lag(Corrup_TI_z, 1)")

ls_eqECM <- glue("diff(SupDem_trim, lag = 1) ~ plm::lag(SupDem_trim, 1:2) + {ls_ivECM} + diff(lnGDP_imp, lag = 1) + plm::lag(lnGDP_imp, 1)") %>% 
  outer(ls_ctrlECM, paste0) %>% 
  as.vector()

ls_ivFD <- c("Libdem_z",
             "Polyarchy_z + Liberal_z")

ls_ctrlFD <- c("", " + Corrup_TI_z")

ls_eqFD <- glue("SupDem_trim ~ {ls_ivFD} + lnGDP_imp") %>% 
  outer(ls_ctrlFD, paste0) %>% 
  as.vector()

ls_mod <- c(
  glue("plm({ls_eqECM}, model = 'pooling', data = sd.plm)"),
  glue("plm({ls_eqFD}, model = 'fd', index = 'Country', data = sd.plm)")
)

df_result_clsAPSR <- map(ls_mod, function(mod) {
  result <- eval(parse(text = mod))
})

result_clsAPSR <- map(df_result_clsAPSR, function(aResult) {
  tidy_result <- tidy(aResult, conf.int = TRUE) %>% 
    mutate(std.error = vcovHC_se(aResult))
  
  glance_result <- glance.plm(aResult)
  
  ls_result <- list(tidy_result, glance_result)
})

names(result_clsAPSR) <- c("pooled1", "pooled-regime1", "pooled2", "pooled-regime2", 
                           "fd1", "fd-regime1", "fd2", "fd-regime2")

saveRDS(result_clsAPSR, file = here::here("output", "estimates_clsMeanAPSR.RDS"))


## ----claassenAPSRUncertainty-----------------------
ls_country <- unique(claassen_replication$country)
first_year <- min(claassen_replication$year)

sd_control <- sd.plm %>%
  mutate(
    country = as.character(Country),
    year = as.numeric(as.character(Year))
  )

df_clsAPSR <- purrr::map(1:1000, function(anEntry) {
  claassen_m5_theta$theta[anEntry,,] %>% 
    reformat_dcpo_output("theta") %>% 
    # left_join(reformat_dcpo_output(dcpo_sigma$sigma[anEntry,,], "sigma"),
    #           by = c("year", "country")) %>%
    left_join(sd_control, by = c("country", "year")) %>% 
    plm::pdata.frame(index = c("country", "year"))
})

ls_ivECM <- c("diff(Libdem_z, lag = 1) + plm::lag(Libdem_z, 1)", 
              "diff(Polyarchy_z, lag = 1) + plm::lag(Polyarchy_z, 1) + diff(Liberal_z, lag = 1) + plm::lag(Liberal_z, 1)")
ls_ctrlECM <- c("", " + diff(Corrup_TI_z, lag = 1) + plm::lag(Corrup_TI_z, 1)")

ls_eqECM <- glue("diff(theta, lag = 1) ~ plm::lag(theta, 1:2) + {ls_ivECM} + diff(lnGDP_imp, lag = 1) + plm::lag(lnGDP_imp, 1)") %>% 
  outer(ls_ctrlECM, paste0) %>% 
  as.vector()

ls_ivFD <- c("Libdem_z",
             "Polyarchy_z + Liberal_z")

ls_ctrlFD <- c("", " + Corrup_TI_z")

ls_eqFD <- glue("diff(theta, lag = 1) ~ {ls_ivFD} + lnGDP_imp") %>% 
  outer(ls_ctrlFD, paste0) %>% 
  as.vector()

ls_mod <- c(
  glue("plm({ls_eqECM}, model = 'pooling', data = aData)"),
  glue("plm({ls_eqFD}, model = 'fd', index = 'Country', data = aData)")
)

result_clsUncertainAPSR <- map(ls_mod, function(aMod){ # reading a model
  result <- map(df_clsAPSR, function(aData){ # applying to the 1000 datasets
    eval(parse(text = aMod))
  })
  
  result_betas <- mitools::MIextract(result, fun = coef)
  
  result_vars <- mitools::MIextract(result, fun = vcovHC_se)
  
  tidy_clsUncertainAPSR <- summary(mitools::MIcombine(results = result_betas, 
                             variance = result_vars)) %>% 
    rownames_to_column(var = "term") %>%
        rename(conf.low = `(lower`,
               conf.high = `upper)`,
               estimate = results) %>%
        filter(!str_detect(term, "(Intercept)"))
  
  glance_clsUncertainAPSR <- glance.plm(result[[1]])
  
  result_clsUncertainAPSR <- list(tidy_clsUncertainAPSR, glance_clsUncertainAPSR)
})

names(result_clsUncertainAPSR) <- c("pooled1", "pooled-regime1", "pooled2", "pooled-regime2", "fd1", "fd-regime1", "fd2", "fd-regime2")

saveRDS(result_clsUncertainAPSR, file = here::here("output", "estimates_clsAPSR.RDS"))



## ----dcpoPointAPSR---------------------------------

df_dcpoMean <- readRDS(here("data", "df_dcpoMean.RDS"))

df_controls <- readRDS(here("data", "control_variables_apsr.rds")) %>% 
  mutate(country = countrycode::countrycode(country, "country.name", "country.name")) %>% 
  filter(year >= first_year)


df_dcpoMeanAPSR <- df_dcpoMean %>% 
  left_join(df_controls, by = c("country", "year")) %>% 
  plm::pdata.frame(index = c("country", "year"))


ls_ivECM <- c("diff(Vdem_libdem_z, lag = 1) + plm::lag(Vdem_libdem_z, 1)", 
              "diff(Vdem_polyarchy_z, lag = 1) + plm::lag(Vdem_polyarchy_z, 1) + diff(Vdem_liberal_z, lag = 1) + plm::lag(Vdem_liberal_z, 1)")
ls_ctrlECM <- c("", " + diff(corruption_z, lag = 1) + plm::lag(corruption_z, 1)")

ls_eqECM <- glue("diff(theta, lag = 1) ~ plm::lag(theta, 1:2) + {ls_ivECM} + diff(lg_imp_mdpgdp, lag = 1) + plm::lag(lg_imp_mdpgdp, 1)") %>% 
  outer(ls_ctrlECM, paste0) %>% 
  as.vector()

ls_ivFD <- c("Vdem_libdem_z",
             "Vdem_polyarchy_z + Vdem_liberal_z")

ls_ctrlFD <- c("", " + corruption_z")

ls_eqFD <- glue("theta ~ {ls_ivFD} + lg_imp_mdpgdp") %>% 
  outer(ls_ctrlFD, paste0) %>% 
  as.vector()

ls_mod <- c(
  glue("plm({ls_eqECM}, model = 'pooling', data = df_dcpoMeanAPSR)"),
  glue("plm({ls_eqFD}, model = 'fd', index = 'country', data = df_dcpoMeanAPSR)")
)

df_result_dcpoMeanAPSR <- map(ls_mod, function(mod) {
  result <- eval(parse(text = mod))
})

result_dcpoMeanAPSR <- map(df_result_dcpoMeanAPSR, function(aResult) {
  tidy_result <- tidy(aResult, conf.int = TRUE) %>% 
    mutate(std.error = vcovHC_se(aResult))
  
  glance_result <- glance.plm(aResult)
  
  ls_result <- list(tidy_result, glance_result)
})

names(result_dcpoMeanAPSR) <- c("pooled1", "pooled-regime1", "pooled2", "pooled-regime2",  "fd1", "fd-regime1", "fd2", "fd-regime2")

saveRDS(result_dcpoMeanAPSR, file = here::here("output", "estimates_dcpoMeanAPSR.RDS"))


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


## ----dcpoUncertaintyAPSR---------------------------
## The same as in the previous chunk

ls_ivECM <- c("diff(Vdem_libdem_z, lag = 1) + plm::lag(Vdem_libdem_z, 1)", 
              "diff(Vdem_polyarchy_z, lag = 1) + plm::lag(Vdem_polyarchy_z, 1) + diff(Vdem_liberal_z, lag = 1) + plm::lag(Vdem_liberal_z, 1)")
ls_ctrlECM <- c("", " + diff(corruption_z, lag = 1) + plm::lag(corruption_z, 1)")

ls_eqECM <- glue("diff(theta, lag = 1) ~ plm::lag(theta, 1:2) + {ls_ivECM} + diff(lg_imp_mdpgdp, lag = 1) + plm::lag(lg_imp_mdpgdp, 1)") %>% 
  outer(ls_ctrlECM, paste0) %>% 
  as.vector()

ls_ivFD <- c("Vdem_libdem_z",
             "Vdem_polyarchy_z + Vdem_liberal_z")

ls_ctrlFD <- c("", " + corruption_z")

ls_eqFD <- glue("theta ~ {ls_ivFD} + lg_imp_mdpgdp") %>% 
  outer(ls_ctrlFD, paste0) %>% 
  as.vector()

### Different datasets

ls_mod <- c(
  glue("plm({ls_eqECM}, model = 'pooling', data = aData)"),
  glue("plm({ls_eqFD}, model = 'fd', index = 'country', data = aData)")
)

result_dcpoUncertainAPSR <- map(ls_mod, function(aMod){ # reading a model
  result <- map(df_dcpoUncertainAPSR, function(aData){ # applying to the 1000 datasets
    eval(parse(text = aMod))
  })
  
  result_betas <- mitools::MIextract(result, fun = coef)
  
  result_vars <- mitools::MIextract(result, fun = vcovHC_se)
  
  tidy_dcpoUncertainAPSR <- summary(mitools::MIcombine(results = result_betas, 
                             variance = result_vars)) %>% 
    rownames_to_column(var = "term") %>%
        rename(conf.low = `(lower`,
               conf.high = `upper)`,
               estimate = results) %>%
        filter(!str_detect(term, "(Intercept)"))
  
  glance_dcpoUncertainAPSR <- glance.plm(result[[1]])
  
  result_dcpoUncertainAPSR <- list(tidy_dcpoUncertainAPSR, glance_dcpoUncertainAPSR)
})

names(result_dcpoUncertainAPSR) <- c("pooled1", "pooled-regime1", "pooled2", "pooled-regime2",  "fd1", "fd-regime1", "fd2", "fd-regime2")

saveRDS(result_dcpoUncertainAPSR, file = here::here("output", "estimates_dcpoAPSR.RDS"))


## ----visualizedAPSR, fig.cap= "The Effect of Democracy on Public Support", eval = TRUE, fig.width=7, fig.height=10----

result_clsAPSR <- readRDS(here("output", "estimates_clsMeanAPSR.RDS"))
result_clsUncertainAPSR <- readRDS(here("output", "estimates_clsAPSR.RDS"))
result_dcpoAPSR <- readRDS(here("output", "estimates_dcpoAPSR.RDS"))

# Regime compacted ####

result_ecm1 <- bind_rows(
  mutate(result_clsAPSR$pooled1[[1]], model = "Claassen 2020a"),
  mutate(result_clsUncertainAPSR$pooled1[[1]], model = "Claassen w/ Uncertainty"),
  mutate(result_dcpoAPSR$pooled1[[1]], model = "DCPO")
) %>% 
  mutate(type = "ECM")

result_fd1 <- bind_rows(
  mutate(result_clsAPSR$fd1[[1]], model = "Claassen 2020a"),
  mutate(result_clsUncertainAPSR$fd1[[1]], model = "Claassen w/ Uncertainty"),
  mutate(result_dcpoAPSR$fd1[[1]], model = "DCPO")
) %>% 
  mutate(type = "First Difference")

result_APSR1 <- bind_rows(result_ecm1, result_fd1) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = c(rep(
      c("Democratic Mood\n (t-1)",
        "Democratic Mood\n (t-2)",
      "Liberal Democracy\n (Difference)",
      "Liberal Democracy\n (t-1)",
      "Log GDP\n per capita\n (Difference)",
      "Log GDP\n (t-1)"),
    time = 3),
    rep(
      c("Liberal Democracy\n (Difference)", 
        "Log GDP\n per capita\n (Difference)"), 
      time = 3
    )),
    type = factor(type, levels = c("ECM", "First Difference")),
    model = factor(model, levels = c("Claassen 2020a", "Claassen w/ Uncertainty", "DCPO"))
  )

terms1 <- factor(rev(c("Support[t-1]",
                      "Support[t-2]",
                      "Delta~Liberal~democracy",
                      "Liberal~democracy[t-1]",
                      "Delta~Log~GDP~per~capita",
                      "Log~GDP[t-1]")),
                levels = rev(c("Support[t-1]",
                               "Support[t-2]",
                               "Delta~Liberal~democracy",
                               "Liberal~democracy[t-1]",
                               "Delta~Log~GDP~per~capita",
                               "Log~GDP[t-1]")))

plot_APSR1 <- dwplot(result_APSR1, dodge_size = 0.8, by_2sd = FALSE) +
  scale_y_discrete(labels = parse(text = levels(terms1))) +
  scale_color_grey(start = 0.4, end = 0.8, guide = guide_legend(reverse = TRUE)) +
  # scale_color_uiowa(name = "", palette = "digitMixed") +
  labs(
    title = "DV: Public Support for Democracy",
    subtitle = "Regime Compacted"
  ) +
  facet_wrap(~type, scales = "free_x") + 
  theme(legend.position = "none")

 
result_ecm2 <- bind_rows(
  mutate(result_clsAPSR$pooled2[[1]], model = "Claassen 2020a"),
  mutate(result_clsUncertainAPSR$pooled2[[1]], model = "Claassen w/ Uncertainty"),
  mutate(result_dcpoAPSR$pooled2[[1]], model = "DCPO")
) %>% 
  mutate(type = "ECM")

result_fd2 <- bind_rows(
  mutate(result_clsAPSR$fd2[[1]], model = "Claassen 2020a"),
  mutate(result_clsUncertainAPSR$fd2[[1]], model = "Claassen w/ Uncertainty"),
  mutate(result_dcpoAPSR$fd2[[1]], model = "DCPO")
) %>% 
  mutate(type = "First Difference")

result_APSR2 <- bind_rows(result_ecm2, result_fd2) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = c(rep(
      c("Democratic Mood\n (t-1)",
        "Democratic Mood\n (t-2)",
      "Liberal Democracy\n (Difference)",
      "Liberal Democracy\n (t-1)",
      "Log GDP\n per capita\n (Difference)",
      "Log GDP\n (t-1)",
      "Corruption\n (Difference)",
      "Corruption\n (t-1)"
      ),
    time = 3),
    rep(
      c("Liberal Democracy\n (Difference)", 
        "Log GDP\n per capita\n (Difference)",
        "Corruption\n (Difference)"), 
      time = 3
    )),
    type = factor(type, levels = c("ECM", "First Difference")),
    model = factor(model, levels = c("Claassen 2020a", "Claassen w/ Uncertainty", "DCPO"))
  )

terms2 <- factor(rev(c("Support[t-1]",
                      "Support[t-2]",
                      "Delta~Liberal~democracy",
                      "Liberal~democracy[t-1]",
                      "Delta~Log~GDP~per~capita",
                      "Log~GDP[t-1]",
                      "Delta~Corruption",
                      "Corruption[t-1]")),
                levels = rev(c("Support[t-1]",
                               "Support[t-2]",
                               "Delta~Liberal~democracy",
                               "Liberal~democracy[t-1]",
                               "Delta~Log~GDP~per~capita",
                               "Log~GDP[t-1]",
                               "Delta~Corruption",
                               "Corruption[t-1]")))

plot_APSR2 <- dwplot(result_APSR2, dodge_size = 0.8, by_2sd = FALSE) +
  scale_y_discrete(labels = parse(text = levels(terms2))) +
  scale_color_grey(start = 0.4, end = 0.8, guide = guide_legend(reverse = TRUE)) +
  # scale_color_uiowa(name = "", palette = "digitMixed") +
  labs(
    title = "",
    subtitle = "W. Corruption"
  ) +
  facet_wrap(~type, scales = "free_x") +
  theme(legend.position = "none")

# Regime specified ####

result_ecmReg1 <- bind_rows(
  mutate(result_clsAPSR$`pooled-regime1`[[1]], model = "Claassen 2020a"),
  mutate(result_clsUncertainAPSR$`pooled-regime1`[[1]], model = "Claassen w/ Uncertainty"),
  mutate(result_dcpoAPSR$`pooled-regime1`[[1]], model = "DCPO")
) %>% 
  mutate(type = "ECM")

result_fdReg1 <- bind_rows(
  mutate(result_clsAPSR$`fd-regime1`[[1]], model = "Claassen 2020a"),
  mutate(result_clsUncertainAPSR$`fd-regime1`[[1]], model = "Claassen w/ Uncertainty"),
  mutate(result_dcpoAPSR$`fd-regime1`[[1]], model = "DCPO")
) %>% 
  mutate(type = "First Difference")

result_RegAPSR1 <- bind_rows(result_ecmReg1, result_fdReg1) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = c(rep(
      c("Democratic Mood\n (t-1)",
        "Democratic Mood\n (t-2)",
      "Electoral Democracy\n (Difference)",
      "Electoral Democracy\n (t-1)",
      "Minoritarian Democracy\n (Difference)",
      "Minoritarian Democracy\n (t-1)",
      "Log GDP\n per capita\n (Difference)",
      "Log GDP\n (t-1)"),
    time = 3),
    rep(
      c("Electoral Democracy\n (Difference)", 
        "Minoritarian Democracy\n (Difference)",
        "Log GDP\n per capita\n (Difference)"), 
      time = 3
    )),
    type = factor(type, levels = c("ECM", "First Difference")),
    model = factor(model, levels = c("Claassen 2020a", "Claassen w/ Uncertainty", "DCPO"))
  )

terms3 <- factor(rev(c("Support[t-1]",
                      "Support[t-2]",
                      "Delta~Electoral~democracy",
                      "Electoral~democracy[t-1]",
                      "Delta~Minoritarian~democracy",
                      "Minoritarian~democracy[t-1]",
                      "Delta~Log~GDP~per~capita",
                      "Log~GDP[t-1]")),
                levels = rev(c("Support[t-1]",
                      "Support[t-2]",
                      "Delta~Electoral~democracy",
                      "Electoral~democracy[t-1]",
                      "Delta~Minoritarian~democracy",
                      "Minoritarian~democracy[t-1]",
                      "Delta~Log~GDP~per~capita",
                      "Log~GDP[t-1]")))

plot_RegAPSR1 <- dwplot(result_RegAPSR1, dodge_size = 0.8, by_2sd = FALSE) +
  scale_y_discrete(labels = parse(text = levels(terms3))) +
  scale_color_grey(start = 0.4, end = 0.8, guide = guide_legend(reverse = TRUE)) +
  # scale_color_uiowa(name = "", palette = "digitMixed") +
  labs(
    title = "",
    subtitle = "Regime Specified"
  ) +
  facet_wrap(~type, scales = "free_x") + 
  theme(legend.position = "none")

 
result_ecmReg2 <- bind_rows(
  mutate(result_clsAPSR$`pooled-regime2`[[1]], model = "Claassen 2020a"),
  mutate(result_clsUncertainAPSR$`pooled-regime2`[[1]], model = "Claassen w/ Uncertainty"),
  mutate(result_dcpoAPSR$`pooled-regime2`[[1]], model = "DCPO")
) %>% 
  mutate(type = "ECM")

result_fdReg2 <- bind_rows(
  mutate(result_clsAPSR$`fd-regime2`[[1]], model = "Claassen 2020a"),
  mutate(result_clsUncertainAPSR$`fd-regime2`[[1]], model = "Claassen w/ Uncertainty"),
  mutate(result_dcpoAPSR$`fd-regime2`[[1]], model = "DCPO")
) %>% 
  mutate(type = "First Difference")

result_RegAPSR2 <- bind_rows(result_ecmReg2, result_fdReg2) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = c(rep(
      c("Democratic Mood\n (t-1)",
        "Democratic Mood\n (t-2)",
      "Electoral Democracy\n (Difference)",
      "Electoral Democracy\n (t-1)",
      "Minoritarian Democracy\n (Difference)",
      "Minoritarian Democracy\n (t-1)",
      "Log GDP\n per capita\n (Difference)",
      "Log GDP\n (t-1)",
      "Corruption\n (Difference)",
      "Corruption\n (t-1)"
      ),
    time = 3),
    rep(
      c("Electoral Democracy\n (Difference)", 
        "Minoritarian Democracy\n (Difference)",
        "Log GDP\n per capita\n (Difference)",
        "Corruption\n (Difference)"), 
      time = 3
    )),
    type = factor(type, levels = c("ECM", "First Difference")),
    model = factor(model, levels = c("Claassen 2020a", "Claassen w/ Uncertainty", "DCPO"))
  )

terms4 <- factor(rev(c("Support[t-1]",
                      "Support[t-2]",
                      "Delta~Electoral~democracy",
                      "Electoral~democracy[t-1]",
                      "Delta~Minoritarian~democracy",
                      "Minoritarian~democracy[t-1]",
                      "Delta~Log~GDP",
                      "Log~GDP[t-1]",
                      "Delta~Corruption",
                      "Corruption[t-1]")),
                levels = rev(c("Support[t-1]",
                               "Support[t-2]",
                               "Delta~Electoral~democracy",
                               "Electoral~democracy[t-1]",
                               "Delta~Minoritarian~democracy",
                               "Minoritarian~democracy[t-1]",
                               "Delta~Log~GDP",
                               "Log~GDP[t-1]",
                               "Delta~Corruption",
                               "Corruption[t-1]")))

plot_RegAPSR2 <- dwplot(result_RegAPSR2, dodge_size = 0.8, by_2sd = FALSE) +
  scale_y_discrete(labels = parse(text = levels(terms4))) +
  scale_color_grey(start = 0.4, end = 0.8, guide = guide_legend(reverse = TRUE)) +
  # scale_color_uiowa(name = "", palette = "digitMixed") +
  labs(
    title = "",
    subtitle = "Regime Specified w. Corruption"
  ) +
  facet_wrap(~type, scales = "free_x") +
  theme(legend.position = "bottom", 
        legend.title = element_blank())

legend <- get_legend(plot_RegAPSR2)

plot_RegAPSR2 <- plot_RegAPSR2 + theme(legend.position = "none")

grid.arrange(plot_APSR1, plot_APSR2, plot_RegAPSR1, plot_RegAPSR2, legend, ncol = 1)


## ----visualizedUncertaintyAPSR, fig.cap= "The Effect of Democratic Mood on Institutional Democratization based on DCPO"----

load(here::here("output", "estimates_dcpoAPSR.RDS"))

df_plot <- df_result_dcpoMeanAPSR[[1]] %>% 
  mutate(model = "DCPO\n (Point Estimates)") 
 
df_plot <- df_result_dcpoAPSR[[1]] %>%
  mutate(model = "DCPO\n (w. Uncertainty)") %>%
  rename(std.error = se) %>%
  bind_rows(df_plot)
 
plot_pooled <- filter(df_plot, model != "Claassen 2019") %>% 
  dwplot(by_2sd = FALSE) %>%
  relabel_predictors(
    c(
      `plm::lag(theta, 1:2)1` = "Democracy Mood(t-1)",
      `plm::lag(theta, 1:2)2` = "Democracy Mood(t-2)",
      `diff(Vdem_libdem_z, lag = 1)` = "Liberal Democracy (Diff)",
      `plm::lag(Vdem_libdem_z, 1)` = "Liberal Democracy (t-1)",
      `diff(lg_imp_mdpgdp, lag = 1)` = "Log GDP per capita(Diff)", 
      `plm::lag(lg_imp_mdpgdp, 1)` = "Log GDP per capita(t-1)",
      `plm::lag(mdprgdp_grwth, 1)` = "Corruption (Difference)"
    )
  ) +
  geom_vline(xintercept = 0,
             colour = "grey80",
             linetype = 2) +
  theme_minimal() +
  ggtitle("DV: Liberal Democracy (Vdem)") +
  xlab("Coefficient Estimate") +
  theme(
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )

df_plot <- df_result_dcpoMeanAPSR[[2]] %>% 
  mutate(model = "DCPO\n (Point Estimates)")

df_plot <- df_result_dcpoAPSR[[2]] %>%
  mutate(model = "DCPO\n (w. Uncertainty)") %>%
  rename(std.error = se) %>%
  bind_rows(df_plot)
 
plot_pooled2 <- filter(df_plot, model != "Claassen 2019") %>% 
  dwplot(by_2sd = FALSE) %>%
  relabel_predictors(
    c(
      `plm::lag(theta, 1:2)1` = "Democracy Mood(t-1)",
      `plm::lag(theta, 1:2)2` = "Democracy Mood(t-2)",
      `diff(Vdem_libdem_z, lag = 1)` = "Liberal Democracy (Diff)",
      `plm::lag(Vdem_libdem_z, 1)` = "Liberal Democracy (t-1)",
      `diff(lg_imp_mdpgdp, lag = 1)` = "Log GDP per capita(Diff)", 
      `plm::lag(lg_imp_mdpgdp, 1)` = "Log GDP per capita(t-1)",
      `diff(cpi_sd, lag = 1)` = "Corruption (Difference)",
      `plm::lag(cpi_sd, 1)` = "Corruption (t-1)"
    )
  ) +
  geom_vline(xintercept = 0,
             colour = "grey80",
             linetype = 2) +
  theme_minimal() +
  ggtitle("DV: Liberal Democracy (Vdem)") +
  xlab("Coefficient Estimate") +
  theme(
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )

gridExtra::grid.arrange(plot_pooled,plot_pooled2, ncol = 2)


## ----num-cls2020a, results='asis', eval = TRUE-----
tb_result_cls <- map(result_cls, ~ {
  names(.) <- c("tidy", "glance")
  class(.) <- "modelsummary_list"
  return(.)})

names(tb_result_cls) <- c("Pooled", "Pooled-Regime", 
                          "GMM", "GMM-Regime")

names_coef <- c(
  "(Intercept)" = "(Intercept)",
  "plm::lag(Libdem_VD, 1:2)1" = "Democracy[t-1]",
  "plm::lag(Libdem_VD, 1:2)2"  = "Democracy[t-2]",
  "plm::lag(SupDem_trim, 1)" = "Support[t-1]",
  "plm::lag(SupDem_Democ, 1)" = "Support in Democracy",
  "plm::lag(SupDem_Autoc, 1)" = "Support in Autocracy",
  "plm::lag(lnGDP_imp, 1)" = "Log~GDP~per~capita[t-1]",
  "plm::lag(GDP_imp_grth, 1)"  = "GDP~per~capita~growth[t-1]",
  "plm::lag(Libdem_regUN, 1)" = "Regional~democracy[t-1]",
  "plm::lag(Pr_Muslim, 1)" = "Percent~Muslim[t-1]",
  "plm::lag(Res_cp_WDI_di, 1)" = "Resource~dependence[t-1]"
)

modelsummary(tb_result_cls,  
             statistic = "conf.int",
             coef_map = names_coef, 
             output = "latex", 
             title = "Original") %>% 
  
  kableExtra::kable_styling(font_size = 8) %>% 
  kableExtra::kable_styling(latex_options = "HOLD_position")


## ----num-cls2020aUncertain, results='asis', eval = TRUE----
tb_result_clsUncertain <- map(result_clsUncertain, ~ {
  names(.) <- c("tidy", "glance")
  class(.) <- "modelsummary_list"
  return(.)})

names(tb_result_cls) <- c("Pooled", "Pooled-Regime", 
                          "GMM", "GMM-Regime")

names_coef <- c(
  "plm::lag(Libdem_VD, 1:2)1" = "Democracy[t-1]",
  "plm::lag(Libdem_VD, 1:2)2"  = "Democracy[t-2]",
  "plm::lag(theta, 1)" = "Support[t-1]",
  "plm::lag(theta_dem, 1)" = "Support in Democracy",
  "plm::lag(theta_aut, 1)" = "Support in Autocracy",
  "plm::lag(lnGDP_imp, 1)" = "Log~GDP~per~capita[t-1]",
  "plm::lag(GDP_imp_grth, 1)"  = "GDP~per~capita~growth[t-1]",
  "plm::lag(Libdem_regUN, 1)" = "Regional~democracy[t-1]",
  "plm::lag(Pr_Muslim, 1)" = "Percent~Muslim[t-1]",
  "plm::lag(Res_cp_WDI_di, 1)" = "Resource~dependence[t-1]"
)

modelsummary(tb_result_clsUncertain, 
             statistic = "conf.int",
             coef_map = names_coef, 
             output = "latex", 
             title = "W. Uncertainty") %>% 
  kableExtra::kable_styling(latex_options = "HOLD_position")


## ----num-cls2020aDCPO, results='asis', eval = TRUE----
tb_result_DCPO <- map(result_dcpoUncertain, ~ {
  names(.) <- c("tidy", "glance")
  class(.) <- "modelsummary_list"
  return(.)})

names(tb_result_DCPO) <- c("Pooled", "Pooled-Regime", 
                          "GMM", "GMM-Regime")

names_coef <- c(
  "plm::lag(Vdem_libdem, 1:2)1" = "Democracy[t-1]",
  "plm::lag(Vdem_libdem, 1:2)2"  = "Democracy[t-2]",
  "plm::lag(theta, 1)" = "Support[t-1]",
  "plm::lag(theta_dem, 1)" = "Support in Democracy",
  "plm::lag(theta_aut, 1)" = "Support in Autocracy",
  "plm::lag(lg_imp_mdpgdp, 1)" = "Log~GDP~per~capita[t-1]",
  "plm::lag(mdprgdp_grwth, 1)"  = "GDP~per~capita~growth[t-1]",
  "plm::lag(Region_libdem, 1)" = "Regional~democracy[t-1]",
  "plm::lag(muslism_prop_2010, 1)" = "Percent~Muslim[t-1]",
  "plm::lag(dependence_pc_di, 1)" = "Resource~dependence[t-1]"
)

modelsummary(tb_result_DCPO, 
             statistic = "conf.int",
             coef_map = names_coef, 
             output = "latex", 
             title = "W. Uncertainty") %>% 
  kableExtra::kable_styling(latex_options = "HOLD_position")


## ----num-cls2020b, results='asis', eval = TRUE-----
tb_result_clsAPSR <- map(result_clsAPSR, ~ {
  names(.) <- c("tidy", "glance")
  class(.) <- "modelsummary_list"
  return(.)}) %>% 
  .[c("pooled1", "pooled-regime1", "fd1", "fd-regime1",
      "pooled2", "pooled-regime2", "fd2", "fd-regime2")]

names(tb_result_clsAPSR) <- c("ECM", "ECM-Regime", "FD", "FD-Regime","ECM Corrup", "ECM Corrup-Regime", "FD Corrup", "FD Corrup-Regime")

names_coef <- c(
  "(Intercept)" = "(Intercept)",
  "plm::lag(SupDem_trim, 1:2)1" = "Democratic\n Mood\n (t-1)",
  "plm::lag(SupDem_trim, 1:2)2" = "Democratic\n Mood\n (t-2)",
  "diff(Libdem_z, lag = 1)" = "Liberal\n Democracy\n (Difference)",
  "Libdem_z" = "Liberal\n Democracy\n (Difference)",
  "plm::lag(Libdem_z, 1)" = "Liberal\n Democracy\n (t-1)",
  "diff(Polyarchy_z, lag = 1)" = "Electoral\n Democracy\n (Difference)",
  "Polyarchy_z" = "Electoral\n Democracy\n (Difference)",
  "plm::lag(Polyarchy_z, 1)" = "Electoral\n Democracy\n (t-1)",
  "diff(Liberal_z, lag = 1)" = "Minoritarian\n Democracy\n (Difference)",
  "Liberal_z" = "Minoritarian\n Democracy\n (Difference)",
  "plm::lag(Liberal_z, 1)" = "Minoritarian\n Democracy\n (t-1)",
  "diff(lnGDP_imp, lag = 1)" = "Log GDP\n per capita\n (Difference)",
  "lnGDP_imp" = "Log GDP\n per capita\n (Difference)",
  "plm::lag(lnGDP_imp, 1)" = "Log GDP\n (t-1)",
  "diff(Corrup_TI_z, lag = 1)" = "Corruption\n (Difference)",
  "Corrup_TI_z" = "Corruption\n (Difference)",
  "plm::lag(Corrup_TI_z, 1)" = "Corruption\n (t-1)"
)

modelsummary(tb_result_clsAPSR, 
             stars = TRUE,
             coef_map = names_coef, 
             output = "latex") %>% 
  kableExtra::kable_styling(font_size = 7) %>% 
  kableExtra::kable_styling(latex_options = "HOLD_position")


## ----num-cls2020bUncertain, results='asis', eval = TRUE----
tb_result_clsUncertainAPSR <- map(result_clsUncertainAPSR, ~ {
  names(.) <- c("tidy", "glance")
  class(.) <- "modelsummary_list"
  return(.)}) %>% 
  .[c("pooled1", "pooled-regime1", "fd1", "fd-regime1",
      "pooled2", "pooled-regime2", "fd2", "fd-regime2")]

names(tb_result_clsUncertainAPSR) <- c("ECM", "ECM-Regime", "FD", "FD-Regime","ECM Corrup", "ECM Corrup-Regime", "FD Corrup", "FD Corrup-Regime")

names_coef <- c(
  "(Intercept)" = "(Intercept)",
  "plm::lag(SupDem_trim, 1:2)1" = "Democratic\n Mood\n (t-1)",
  "plm::lag(SupDem_trim, 1:2)2" = "Democratic\n Mood\n (t-2)",
  "diff(Libdem_z, lag = 1)" = "Liberal\n Democracy\n (Difference)",
  "Libdem_z" = "Liberal\n Democracy\n (Difference)",
  "plm::lag(Libdem_z, 1)" = "Liberal\n Democracy\n (t-1)",
  "diff(Polyarchy_z, lag = 1)" = "Electoral\n Democracy\n (Difference)",
  "Polyarchy_z" = "Electoral\n Democracy\n (Difference)",
  "plm::lag(Polyarchy_z, 1)" = "Electoral\n Democracy\n (t-1)",
  "diff(Liberal_z, lag = 1)" = "Minoritarian\n Democracy\n (Difference)",
  "Liberal_z" = "Minoritarian\n Democracy\n (Difference)",
  "plm::lag(Liberal_z, 1)" = "Minoritarian\n Democracy\n (t-1)",
  "diff(lnGDP_imp, lag = 1)" = "Log GDP\n per capita\n (Difference)",
  "lnGDP_imp" = "Log GDP\n per capita\n (Difference)",
  "plm::lag(lnGDP_imp, 1)" = "Log GDP\n (t-1)",
  "diff(Corrup_TI_z, lag = 1)" = "Corruption\n (Difference)",
  "Corrup_TI_z" = "Corruption\n (Difference)",
  "plm::lag(Corrup_TI_z, 1)" = "Corruption\n (t-1)"
)

modelsummary(tb_result_clsUncertainAPSR, 
             statistic = "conf.int",
             coef_map = names_coef, 
             output = "latex") %>% 
  kableExtra::kable_styling(font_size = 7) %>% 
  kableExtra::kable_styling(latex_options = "HOLD_position")


## ----num-cls2020bDCPO, results='asis', eval = TRUE----
tb_result_dcpoAPSR <- map(result_dcpoAPSR, ~ {
  names(.) <- c("tidy", "glance")
  class(.) <- "modelsummary_list"
  return(.)}) %>% 
  .[c("pooled1", "pooled-regime1", "fd1", "fd-regime1",
      "pooled2", "pooled-regime2", "fd2", "fd-regime2")]

names(tb_result_dcpoAPSR) <- c("ECM", "ECM-Regime", "FD", "FD-Regime","ECM Corrup", "ECM Corrup-Regime", "FD Corrup", "FD Corrup-Regime")

names_coef <- c(
  "(Intercept)" = "(Intercept)",
  "plm::lag(theta, 1:2)1" = "Democratic\n Mood\n (t-1)",
  "plm::lag(theta, 1:2)2" = "Democratic\n Mood\n (t-2)",
  
  "diff(Vdem_libdem_z, lag = 1)" = "Liberal\n Democracy\n (Difference)",
  "Vdem_libdem_z" = "Liberal\n Democracy\n (Difference)",
  "plm::lag(Vdem_libdem_z, 1)" = "Liberal\n Democracy\n (t-1)",
  
  "diff(Vdem_polyarchy_z, lag = 1)" = "Electoral\n Democracy\n (Difference)",
  "Vdem_polyarchy_z" = "Electoral\n Democracy\n (Difference)",
  "plm::lag(Vdem_polyarchy_z, 1)" = "Electoral\n Democracy\n (t-1)",
  "diff(Vdem_liberal_z, lag = 1)" = "Minoritarian\n Democracy\n (Difference)",
  "Vdem_liberal_z" = "Minoritarian\n Democracy\n (Difference)",
  "plm::lag(Vdem_liberal_z, 1)" = "Minoritarian\n Democracy\n (t-1)",
  "diff(lg_imp_mdpgdp, lag = 1)" = "Log GDP\n per capita\n (Difference)",
  "lg_imp_mdpgdp" = "Log GDP\n per capita\n (Difference)",
  "plm::lag(lg_imp_mdpgdp, 1)" = "Log GDP\n (t-1)",
  "diff(corruption_z, lag = 1)" = "Corruption\n (Difference)",
  "corruption_z" = "Corruption\n (Difference)",
  "plm::lag(corruption_z, 1)" = "Corruption\n (t-1)"
)

modelsummary(tb_result_dcpoAPSR, 
             statistic = "conf.int",
             coef_map = names_coef, 
             output = "latex") %>% 
  kableExtra::kable_styling(font_size = 7) %>% 
  kableExtra::kable_styling(latex_options = "HOLD_position")


## ----datawitherrors--------------------------------

ls_country <- unique(claassen_replication$country)
first_year <- min(claassen_replication$year)

df_cls_errors <- purrr::map(1:900, function(anEntry) {
  claassen_m5_theta$theta[anEntry,,] %>%
    reformat_dcpo_output("theta")  %>%
    mutate(iter = anEntry) 
  }
  )


cls_theta_uncertainty <- bind_rows(df_cls_errors)
saveRDS(cls_theta_uncertainty,"data/cls_theta_uncertainty.rds")


ls_country <- levels(dcpo_input$data$kk)
first_year <- min(dcpo_input$data$year)

df_dcpo <- purrr::map_df(1:900, function(anEntry) {
  dcpo_theta$theta[anEntry,,] %>%
    reformat_dcpo_output("theta") %>%
    mutate(iter = anEntry) %>%
    left_join(reformat_dcpo_output(dcpo_sigma$sigma[anEntry,,], "sigma"),
              by = c("year", "country"))
}) 

saveRDS(df_dcpo,"dcpo_theta_uncertainty.rds")

cntrl_ajps_uncertainty <- readRDS("data/cntrl_ajps_uncertainty.rds")

dcpo_analysis <- left_join(df_dcpo,cntrl_ajps_uncertainty, by=c("country","year","iter"="iter_se") )
saveRDS(dcpo_analysis, "data/dcpo_analysis_ajps.rds")

dcpo_apsr_cntrl <- readRDS("data/cntrl_apsr_uncertainty.rds")
dcpo_apsr_analysis <- left_join(df_dcpo,dcpo_apsr_cntrl, by=c("country","year","iter"="iter_se") )
saveRDS(dcpo_apsr_analysis, "data/dcpo_analysis_apsr.rds")
qsave(dcpo_apsr_analysis, "data/dcpo_analysis_apsr_compress")




cls_ajps_uncertainty <- readRDS("data/cls_ajps_uncertainty.rds")


cls_analysis <- left_join(cls_theta_uncertainty,cls_ajps_uncertainty, by=c("country","year"="Year","iter"="iter_se") )
saveRDS(cls_analysis, "data/cls_analysis_ajps.rds")






## ----claassenFullUncertaintyAJPS-------------------

load(here("data", "cls_ajps.rda")) # produced by cls_ajps_apsr_uncertain.R

ls_iv <- c("plm::lag(theta, 1)", 
    "plm::lag(theta_dem, 1) + plm::lag(theta_aut, 1) ") 

ls_method <- c("plm", "pgmm")

ls_eq <- glue("Libdem_VD ~ plm::lag(Libdem_VD, 1:2) + {ls_iv} + plm::lag(lnGDP_imp, 1) + plm::lag(GDP_imp_grth, 1) + plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1) + plm::lag(Res_cp_WDI_di, 1)") 

ls_mod <- c(
    glue("plm({ls_eq}, model = 'pooling', data = aData)"),
    glue(
        "pgmm({ls_eq} | plm::lag(Libdem_VD, 3:5), data = aData, effect='individual', model='onestep', transformation='ld', indexes=c('country', 'year'))"
    )
)

result_clsUncertainFull <- map(ls_mod, function(aMod){ # reading a model
  result <- map(cls_ajps, function(aData){ # applying to the 1000 datasets
    eval(parse(text = aMod))
  })
  
  result_betas <- mitools::MIextract(result, fun = coef)
  
  ## Not work with ifelse
  if(class(result[[1]])[1] == "plm"){
      result_vars <- mitools::MIextract(result, fun = vcovBK_se)
  }else{
      result_vars <- mitools::MIextract(result, fun = vcovHC_se)
  }
  
  
  tidy_clsUncertain <- summary(mitools::MIcombine(results = result_betas, 
                             variance = result_vars)) %>% 
    rownames_to_column(var = "term") %>%
        rename(conf.low = `(lower`,
               conf.high = `upper)`,
               estimate = results) %>%
        filter(!str_detect(term, "(Intercept)"))
  
  if(class(result[[1]]) == "plm"){ 
    glance_clsUncertain <- glance.plm(result[[1]]) # using the customized glance function
  }else{
    glance_clsUncertain <- glance(result[[1]])
  }
  
  result_clsUncertain <- list(tidy_clsUncertain, glance_clsUncertain)
})


names(result_clsUncertainFull) <- c("pooled", "pooled-regime", "gmm", "gmm-regime")
   
saveRDS(result_clsUncertainFull, file = here::here("output", "estimates_clsAJPSfullUncertain.RDS"))


## ----dcpoFullUncertaintyAJPS-----------------------
load(here("data", "dcpo_ajps.rda")) # produced by dcpo_ajps_apsr_uncertain.R

ls_iv <- c("plm::lag(theta, 1)", 
    "plm::lag(theta_dem, 1) + plm::lag(theta_aut, 1) ") 

ls_method <- c("plm", "pgmm")

ls_eq <- glue("Vdem_libdem ~ plm::lag(Vdem_libdem, 1:2) + {ls_iv} + plm::lag(lg_imp_mdpgdp, 1) + plm::lag(mdprgdp_grwth, 1) + plm::lag(Region_libdem, 1) + plm::lag(muslism_prop_2010, 1) + plm::lag(dependence_pc_di, 1)") 

ls_mod <- c(
    glue("plm({ls_eq}, model = 'pooling', data = aData)"),
    glue(
        "pgmm({ls_eq} | plm::lag(Vdem_libdem, 3:5), data = aData, effect='individual', model='onestep', transformation='ld', indexes=c('country', 'year'))"
    )
)

result_dcpoUncertainFull <- map(ls_mod, function(aMod){ # reading a model
  result <- map(dcpo_ajps, function(aData){ # applying to the 1000 datasets
    eval(parse(text = aMod))
  })
  
  result_betas <- mitools::MIextract(result, fun = coef)
  
  ## Not work with ifelse
  if(class(result[[1]])[1] == "plm"){
      result_vars <- mitools::MIextract(result, fun = vcovBK_se)
  }else{
      result_vars <- mitools::MIextract(result, fun = vcovHC_se)
  }
  
  
  tidy_dcpoUncertain <- summary(mitools::MIcombine(results = result_betas, 
                             variance = result_vars)) %>% 
    rownames_to_column(var = "term") %>%
        rename(conf.low = `(lower`,
               conf.high = `upper)`,
               estimate = results) %>%
        filter(!str_detect(term, "(Intercept)"))
  
  if(class(result[[1]]) == "plm"){ 
    glance_dcpoUncertain <- glance.plm(result[[1]]) # using the customized glance function
  }else{
    glance_dcpoUncertain <- glance(result[[1]])
  }
  
  result_dcpoUncertain <- list(tidy_dcpoUncertain, glance_dcpoUncertain)
})

names(result_dcpoUncertainFull) <- c("pooled", "pooled-regime", "gmm", "gmm-regime")

saveRDS(result_dcpoUncertainFull, file = here::here("output", "estimates_dcpoAJPSfullUncertain.RDS"))


## ----visualizeFullUncertainAJPS, fig.cap= "The Effect of Public Support on Democracy", eval = TRUE, fig.width=7.5, fig.height=7----

# Very weird phenomenon that the same type of rds files needs different ways to read
# Suggesting using the qs or csv data type in future.
result_cls <- readRDS(here("output", "estimates_clsMeanAJPS.RDS"))
result_clsUncertainFull <- readRDS(here("output", "estimates_clsAJPSfullUncertain.RDS"))
result_dcpoUncertainFull <- readRDS(here("output", "estimates_dcpoAJPSfullUncertain.RDS"))

result_pooled <- bind_rows(
  mutate(result_cls$pooled[[1]], model = "Claassen 2020a"),
  mutate(result_clsUncertainFull$pooled[[1]], model = "Claassen w/ Uncertainty"),
  mutate(result_dcpoUncertainFull$pooled[[1]], model = "DCPO")
) %>% 
  mutate(type = "Pooled")

result_gmm <- bind_rows(
  mutate(result_cls$gmm[[1]], model = "Claassen 2020a"),
  mutate(result_clsUncertainFull$gmm[[1]], model = "Claassen w/ Uncertainty"),
  mutate(result_dcpoUncertainFull$gmm[[1]], model = "DCPO")
) %>% 
  mutate(type = "GMM")

result_pooledAJPS <- bind_rows(result_pooled, result_gmm) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = rep(
      c("Democracy\n (t-1)",
        "Democracy\n (t-2)",
      "Support\n (t-1)",
      "Log GDP\n per capita\n (t-1)",
      "GDP per\n capita growth\n (t-1)",
      "Regional\n democracy\n (t-1)",
      "Percent\n Muslim\n (t-1)",
      "Resource\n dependence\n (t-1)"),
    time = 6), 
    type = factor(type, levels = c("Pooled", "GMM")),
    model = factor(model, levels = c("Claassen 2020a", "Claassen w/ Uncertainty", "DCPO")))

terms <- factor(rev(c("Democracy[t-1]",
        "Democracy[t-2]",
      "Support[t-1]",
      "Log~GDP~per~capita[t-1]",
      "GDP~per~capita~growth[t-1]",
      "Regional~democracy[t-1]",
      "Percent~Muslim[t-1]",
      "Resource~dependence[t-1]")), levels =
      rev(c("Democracy[t-1]",
        "Democracy[t-2]",
      "Support[t-1]",
      "Log~GDP~per~capita[t-1]",
      "GDP~per~capita~growth[t-1]",
      "Regional~democracy[t-1]",
      "Percent~Muslim[t-1]",
      "Resource~dependence[t-1]")))

# ggplot produces reversed plot, and embarrassed to say, I can't fix, again. Any thought?
# What I've tried
# 1. using guide_legend(reverse = TRUE) will change the legend's order not the plot.
# 2. ordering models with factor levels using the original names or even c("1", "2", "3")
# 2. redefining the order of model in the dataframe or in dwplot 
# 3. checking the dwplot codes, model works in line 453, but nothing looks helpful
# 4. Every post online is to reverse 

plot_AJPS1 <- dwplot(result_pooledAJPS, dodge_size = 0.8, by_2sd = FALSE) +
  scale_y_discrete(labels = parse(text = levels(terms))) +
  scale_color_grey(start = 0.4, end = 0.8) +
  labs(
    title = "DV: V-Dem Liberal Democracy Index",
    subtitle = "Regime Compacted"
  ) +
  facet_wrap(~type, scales = "free_x") + 
  theme(legend.position = "none")

# the attemp to change the order of legends failed. Both factorize the model ahead or using the  guides(fill = guide_legend(reverse=TRUE)) fails
# The trick is  to use guide = guide_legend(reverse = TRUE) within scale_color_*(); see below in plotAJPS2. --FS

result_regime <- bind_rows(
  mutate(result_cls$`pooled-regime`[[1]], model = "Claassen 2020a"),
  mutate(result_clsUncertainFull$`pooled-regime`[[1]], model = "Claassen w/ Uncertainty"),
  mutate(result_dcpoUncertainFull$`pooled-regime`[[1]], model = "DCPO")
) %>% 
  mutate(type = "Pooled")

result_regimeGmm <- bind_rows(
  mutate(result_cls$`gmm-regime`[[1]], model = "Claassen 2020a"),
  mutate(result_clsUncertainFull$`gmm-regime`[[1]], model = "Claassen w/ Uncertainty"),
  mutate(result_dcpoUncertainFull$`gmm-regime`[[1]], model = "DCPO")
) %>% 
  mutate(type = "GMM")

result_regimeAJPS <- bind_rows(result_regime, result_regimeGmm) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = rep(
      c("Democracy\n (t-1)",
        "Democracy\n (t-2)",
      "Support\n demo only\n (t-1)",
      "Support\n auto only\n (t-1)",
      "Log GDP\n per capita\n (t-1)",
      "GDP per\n capita growth\n (t-1)",
      "Regional\n democracy\n (t-1)",
      "Percent\n Muslim\n (t-1)",
      "Resource\n dependence\n (t-1)"),
    time = 6), 
    type = factor(type, levels = c("Pooled", "GMM")),
    model = factor(model, levels = c("Claassen 2020a", "Claassen w/ Uncertainty", "DCPO")))

terms2 <- factor(rev(c("Democracy[t-1]",
                       "Democracy[t-2]",
                       "Support[t-1] (democracies~only)",
                       "Support[t-1] (autocracies~only)",
                       "Log~GDP~per~capita[t-1]",
                       "GDP~per~capita~growth[t-1]",
                       "Regional~democracy[t-1]",
                       "Percent~Muslim[t-1]",
                       "Resource~dependence[t-1]")), levels =
                   rev(c("Democracy[t-1]",
                         "Democracy[t-2]",
                         "Support[t-1] (democracies~only)",
                         "Support[t-1] (autocracies~only)",
                         "Log~GDP~per~capita[t-1]",
                         "GDP~per~capita~growth[t-1]",
                         "Regional~democracy[t-1]",
                         "Percent~Muslim[t-1]",
                         "Resource~dependence[t-1]")))

plot_AJPS2 <- dwplot(result_regimeAJPS, dodge_size = 0.8, by_2sd = FALSE) +
    scale_y_discrete(labels = parse(text = levels(terms2))) +
  scale_color_grey(start = 0.4, end = 0.8, guide = guide_legend(reverse = TRUE)) +
  labs(
    title = "",
    subtitle = "Regime Specific"
  ) +
  facet_wrap(~type, scales = "free_x") +
  theme(legend.position = "bottom", 
        legend.title = element_blank())

legend <- get_legend(plot_AJPS2)

plot_AJPS2 <- plot_AJPS2 + theme(legend.position = "none")

grid.arrange(plot_AJPS1, plot_AJPS2, legend, ncol = 1)



## ----claassenFullUncertaintyAPSR-------------------

load(here("data", "cls_apsr.rda")) # produced by cls_ajps_apsr_uncertain.R

ls_ivECM <- c("diff(Libdem_z, lag = 1) + plm::lag(Libdem_z, 1)", 
              "diff(Polyarchy_z, lag = 1) + plm::lag(Polyarchy_z, 1) + diff(Liberal_z, lag = 1) + plm::lag(Liberal_z, 1)")
ls_ctrlECM <- c("", " + diff(Corrup_TI_z, lag = 1) + plm::lag(Corrup_TI_z, 1)")

ls_eqECM <- glue("diff(theta, lag = 1) ~ plm::lag(theta, 1:2) + {ls_ivECM} + diff(lnGDP_imp, lag = 1) + plm::lag(lnGDP_imp, 1)") %>% 
  outer(ls_ctrlECM, paste0) %>% 
  as.vector()

ls_ivFD <- c("Libdem_z",
             "Polyarchy_z + Liberal_z")

ls_ctrlFD <- c("", " + Corrup_TI_z")

ls_eqFD <- glue("diff(theta, lag = 1) ~ {ls_ivFD} + lnGDP_imp") %>% 
  outer(ls_ctrlFD, paste0) %>% 
  as.vector()

ls_mod <- c(
  glue("plm({ls_eqECM}, model = 'pooling', data = aData)"),
  glue("plm({ls_eqFD}, model = 'fd', index = 'country', data = aData)")
)

result_clsFullUncertainAPSR <- map(ls_mod, function(aMod){ # reading a model
  result <- map(cls_apsr, function(aData){ # applying to the 1000 datasets
    eval(parse(text = aMod))
  })
  
  result_betas <- mitools::MIextract(result, fun = coef)
  
  result_vars <- mitools::MIextract(result, fun = vcovHC_se)
  
  tidy_clsUncertainAPSR <- summary(mitools::MIcombine(results = result_betas, 
                             variance = result_vars)) %>% 
    rownames_to_column(var = "term") %>%
        rename(conf.low = `(lower`,
               conf.high = `upper)`,
               estimate = results) %>%
        filter(!str_detect(term, "(Intercept)"))
  
  glance_clsUncertainAPSR <- glance.plm(result[[1]])
  
  result_clsUncertainAPSR <- list(tidy_clsUncertainAPSR, glance_clsUncertainAPSR)
})

names(result_clsFullUncertainAPSR) <- c("pooled1", "pooled-regime1", "pooled2", "pooled-regime2", "fd1", "fd-regime1", "fd2", "fd-regime2")

saveRDS(result_clsFullUncertainAPSR, file = here::here("output", "estimates_clsFullUncertainAPSR.RDS"))



## ----dcpoFullUncertaintyAPSR-----------------------
load(here("data", "dcpo_apsr.rda")) # produced by cls_ajps_apsr_uncertain.R

ls_ivECM <- c("diff(Vdem_libdem_z, lag = 1) + plm::lag(Vdem_libdem_z, 1)", 
              "diff(Vdem_polyarchy_z, lag = 1) + plm::lag(Vdem_polyarchy_z, 1) + diff(Vdem_liberal_z, lag = 1) + plm::lag(Vdem_liberal_z, 1)")
ls_ctrlECM <- c("", " + diff(corruption_z, lag = 1) + plm::lag(corruption_z, 1)")

ls_eqECM <- glue("diff(theta, lag = 1) ~ plm::lag(theta, 1:2) + {ls_ivECM} + diff(lg_imp_mdpgdp, lag = 1) + plm::lag(lg_imp_mdpgdp, 1)") %>% 
  outer(ls_ctrlECM, paste0) %>% 
  as.vector()

ls_ivFD <- c("Vdem_libdem_z",
             "Vdem_polyarchy_z + Vdem_liberal_z")

ls_ctrlFD <- c("", " + corruption_z")

ls_eqFD <- glue("theta ~ {ls_ivFD} + lg_imp_mdpgdp") %>% 
  outer(ls_ctrlFD, paste0) %>% 
  as.vector()

### Different datasets


###Duble check the dataset--------------
ls_mod <- c(
  glue("plm({ls_eqECM}, model = 'pooling', data = aData)"),
  glue("plm({ls_eqFD}, model = 'fd', index = 'country', data = aData)")
)

result_dcpoUncertainAPSR <- map(ls_mod, function(aMod){ # reading a model
  result <- map(dcpo_apsr, function(aData){ # applying to the 1000 datasets
    eval(parse(text = aMod))
  })
  
  result_betas <- mitools::MIextract(result, fun = coef)
  
  result_vars <- mitools::MIextract(result, fun = vcovHC_se)
  
  tidy_dcpoUncertainAPSR <- summary(mitools::MIcombine(results = result_betas, 
                             variance = result_vars)) %>% 
    rownames_to_column(var = "term") %>%
        rename(conf.low = `(lower`,
               conf.high = `upper)`,
               estimate = results) %>%
        filter(!str_detect(term, "(Intercept)"))
  
  glance_dcpoUncertainAPSR <- glance.plm(result[[1]])
  
  result_dcpoUncertainAPSR <- list(tidy_dcpoUncertainAPSR, glance_dcpoUncertainAPSR)
})

names(result_dcpoUncertainAPSR) <- c("pooled1", "pooled-regime1", "pooled2", "pooled-regime2",  "fd1", "fd-regime1", "fd2", "fd-regime2")

saveRDS(result_dcpoUncertainAPSR, file = here::here("output", "estimates_dcpoUncertainAPSR.RDS"))


## ----visualizedFullUncertainAPSR, fig.cap= "The Effect of Democracy on Public Support", eval = TRUE, fig.width=7, fig.height=10----

result_clsAPSR <- readRDS(here("output", "estimates_clsMeanAPSR.RDS"))
result_clsFullUncertainAPSR <- readRDS(here("output", "estimates_clsFullUncertainAPSR.RDS"))
result_dcpoFullUncertainAPSR <- readRDS(here("output", "estimates_dcpoUncertainAPSR.RDS"))

# Regime compacted ####

result_ecm1 <- bind_rows(
  mutate(result_clsAPSR$pooled1[[1]], model = "Claassen 2020a"),
  mutate(result_clsFullUncertainAPSR$pooled1[[1]], model = "Claassen w/ Uncertainty"),
  mutate(result_dcpoFullUncertainAPSR$pooled1[[1]], model = "DCPO")
) %>% 
  mutate(type = "ECM")

result_fd1 <- bind_rows(
  mutate(result_clsAPSR$fd1[[1]], model = "Claassen 2020a"),
  mutate(result_clsFullUncertainAPSR$fd1[[1]], model = "Claassen w/ Uncertainty"),
  mutate(result_dcpoFullUncertainAPSR$fd1[[1]], model = "DCPO")
) %>% 
  mutate(type = "First Difference")

result_APSR1 <- bind_rows(result_ecm1, result_fd1) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = c(rep(
      c("Democratic Mood\n (t-1)",
        "Democratic Mood\n (t-2)",
      "Liberal Democracy\n (Difference)",
      "Liberal Democracy\n (t-1)",
      "Log GDP\n per capita\n (Difference)",
      "Log GDP\n (t-1)"),
    time = 3),
    rep(
      c("Liberal Democracy\n (Difference)", 
        "Log GDP\n per capita\n (Difference)"), 
      time = 3
    )),
    type = factor(type, levels = c("ECM", "First Difference")),
    model = factor(model, levels = c("Claassen 2020a", "Claassen w/ Uncertainty", "DCPO"))
  )

terms1 <- factor(rev(c("Support[t-1]",
                      "Support[t-2]",
                      "Delta~Liberal~democracy",
                      "Liberal~democracy[t-1]",
                      "Delta~Log~GDP~per~capita",
                      "Log~GDP[t-1]")),
                levels = rev(c("Support[t-1]",
                               "Support[t-2]",
                               "Delta~Liberal~democracy",
                               "Liberal~democracy[t-1]",
                               "Delta~Log~GDP~per~capita",
                               "Log~GDP[t-1]")))

plot_APSR1 <- dwplot(result_APSR1, dodge_size = 0.8, by_2sd = FALSE) +
  scale_y_discrete(labels = parse(text = levels(terms1))) +
  scale_color_grey(start = 0.4, end = 0.8, guide = guide_legend(reverse = TRUE)) +
  # scale_color_uiowa(name = "", palette = "digitMixed") +
  labs(
    title = "DV: Public Support for Democracy",
    subtitle = "Regime Compacted"
  ) +
  facet_wrap(~type, scales = "free_x") + 
  theme(legend.position = "none")

 
result_ecm2 <- bind_rows(
  mutate(result_clsAPSR$pooled2[[1]], model = "Claassen 2020a"),
  mutate(result_clsFullUncertainAPSR$pooled2[[1]], model = "Claassen w/ Uncertainty"),
  mutate(result_dcpoFullUncertainAPSR$pooled2[[1]], model = "DCPO")
) %>% 
  mutate(type = "ECM")

result_fd2 <- bind_rows(
  mutate(result_clsAPSR$fd2[[1]], model = "Claassen 2020a"),
  mutate(result_clsFullUncertainAPSR$fd2[[1]], model = "Claassen w/ Uncertainty"),
  mutate(result_dcpoFullUncertainAPSR$fd2[[1]], model = "DCPO")
) %>% 
  mutate(type = "First Difference")

result_APSR2 <- bind_rows(result_ecm2, result_fd2) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = c(rep(
      c("Democratic Mood\n (t-1)",
        "Democratic Mood\n (t-2)",
      "Liberal Democracy\n (Difference)",
      "Liberal Democracy\n (t-1)",
      "Log GDP\n per capita\n (Difference)",
      "Log GDP\n (t-1)",
      "Corruption\n (Difference)",
      "Corruption\n (t-1)"
      ),
    time = 3),
    rep(
      c("Liberal Democracy\n (Difference)", 
        "Log GDP\n per capita\n (Difference)",
        "Corruption\n (Difference)"), 
      time = 3
    )),
    type = factor(type, levels = c("ECM", "First Difference")),
    model = factor(model, levels = c("Claassen 2020a", "Claassen w/ Uncertainty", "DCPO"))
  )

terms2 <- factor(rev(c("Support[t-1]",
                      "Support[t-2]",
                      "Delta~Liberal~democracy",
                      "Liberal~democracy[t-1]",
                      "Delta~Log~GDP~per~capita",
                      "Log~GDP[t-1]",
                      "Delta~Corruption",
                      "Corruption[t-1]")),
                levels = rev(c("Support[t-1]",
                               "Support[t-2]",
                               "Delta~Liberal~democracy",
                               "Liberal~democracy[t-1]",
                               "Delta~Log~GDP~per~capita",
                               "Log~GDP[t-1]",
                               "Delta~Corruption",
                               "Corruption[t-1]")))

plot_APSR2 <- dwplot(result_APSR2, dodge_size = 0.8, by_2sd = FALSE) +
  scale_y_discrete(labels = parse(text = levels(terms2))) +
  scale_color_grey(start = 0.4, end = 0.8, guide = guide_legend(reverse = TRUE)) +
  # scale_color_uiowa(name = "", palette = "digitMixed") +
  labs(
    title = "",
    subtitle = "W. Corruption"
  ) +
  facet_wrap(~type, scales = "free_x") +
  theme(legend.position = "none")

# Regime specified ####

result_ecmReg1 <- bind_rows(
  mutate(result_clsAPSR$`pooled-regime1`[[1]], model = "Claassen 2020a"),
  mutate(result_clsFullUncertainAPSR$`pooled-regime1`[[1]], model = "Claassen w/ Uncertainty"),
  mutate(result_dcpoFullUncertainAPSR$`pooled-regime1`[[1]], model = "DCPO")
) %>% 
  mutate(type = "ECM")

result_fdReg1 <- bind_rows(
  mutate(result_clsAPSR$`fd-regime1`[[1]], model = "Claassen 2020a"),
  mutate(result_clsFullUncertainAPSR$`fd-regime1`[[1]], model = "Claassen w/ Uncertainty"),
  mutate(result_dcpoFullUncertainAPSR$`fd-regime1`[[1]], model = "DCPO")
) %>% 
  mutate(type = "First Difference")

result_RegAPSR1 <- bind_rows(result_ecmReg1, result_fdReg1) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = c(rep(
      c("Democratic Mood\n (t-1)",
        "Democratic Mood\n (t-2)",
      "Electoral Democracy\n (Difference)",
      "Electoral Democracy\n (t-1)",
      "Minoritarian Democracy\n (Difference)",
      "Minoritarian Democracy\n (t-1)",
      "Log GDP\n per capita\n (Difference)",
      "Log GDP\n (t-1)"),
    time = 3),
    rep(
      c("Electoral Democracy\n (Difference)", 
        "Minoritarian Democracy\n (Difference)",
        "Log GDP\n per capita\n (Difference)"), 
      time = 3
    )),
    type = factor(type, levels = c("ECM", "First Difference")),
    model = factor(model, levels = c("Claassen 2020a", "Claassen w/ Uncertainty", "DCPO"))
  )

terms3 <- factor(rev(c("Support[t-1]",
                      "Support[t-2]",
                      "Delta~Electoral~democracy",
                      "Electoral~democracy[t-1]",
                      "Delta~Minoritarian~democracy",
                      "Minoritarian~democracy[t-1]",
                      "Delta~Log~GDP~per~capita",
                      "Log~GDP[t-1]")),
                levels = rev(c("Support[t-1]",
                      "Support[t-2]",
                      "Delta~Electoral~democracy",
                      "Electoral~democracy[t-1]",
                      "Delta~Minoritarian~democracy",
                      "Minoritarian~democracy[t-1]",
                      "Delta~Log~GDP~per~capita",
                      "Log~GDP[t-1]")))

plot_RegAPSR1 <- dwplot(result_RegAPSR1, dodge_size = 0.8, by_2sd = FALSE) +
  scale_y_discrete(labels = parse(text = levels(terms3))) +
  scale_color_grey(start = 0.4, end = 0.8, guide = guide_legend(reverse = TRUE)) +
  # scale_color_uiowa(name = "", palette = "digitMixed") +
  labs(
    title = "",
    subtitle = "Regime Specified"
  ) +
  facet_wrap(~type, scales = "free_x") + 
  theme(legend.position = "none")

 
result_ecmReg2 <- bind_rows(
  mutate(result_clsAPSR$`pooled-regime2`[[1]], model = "Claassen 2020a"),
  mutate(result_clsFullUncertainAPSR$`pooled-regime2`[[1]], model = "Claassen w/ Uncertainty"),
  mutate(result_dcpoFullUncertainAPSR$`pooled-regime2`[[1]], model = "DCPO")
) %>% 
  mutate(type = "ECM")

result_fdReg2 <- bind_rows(
  mutate(result_clsAPSR$`fd-regime2`[[1]], model = "Claassen 2020a"),
  mutate(result_clsFullUncertainAPSR$`fd-regime2`[[1]], model = "Claassen w/ Uncertainty"),
  mutate(result_dcpoFullUncertainAPSR$`fd-regime2`[[1]], model = "DCPO")
) %>% 
  mutate(type = "First Difference")

result_RegAPSR2 <- bind_rows(result_ecmReg2, result_fdReg2) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = c(rep(
      c("Democratic Mood\n (t-1)",
        "Democratic Mood\n (t-2)",
      "Electoral Democracy\n (Difference)",
      "Electoral Democracy\n (t-1)",
      "Minoritarian Democracy\n (Difference)",
      "Minoritarian Democracy\n (t-1)",
      "Log GDP\n per capita\n (Difference)",
      "Log GDP\n (t-1)",
      "Corruption\n (Difference)",
      "Corruption\n (t-1)"
      ),
    time = 3),
    rep(
      c("Electoral Democracy\n (Difference)", 
        "Minoritarian Democracy\n (Difference)",
        "Log GDP\n per capita\n (Difference)",
        "Corruption\n (Difference)"), 
      time = 3
    )),
    type = factor(type, levels = c("ECM", "First Difference")),
    model = factor(model, levels = c("Claassen 2020a", "Claassen w/ Uncertainty", "DCPO"))
  )

terms4 <- factor(rev(c("Support[t-1]",
                      "Support[t-2]",
                      "Delta~Electoral~democracy",
                      "Electoral~democracy[t-1]",
                      "Delta~Minoritarian~democracy",
                      "Minoritarian~democracy[t-1]",
                      "Delta~Log~GDP",
                      "Log~GDP[t-1]",
                      "Delta~Corruption",
                      "Corruption[t-1]")),
                levels = rev(c("Support[t-1]",
                               "Support[t-2]",
                               "Delta~Electoral~democracy",
                               "Electoral~democracy[t-1]",
                               "Delta~Minoritarian~democracy",
                               "Minoritarian~democracy[t-1]",
                               "Delta~Log~GDP",
                               "Log~GDP[t-1]",
                               "Delta~Corruption",
                               "Corruption[t-1]")))

plot_RegAPSR2 <- dwplot(result_RegAPSR2, dodge_size = 0.8, by_2sd = FALSE) +
  scale_y_discrete(labels = parse(text = levels(terms4))) +
  scale_color_grey(start = 0.4, end = 0.8, guide = guide_legend(reverse = TRUE)) +
  # scale_color_uiowa(name = "", palette = "digitMixed") +
  labs(
    title = "",
    subtitle = "Regime Specified w. Corruption"
  ) +
  facet_wrap(~type, scales = "free_x") +
  theme(legend.position = "bottom", 
        legend.title = element_blank())

legend <- get_legend(plot_RegAPSR2)

plot_RegAPSR2 <- plot_RegAPSR2 + theme(legend.position = "none")

grid.arrange(plot_APSR1, plot_APSR2, plot_RegAPSR1, plot_RegAPSR2, legend, ncol = 1)


## ----MocclaassenUncertaintyAJPS--------------------
load("data/cls_ajps.rda")
df_cls <- purrr::map(1:900, function(anEntry) {
  cls_ajps[[anEntry]] %>% 
    plm::pdata.frame(index = c("country", "year"))
})

ls_iv <- c("plm::lag(SupDem_trim, 1)",                         ##sumpdem_trim, our model did not use trim
           "plm::lag(theta_dem, 1) + plm::lag(theta_aut, 1) ")
ls_method <- c("plm", "pgmm")
ls_eq <- glue("Libdem_VD ~ plm::lag(Libdem_VD, 1:2) + {ls_iv} + 
              plm::lag(lnGDP_imp, 1) + plm::lag(GDP_imp_grth, 1) + 
              plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1) + plm::lag(Res_cp_WDI_di, 1)") 

ls_mod <- c(
  glue("plm({ls_eq}, model = 'pooling', data = aData)"),
  glue(
    "pgmm({ls_eq} | plm::lag(Libdem_VD, 3:5), data = aData, effect='individual', model='onestep', transformation='ld', indexes=c('country', 'year'))"
  )
)

result_clsUncertain  <- map(ls_mod[1:2], function(aMod){
  result <- methodComposition1(df_cls,aMod)
})

names(result_clsUncertain) <- c("pooled", "pooled-regime")

result_clsUncertain_gmm  <- map(ls_mod[3:4], function(aMod){
  result <- methodComposition2(df_cls,aMod)
})
names(result_clsUncertain_gmm) <- c("gmm", "gmm-regime")

result_clsUncertain_whole <- append(result_clsUncertain, result_clsUncertain_gmm)

saveRDS(result_clsUncertain_whole, file =here("output","clsAJPS_uncertainty.RDS"))



## ----MocdcpoUncertaintyAJPS------------------------
load(here("data","dcpo_ajps.rda"))

df_dcpo <- purrr::map(1:900, function(anEntry) {
  dcpo_ajps[[anEntry]] %>% 
    mutate(theta_dem = ifelse(is.na(theta_trim),NA,theta_dem),
           theta_aut = ifelse(is.na(theta_trim),NA,theta_aut)) %>% 
    plm::pdata.frame(index = c("country", "year"))
})


ls_iv <- c("plm::lag(theta_trim, 1)", 
           "plm::lag(theta_dem, 1) + plm::lag(theta_aut, 1)") 
ls_method <- c("plm", "pgmm")
ls_eq <- glue("Vdem_libdem ~ plm::lag(Vdem_libdem, 1:2) + {ls_iv} + plm::lag(lg_imp_mdpgdp, 1) + plm::lag(mdprgdp_grwth, 1) + plm::lag(Region_libdem, 1) + plm::lag(muslism_prop_2010, 1) + plm::lag(dependence_pc_di, 1)") 
ls_mod <- c(
  glue("plm({ls_eq}, model = 'pooling', data = aData)"),
  glue(
    "pgmm({ls_eq} | plm::lag(Vdem_libdem, 3:5), data = aData, effect='individual', model='onestep', transformation='ld', indexes=c('country', 'year'))"
  )
)

result_dcpoUncertain  <- map(ls_mod[1:2], function(aMod){
  result <- methodComposition1(df_dcpo,aMod)
})

names(result_dcpoUncertain) <- c("pooled", "pooled-regime")

result_dcpoUncertain_gmm  <- map(ls_mod[3:4], function(aMod){
  result <- methodComposition2(df_dcpo,aMod)
})

names(result_dcpoUncertain_gmm) <- c("gmm", "gmm-regime")

result_dcpoUncertain_whole <- append(result_dcpoUncertain, result_dcpoUncertain_gmm)

saveRDS(result_dcpoUncertain_whole, file =here("output","dcpoAJPS_uncertainty.RDS"))




## ----visualizedMocUncertainAJPS, fig.cap= "The Effect of Public Support on Democracy ", eval = TRUE, fig.width=7, fig.height=10----

result_clsAJPS <- readRDS(here("output", "estimates_clsMeanAJPS.RDS"))
result_clsUncertain_whole <- readRDS(here("output", "clsAJPS_uncertainty.RDS"))
result_dcpoUncertain_whole <- readRDS(here("output", "dcpoAJPS_uncertainty.RDS"))

result_pooled <- bind_rows(
  mutate(result_clsAJPS[["pooled"]][[1]][,1:3], model = "Claassen 2020a"),
  mutate(MOCsumm(result_clsUncertain_whole[["pooled"]]), model = "Claassen w/uncertainty"),
  mutate(MOCsumm(result_dcpoUncertain_whole[["pooled"]]), model = "DCPO")
) %>% 
  mutate(type = "Pooled")


result_gmm <- bind_rows(
  mutate(result_clsAJPS[["gmm"]][[1]][,1:3], model = "Claassen 2020a"),
  mutate(MOCsumm(result_clsUncertain_whole[["gmm"]]), model = "Claassen w/uncertainty"),
  mutate(MOCsumm(result_dcpoUncertain_whole[["gmm"]]), model = "DCPO")
) %>% 
  mutate(type = "GMM")


result_pooledAJPS <- bind_rows(result_pooled, result_gmm) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = rep(
      c("Democracy\n (t-1)",
        "Democracy\n (t-2)",
        "Support\n (t-1)",
        "Log GDP\n per capita\n (t-1)",
        "GDP per\n capita growth\n (t-1)",
        "Regional\n democracy\n (t-1)",
        "Percent\n Muslim\n (t-1)",
        "Resource\n dependence\n (t-1)"),
      time = 6), 
    type = factor(type, levels = c("Pooled", "GMM")),
    model = factor(model, levels = c("Claassen 2020a", "Claassen w/uncertainty", "DCPO")))
    

terms <- factor(rev(c("Democracy[t-1]",
                      "Democracy[t-2]",
                      "Support[t-1]",
                      "Log~GDP~per~capita[t-1]",
                      "GDP~per~capita~growth[t-1]",
                      "Regional~democracy[t-1]",
                      "Percent~Muslim[t-1]",
                      "Resource~dependence[t-1]")), levels =
                  rev(c("Democracy[t-1]",
                        "Democracy[t-2]",
                        "Support[t-1]",
                        "Log~GDP~per~capita[t-1]",
                        "GDP~per~capita~growth[t-1]",
                        "Regional~democracy[t-1]",
                        "Percent~Muslim[t-1]",
                        "Resource~dependence[t-1]")))

plot_AJPS1 <- dwplot(result_pooledAJPS, dodge_size = 0.8, by_2sd = FALSE) +
  scale_y_discrete(labels = parse(text = levels(terms))) +
  scale_color_grey(start = 0.4, end = 0.8) +
  labs(
    title = "DV: V-Dem Liberal Democracy Index",
    subtitle = "Regime Compacted"
  ) +
  facet_wrap(~type, scales = "free_x") + 
  theme(legend.position = "none")

###########Regime
result_regime <- bind_rows(
  mutate(result_clsAJPS[["pooled-regime"]][[1]][,1:3], model = "Claassen 2020a"),
  mutate(MOCsumm(result_clsUncertain_whole[["pooled-regime"]]), model = "Claassen w/uncertainty"),
  mutate(MOCsumm(result_dcpoUncertain_whole[["pooled-regime"]]), model = "DCPO")
) %>% 
  mutate(type = "Pooled")

result_regimeGmm <- bind_rows(
  mutate(result_clsAJPS[["gmm-regime"]][[1]][,1:3], model = "Claassen 2020a"),
  mutate(MOCsumm(result_clsUncertain_whole[["gmm-regime"]]), model = "Claassen w/uncertainty"),
    mutate(MOCsumm(result_dcpoUncertain_whole[["gmm-regime"]]), model = "DCPO")
)%>% 
  mutate(type = "GMM")

result_regimeAJPS <- bind_rows(result_regime, result_regimeGmm) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = rep(
      c("Democracy\n (t-1)",
        "Democracy\n (t-2)",
        "Support\n demo only\n (t-1)",
        "Support\n auto only\n (t-1)",
        "Log GDP\n per capita\n (t-1)",
        "GDP per\n capita growth\n (t-1)",
        "Regional\n democracy\n (t-1)",
        "Percent\n Muslim\n (t-1)",
        "Resource\n dependence\n (t-1)"),
      time = 6), 
    type = factor(type, levels = c("Pooled", "GMM")),
    model = factor(model, levels = c("Claassen 2020a","Claassen w/uncertainty", "DCPO")))
terms2 <- factor(rev(c("Democracy[t-1]",
                       "Democracy[t-2]",
                       "Support[t-1] (democracies~only)",
                       "Support[t-1] (autocracies~only)",
                       "Log~GDP~per~capita[t-1]",
                       "GDP~per~capita~growth[t-1]",
                       "Regional~democracy[t-1]",
                       "Percent~Muslim[t-1]",
                       "Resource~dependence[t-1]")), levels =
                   rev(c("Democracy[t-1]",
                         "Democracy[t-2]",
                         "Support[t-1] (democracies~only)",
                         "Support[t-1] (autocracies~only)",
                         "Log~GDP~per~capita[t-1]",
                         "GDP~per~capita~growth[t-1]",
                         "Regional~democracy[t-1]",
                         "Percent~Muslim[t-1]",
                         "Resource~dependence[t-1]")))
plot_AJPS2 <- dwplot(result_regimeAJPS, dodge_size = 0.8, by_2sd = FALSE) +
  scale_y_discrete(labels = parse(text = levels(terms2))) +
  scale_color_grey(start = 0.4, end = 0.8, guide = guide_legend(reverse = TRUE)) +
  labs(
    title = "",
    subtitle = "Regime Specific"
  ) +
  facet_wrap(~type, scales = "free_x") +
  theme(legend.position = "bottom", 
        legend.title = element_blank())
legend <- get_legend(plot_AJPS2)
plot_AJPS2 <- plot_AJPS2 + theme(legend.position = "none")
grid.arrange(plot_AJPS1, plot_AJPS2, legend, ncol = 1)





## ----claassenMocUncertaintyAPSR--------------------
load(here("data", "cls_apsr.rda"))



df_clsAPSR <- purrr::map(1:900, function(anEntry) {
  cls_apsr[[anEntry]] %>% 
    plm::pdata.frame(index = c("country", "year"))
}
)

ls_ivECM <- c("diff(Libdem_z, lag = 1) + plm::lag(Libdem_z, 1)", 
              "diff(Polyarchy_z, lag = 1) + plm::lag(Polyarchy_z, 1) + diff(Liberal_z, lag = 1) + plm::lag(Liberal_z, 1)")
ls_ctrlECM <- c("", " + diff(Corrup_TI_z, lag = 1) + plm::lag(Corrup_TI_z, 1)")

ls_eqECM <- glue("diff(SupDem_trim, lag = 1) ~ plm::lag(SupDem_trim, 1:2) + {ls_ivECM} + diff(lnGDP_imp, lag = 1) + plm::lag(lnGDP_imp, 1)") %>% 
  outer(ls_ctrlECM, paste0) %>% 
  as.vector() ##Change theta to SupDem_trim



ls_ivFD <- c("Libdem_z",
             "Polyarchy_z + Liberal_z")
ls_ctrlFD <- c("", " + Corrup_TI_z")
ls_eqFD <- glue("SupDem_trim ~ {ls_ivFD} + lnGDP_imp") %>% 
  outer(ls_ctrlFD, paste0) %>% 
  as.vector()
ls_mod <- c(
  glue("plm({ls_eqECM}, model = 'pooling', data = aData)"),
  glue("plm({ls_eqFD}, model = 'fd', index = 'country', data = aData)")
) 

result_clsUncertain  <- map(ls_mod, function(aMod){
  result <- methodComposition3(df_clsAPSR,aMod)
})

names(result_clsUncertain) <- c("pooled1", "pooled-regime1", "pooled2", "pooled-regime2",
                                "fd1", "fd-regime1", "fd2", "fd-regime2")



saveRDS(result_clsUncertain, file = here("ouput","clsAPSR_uncertainty.RDS"))





## ----dcpoMocUncertaintyAPSR------------------------
load(here("data","dcpo_apsr.rda"))
df_dcpoUncertainAPSR <- purrr::map(1:900, function(anEntry) {
  dcpo_apsr[[anEntry]] %>% 
    plm::pdata.frame(index = c("country", "year"))
})

ls_ivECM <- c("diff(Vdem_libdem_z, lag = 1) + plm::lag(Vdem_libdem_z, 1)", 
              "diff(Vdem_polyarchy_z, lag = 1) + plm::lag(Vdem_polyarchy_z, 1) + diff(Vdem_liberal_z, lag = 1) + plm::lag(Vdem_liberal_z, 1)")
ls_ctrlECM <- c("", " + diff(corruption_z, lag = 1) + plm::lag(corruption_z, 1)")
ls_eqECM <- glue("diff(theta_trim, lag = 1) ~ plm::lag(theta_trim, 1:2) + {ls_ivECM} + diff(lg_imp_mdpgdp, lag = 1) + plm::lag(lg_imp_mdpgdp, 1)") %>% 
  outer(ls_ctrlECM, paste0) %>% 
  as.vector()


ls_ivFD <- c("Vdem_libdem_z",
             "Vdem_polyarchy_z + Vdem_liberal_z")
ls_ctrlFD <- c("", " + corruption_z")
ls_eqFD <- glue("theta_trim ~ {ls_ivFD} + lg_imp_mdpgdp") %>% 
  outer(ls_ctrlFD, paste0) %>% 
  as.vector()


ls_mod <- c(
  glue("plm({ls_eqECM}, model = 'pooling', data = aData)"),
  glue("plm({ls_eqFD}, model = 'fd', index = 'country', data = aData)")
)

result_dcpoUncertain  <- map(ls_mod, function(aMod){
  result <- methodComposition3(df_dcpoUncertainAPSR,aMod)
})

names(result_dcpoUncertain) <- c("pooled1", "pooled-regime1", "pooled2", "pooled-regime2",
                                "fd1", "fd-regime1", "fd2", "fd-regime2")

saveRDS(result_dcpoUncertain, file =here("output","dcpoAPSR_uncertainty.RDS"))




## ----visualizedMocUncertainAPSR, fig.cap= "The Effect of Democracy on Public Support", eval = TRUE, fig.width=7, fig.height=10----


result_clsUncertain <- readRDS(here("output","clsAPSR_uncertainty.RDS"))
result_dcpoUncertain <- readRDS(here("output","dcpoAPSR_uncertainty.RDS"))
result_clsAPSR <- readRDS(here("output","estimates_clsMeanAPSR.RDS"))

result_ecm1 <- bind_rows(
  mutate(result_clsAPSR[["pooled1"]][[1]][,1:3], model = "Claassen 2020b"),
  mutate(MOCsumm2(result_clsUncertain[["pooled1"]]), model = "Claassen w/uncertainty"),
  mutate(MOCsumm2(result_dcpoUncertain[["pooled1"]]), model = "DCPO")
) %>% 
  mutate(type = "ECM")


result_fd1 <- bind_rows(
  mutate(result_clsAPSR[["fd1"]][[1]][,1:3], model = "Claassen 2020b"),
  mutate(MOCsumm2(result_clsUncertain[["fd1"]]), model = "Claassen w/uncertainty"),
  mutate(MOCsumm2(result_dcpoUncertain[["fd1"]]), model = "DCPO")
) %>% 
  mutate(type = "First Difference")

result_APSR1 <- bind_rows(result_ecm1, result_fd1) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = c(rep(
      c("Democratic Mood\n (t-1)",
        "Democratic Mood\n (t-2)",
        "Liberal Democracy\n (Difference)",
        "Liberal Democracy\n (t-1)",
        "Log GDP\n per capita\n (Difference)",
        "Log GDP\n (t-1)"),
      time = 3),
      rep(
        c("Liberal Democracy\n (Difference)", 
          "Log GDP\n per capita\n (Difference)"), 
        time = 3
      )),
    type = factor(type,  levels = c("ECM", "First Difference")),
    model = factor(model, levels = c("Claassen 2020b","Claassen w/uncertainty", "DCPO"))
  )

terms1 <- factor(rev(c("Support[t-1]",
                       "Support[t-2]",
                       "Delta~Liberal~democracy",
                       "Liberal~democracy[t-1]",
                       "Delta~Log~GDP~per~capita",
                       "Log~GDP[t-1]")),
                 levels = rev(c("Support[t-1]",
                                "Support[t-2]",
                                "Delta~Liberal~democracy",
                                "Liberal~democracy[t-1]",
                                "Delta~Log~GDP~per~capita",
                                "Log~GDP[t-1]")))
plot_APSR1 <- dwplot(result_APSR1, dodge_size = 0.8, by_2sd = FALSE) +
  scale_y_discrete(labels = parse(text = levels(terms1))) +
  scale_color_grey(start = 0.4, end = 0.8, guide = guide_legend(reverse = TRUE)) +
  # scale_color_uiowa(name = "", palette = "digitMixed") +
  labs(
    title = "DV: Public Support for Democracy",
    subtitle = "Regime Compacted"
  ) +
  facet_wrap(~type, drop=T,scales = "free_x") +
  theme(legend.position = "none")

############################ Regime specified####
result_ecmReg1 <- bind_rows(
  mutate(result_clsAPSR[["pooled-regime1"]][[1]][,1:3], model = "Claassen 2020b"),
  mutate(MOCsumm2(result_clsUncertain[["pooled-regime1"]]), model = "Claassen w/uncertainty"),
  mutate(MOCsumm2(result_dcpoUncertain[["pooled-regime1"]]), model = "DCPO")
) %>% 
  mutate(type = "ECM")


result_fdReg1 <- bind_rows(
  mutate(result_clsAPSR[["fd-regime1"]][[1]][,1:3], model = "Claassen 2020b"),
  mutate(MOCsumm2(result_clsUncertain[["fd-regime1"]]), model = "Claassen w/uncertainty"),
  mutate(MOCsumm2(result_dcpoUncertain[["fd-regime1"]]), model = "DCPO")
) %>% 
  mutate(type = "First Difference")


result_RegAPSR1 <- bind_rows(result_ecmReg1, result_fdReg1) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = c(rep(
      c("Democratic Mood\n (t-1)",
        "Democratic Mood\n (t-2)",
        "Electoral Democracy\n (Difference)",
        "Electoral Democracy\n (t-1)",
        "Minoritarian Democracy\n (Difference)",
        "Minoritarian Democracy\n (t-1)",
        "Log GDP\n per capita\n (Difference)",
        "Log GDP\n (t-1)"),
      time = 3),
      rep(
        c("Electoral Democracy\n (Difference)", 
          "Minoritarian Democracy\n (Difference)",
          "Log GDP\n per capita\n (Difference)"), 
        time = 3
      )),
    type = factor(type,  levels = c("ECM", "First Difference")),
    model = factor(model, levels = c("Claassen 2020b","Claassen w/uncertainty", "DCPO"))
  )
terms3 <- factor(rev(c("Support[t-1]",
                       "Support[t-2]",
                       "Delta~Electoral~democracy",
                       "Electoral~democracy[t-1]",
                       "Delta~Minoritarian~democracy",
                       "Minoritarian~democracy[t-1]",
                       "Delta~Log~GDP~per~capita",
                       "Log~GDP[t-1]")),
                 levels = rev(c("Support[t-1]",
                                "Support[t-2]",
                                "Delta~Electoral~democracy",
                                "Electoral~democracy[t-1]",
                                "Delta~Minoritarian~democracy",
                                "Minoritarian~democracy[t-1]",
                                "Delta~Log~GDP~per~capita",
                                "Log~GDP[t-1]")))
plot_RegAPSR1 <- dwplot(result_RegAPSR1, dodge_size = 0.8, by_2sd = FALSE) +
  scale_y_discrete(labels = parse(text = levels(terms3))) +
  scale_color_grey(start = 0.4, end = 0.8, guide = guide_legend(reverse = TRUE)) +
  # scale_color_uiowa(name = "", palette = "digitMixed") +
  labs(
    title = "",
    subtitle = "Regime Specified"
  ) +
  facet_wrap(~type, scales = "free_x") + 
  theme(legend.position = "none")


#########Corruption

result_ecm2 <- bind_rows(
  mutate(result_clsAPSR[["pooled2"]][[1]][,1:3], model = "Claassen 2020b"),
  mutate(MOCsumm2(result_clsUncertain[["pooled2"]]), model = "Claassen w/uncertainty"),
  mutate(MOCsumm2(result_dcpoUncertain[["pooled2"]]), model = "DCPO")
) %>% 
  mutate(type = "ECM")

result_fd2 <- bind_rows(
  mutate(result_clsAPSR[["fd2"]][[1]][,1:3], model = "Claassen 2020b"),
  mutate(MOCsumm2(result_clsUncertain[["fd2"]]), model = "Claassen w/uncertainty"),
  mutate(MOCsumm2(result_dcpoUncertain[["fd2"]]), model = "DCPO")
) %>% 
  mutate(type = "First Difference")

result_APSR2 <- bind_rows(result_ecm2, result_fd2) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = c(rep(
      c("Democratic Mood\n (t-1)",
        "Democratic Mood\n (t-2)",
        "Liberal Democracy\n (Difference)",
        "Liberal Democracy\n (t-1)",
        "Log GDP\n per capita\n (Difference)",
        "Log GDP\n (t-1)",
        "Corruption\n (Difference)",
        "Corruption\n (t-1)"
      ),
      time = 3),
      rep(
        c("Liberal Democracy\n (Difference)", 
          "Log GDP\n per capita\n (Difference)",
          "Corruption\n (Difference)"), 
        time = 3
      )),
    type = factor(type,  levels = c("ECM", "First Difference")),
    model = factor(model, levels = c("Claassen 2020b", "Claassen w/uncertainty","DCPO"))
  )
terms2 <- factor(rev(c("Support[t-1]",
                       "Support[t-2]",
                       "Delta~Liberal~democracy",
                       "Liberal~democracy[t-1]",
                       "Delta~Log~GDP~per~capita",
                       "Log~GDP[t-1]",
                       "Delta~Corruption",
                       "Corruption[t-1]")),
                 levels = rev(c("Support[t-1]",
                                "Support[t-2]",
                                "Delta~Liberal~democracy",
                                "Liberal~democracy[t-1]",
                                "Delta~Log~GDP~per~capita",
                                "Log~GDP[t-1]",
                                "Delta~Corruption",
                                "Corruption[t-1]")))
plot_APSR2 <- dwplot(result_APSR2, dodge_size = 0.8, by_2sd = FALSE) +
  scale_y_discrete(labels = parse(text = levels(terms2))) +
  scale_color_grey(start = 0.4, end = 0.8, guide = guide_legend(reverse = TRUE)) +
  # scale_color_uiowa(name = "", palette = "digitMixed") +
  labs(
    title = "",
    subtitle = "W. Corruption"
  ) +
  facet_wrap(~type, scales = "free_x") +
  theme(legend.position = "none")


#########Regime + Corruption


result_ecmReg2 <- bind_rows(
  mutate(result_clsAPSR[["pooled-regime2"]][[1]][,1:3], model = "Claassen 2020b"),
  mutate(MOCsumm2(result_clsUncertain[["pooled-regime2"]]), model = "Claassen w/uncertainty"),
  mutate(MOCsumm2(result_dcpoUncertain[["pooled-regime2"]]), model = "DCPO")) %>% 
  mutate(type = "ECM")
  


result_fdReg2 <- bind_rows(
  mutate(result_clsAPSR[["fd-regime2"]][[1]][,1:3], model = "Claassen 2020b"),
  mutate(MOCsumm2(result_clsUncertain[["fd-regime2"]]), model = "Claassen w/uncertainty"),
  mutate(MOCsumm2(result_dcpoUncertain[["fd-regime2"]]), model = "DCPO")) %>% 
  mutate(type = "First Difference")



result_RegAPSR2 <- bind_rows(result_ecmReg2, result_fdReg2) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = c(rep(
      c("Democratic Mood\n (t-1)",
        "Democratic Mood\n (t-2)",
        "Electoral Democracy\n (Difference)",
        "Electoral Democracy\n (t-1)",
        "Minoritarian Democracy\n (Difference)",
        "Minoritarian Democracy\n (t-1)",
        "Log GDP\n per capita\n (Difference)",
        "Log GDP\n (t-1)",
        "Corruption\n (Difference)",
        "Corruption\n (t-1)"
      ),
      time = 3),
      rep(
        c("Electoral Democracy\n (Difference)", 
          "Minoritarian Democracy\n (Difference)",
          "Log GDP\n per capita\n (Difference)",
          "Corruption\n (Difference)"), 
        time = 3
      )),
    type = factor(type,  levels = c("ECM", "First Difference")),
    model = factor(model, levels = c("Claassen 2020b", "Claassen w/uncertainty","DCPO"))
  )
terms4 <- factor(rev(c("Support[t-1]",
                       "Support[t-2]",
                       "Delta~Electoral~democracy",
                       "Electoral~democracy[t-1]",
                       "Delta~Minoritarian~democracy",
                       "Minoritarian~democracy[t-1]",
                       "Delta~Log~GDP",
                       "Log~GDP[t-1]",
                       "Delta~Corruption",
                       "Corruption[t-1]")),
                 levels = rev(c("Support[t-1]",
                                "Support[t-2]",
                                "Delta~Electoral~democracy",
                                "Electoral~democracy[t-1]",
                                "Delta~Minoritarian~democracy",
                                "Minoritarian~democracy[t-1]",
                                "Delta~Log~GDP",
                                "Log~GDP[t-1]",
                                "Delta~Corruption",
                                "Corruption[t-1]")))
plot_RegAPSR2 <- dwplot(result_RegAPSR2, dodge_size = 0.8, by_2sd = FALSE) +
  scale_y_discrete(labels = parse(text = levels(terms4))) +
  scale_color_grey(start = 0.4, end = 0.8, guide = guide_legend(reverse = TRUE)) +
  # scale_color_uiowa(name = "", palette = "digitMixed") +
  labs(
    title = "",
    subtitle = "Regime Specified w. Corruption"
  ) +
  facet_wrap(~type, scales = "free_x") +
  theme(legend.position = "bottom", 
        legend.title = element_blank())

legend <- get_legend(plot_RegAPSR2)

plot_RegAPSR2 <- plot_RegAPSR2 + theme(legend.position = "none")

grid.arrange(plot_APSR1, plot_APSR2, plot_RegAPSR1, plot_RegAPSR2, legend, ncol = 1)




