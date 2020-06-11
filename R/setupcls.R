
library(tidyverse)
library(countrycode)
library(DCPOtools)

wd <- getwd()
#surveys_data <- rio::import("https://github.com/fsolt/DCPOtools/blob/master/data/surveys_data.rda") # cannot work

load(paste(wd, '/data-raw/surveys_data.rda',sep = ''))  #load dcpo data


##mood_dem_cls deletes evs2017, arob5, eb47 which are not in original data. 
surveys_demmood <- read_csv("data/mood_dem_cls.csv")  
##non-respone need to 99
surveys_demmood$variable <- tolower(surveys_demmood$variable)
##convert to lowercase with clean replication.
surveys_demmood <- surveys_demmood %>% drop_na()

surveys_data$subfile[surveys_data$survey == "pew2005_6"] <- NA

########Function for claassen_setup from https://github.com/fsolt/DCPOtools/blob/master/R/claassen_setup.R
claassen_setup <- function(vars,
                           datapath = "../data/dcpo_surveys",
                           file = "",
                           chime = TRUE) {
  if ("data.frame" %in% class(vars)) {
    vars_table <- vars
  } else {
    vars_table <- readr::read_csv(vars, col_types = "ccccc")
  }
  
  # Revise countrycode::countrycode to work better with custom names in cc_dcpo
  body(countrycode)[[2]] <- substitute(
    if (is.null(custom_dict) | as.list(match.call())[["custom_dict"]] == "cc_dcpo") {
      if (origin == "country.name") {
        origin <- "country.name.en"
      }
      if (destination == "country.name") {
        destination <- "country.name.en"
      }
      if (origin %in% c("country.name.en", "country.name.de")) {
        origin <- paste0(origin, ".regex")
        origin_regex <- TRUE
      }
      else {
        origin_regex <- FALSE
      }
    }
  )
  
  # loop rather than purrr to avoid reloading datasets (some are big and slow)
  all_sets <- list()
  for (i in seq(nrow(vars_table))) {
    cat(i, " ")
    v <- vars_table[i, ]
    ds <- surveys_data[surveys_data$survey==v$survey, ]
    
    # Import dataset (if necessary)
    if (vars_table[["survey"]][i] != c(0, head(vars_table[["survey"]], -1))[i]) {
      dataset_path <- file.path(datapath,
                                paste0(ds$archive, "_files"),
                                paste0(ds$surv_program, "_files"),
                                ds$file_id)
      dataset_file <- list.files(path = dataset_path) %>% stringr::str_subset(".RData") %>% last()
      if (!is.na(ds$subfile)) dataset_file <- paste0(ds$subfile, ".RData")
      t_data <- rio::import(file.path(dataset_path, dataset_file))
      
      # Fix column names and make lowercase
      valid_column_names <- make.names(names = names(t_data), unique = TRUE, allow_ = TRUE) %>%
        stringr::str_to_lower()
      names(t_data) <- valid_column_names
      
      # Get countries
      suppressWarnings(
        t_data$c_dcpo <- if (ds$country_var %in% names(t_data)) {
          if (is.null(attr(t_data[[ds$country_var]], "labels"))) {
            if (!is.null(attr(t_data[[ds$country_var]], "value.labels"))) {
              attr(t_data[[ds$country_var]], "labels") <- attr(t_data[[ds$country_var]],
                                                               "value.labels") %>% as.numeric()
              attr(attr(t_data[[ds$country_var]], "labels"), "names") <- attr(attr(t_data[[ds$country_var]],
                                                                                   "value.labels"), "names")
            }
          }
          t_data[[ds$country_var]] %>%
            {if (!is.null(attr(t_data[[ds$country_var]], "labels")))
              labelled::labelled(., attr(., "labels")) %>%
                labelled::to_factor(levels = "prefixed") %>%
                forcats::fct_relabel(., function(x) stringr::str_replace(x, "\\[\\d+\\]\\s+", ""))
              else .} %>%
            as.character() %>%
            stringr::str_replace("Hait\xed", "Haiti") %>%
            {if (!is.na(ds$cc_dict))
              countrycode(., "orig", "dest", custom_dict = eval(parse(text = ds$cc_dict)))
              else if (!is.na(ds$cc_origin))
                countrycode(., ds$cc_origin, "dcpo.name", custom_dict = cc_dcpo)
              else if (!is.na(ds$cc_match))
                countrycode(., "country.name", "dcpo.name",
                            custom_match = eval(parse(text = ds$cc_match)), custom_dict = cc_dcpo)
              else countrycode(., "country.name", "dcpo.name", custom_dict = cc_dcpo)} %>%
            countrycode("country.name", "dcpo.name", custom_dict = cc_dcpo)
        } else ds$country_var %>%
          countrycode("country.name", "dcpo.name", custom_dict = cc_dcpo)
      )
      t_data <- t_data %>%
        filter(!is.na(c_dcpo))
      
      # Get years
      t_data$y_dcpo <- if (!is.na(ds$year_dict)) { # if there's a year dictionary...
        t_data[[ds$country_var]] %>%
          labelled::labelled(., attr(., "labels")) %>%
          labelled::to_factor(levels = "labels") %>%
          as.character() %>%
          countrycode("orig", "year", custom_dict = eval(parse(text = ds$year_dict)))
      } else if (!is.na(ds$year_var)) { # if there's a year variable...
        if (length(unique(t_data$c_dcpo))==1) { # single-country study
          t_data[[ds$year_var]]
        } else if (stringr::str_detect(ds$survey, "ess")) {
          if ("inwyr" %in% names(t_data)) {
            t_data %>%
              group_by(c_dcpo) %>%
              mutate(year = ifelse(!is.na(inwyr) & !inwyr==9999, inwyr, 2000 + essround * 2),
                     y_dcpo = round(mean(year, na.rm = TRUE))) %>%
              ungroup() %>%
              .[["y_dcpo"]]
          } else if ("inwyys" %in% names(t_data)) {
            t_data %>%
              group_by(c_dcpo) %>%
              mutate(year = ifelse(!is.na(inwyys) & !inwyys==9999, inwyys, 2000 + essround * 2),
                     y_dcpo = round(mean(year, na.rm = TRUE))) %>%
              ungroup() %>%
              .[["y_dcpo"]]
          }
        } else if (ds$survey == "cdcee" | ds$survey == "eqls") {
          suppressWarnings(
            t_data[[ds$year_var]] %>%
              labelled::labelled(., attr(., "labels")) %>%
              labelled::to_character(levels = "prefixed") %>%
              stringr::str_extract("\\d{4}")
          )
        } else if (ds$survey == "amb_combo") {
          t_data[[ds$year_var]]
        } else if (ds$survey == "neb_combo") {
          floor(t_data[[ds$year_var]])
        } else if (ds$surv_program == "afrob" |   # single-wave cross-national surveys
                   ds$survey == "arabb3") {       # with interviews bleeding over years
          t_data %>%
            mutate(year = lubridate::year(t_data[[ds$year_var]]),
                   modal_year = as.integer(names(table(year)[table(year)==max(table(year))])),
                   year = if_else(is.na(year) | year < 1950,
                                  modal_year,
                                  as.integer(year)),
                   group_dcpo = c_dcpo) %>%
            group_by(c_dcpo) %>%
            mutate(y_dcpo = round(mean(year))) %>%
            ungroup() %>%
            .[["y_dcpo"]]
        } else { # cross-national surveys with interviews bleeding over years
          t_data %>%
            mutate(modal_year = as.integer(names(table(t_data[[ds$year_var]])[table(t_data[[ds$year_var]])==max(table(t_data[[ds$year_var]]))])),
                   year = if_else(between(as.numeric(t_data[[ds$year_var]]),
                                          1950, as.numeric(lubridate::year(Sys.Date()))),
                                  as.integer(t_data[[ds$year_var]]),
                                  modal_year),
                   group_dcpo = c_dcpo) %>%
            {if (!is.na(ds$cy_var))
              mutate(., group_dcpo = t_data[[ds$cy_var]])
              else .} %>%
            group_by(group_dcpo) %>%
            mutate(y_dcpo = round(mean(year))) %>%
            ungroup() %>%
            .[["y_dcpo"]]
        }
      } else as.numeric(ds$year)
      t_data$y_dcpo <- as.numeric(t_data$y_dcpo)
      t_data <- t_data %>%
        filter(!is.na(y_dcpo))
      
      # Get weights
      if (!is.na(ds$wt) & !all(is.na(ds$wt))) {
        if (length(unlist(strsplit(ds$wt, split = " "))) == 1) {
          wt <- with(t_data, get(ds$wt))
        } else eval(parse(text = ds$wt))
        t_data$wt_dcpo <- wt
        t_data$wt_dcpo[t_data$wt_dcpo > 10] <- 10
        t_data$wt_dcpo <- t_data$wt_dcpo/mean(t_data$wt_dcpo, na.rm = TRUE)
        t_data$wt_dcpo[is.na(t_data$wt_dcpo)] <- 1
        rm(wt)
      } else t_data$wt_dcpo <- 1
    }
    
    # Get variable of interest
    if (length(unlist(strsplit(v$variable, split = " "))) == 1) {
      t_data$target <- with(t_data, as.numeric(get(v$variable) %>% stringr::str_trim()))
    } else {
      t_data <- t_data %>%
        mutate(target = eval(parse(text = v$variable)))
    }
    vals <- eval(parse(text = v$values))
    na_vals <- eval(parse(text = v$non_response))
    t_data$target <- if_else(t_data$target %in% c(vals, na_vals), t_data$target, NA_real_)
    options(warn = 2)
    t_data$target <- do.call(dplyr::recode, c(list(t_data$target), setNames(c(rep(1,length(na_vals)), 1:length(vals)), c(na_vals, vals))))
    options(warn = 0)
    
    # Summarize by country and year
    vars1 <- t_data %>%
      dplyr::select(c_dcpo, y_dcpo, wt_dcpo, target) %>%
      filter(!is.na(target)) %>%
      group_by(c_dcpo, y_dcpo, target) %>%
      summarise(survey = v$survey,
                item = v$item,
                n = mean(wt_dcpo) * length(na.omit(target))) %>%
      ungroup() %>%
      rename(country = c_dcpo,
             year = y_dcpo,
             survey = survey,
             item = item,
             r = target,
             n = n)
    
    all_sets[[i]] <- vars1
    rm(vars1)
  }
  
  suppressWarnings(
    all_data <- bind_rows(all_sets)
  )
  rm(list = c("t_data", "ds", "v"))
  
  all_data2 <- all_data %>%
    group_by(country, year, item, r) %>%
    summarize(n = sum(n),     # When two surveys ask the same question in
              survey = paste0(survey, collapse = ", ")) %>% # the same country-year, add samples together
    ungroup() %>%
    group_by(country) %>%
    mutate(cc_rank = n(),         # number of country-year-items (data-richness)
           year = as.integer(year)) %>%
    ungroup() %>%
    arrange(desc(cc_rank), country, year)
  
  # Chime
  if(chime) {
    beepr::beep()
  }
  
  if(file!="") {
    write_csv(all_data2, file)
  }
  
  return(all_data2)
}



###Setup dataframe for Claassen's model5. 


demmood <- claassen_setup(vars = surveys_demmood, datapath = "data/dcpo_surveys", file = "data/claassen_cleaned_data.csv")  
#16732  #this part is trikcy since I didn't use get_survey to create dcpo_surveys

demmood %>% 
  filter(survey=="amb_combo" & year== 2004&item=="strong_amb_1") %>%
  summarise(n=n()) #18

demmood$item[demmood$survey=="amb_combo"&demmood$year== 2004&demmood$item=="strong_amb_1"] <-"strong_amb_2"
demmood %>% 
  filter(survey=="amb_combo" & year== 2004&item=="strong_amb_2") %>%
  summarise(n=n())

#Survey responses from the following items-year-country combinations from the World Values Survey were therefore excluded
#. Vietnam: Army rule 2001; Strong leader 2001 . Albania: Army rule 1998 
#. Indonesia: Army rule 2001 & 2006 . Iran: Army rule 2000; Strong leader 2000 & 2005 
#. India: Strong leader, all years. . Pakistan: Army rule 1996 & 2001; Strong leader 1996 & 2001 
#. Kyrgyzstan: Strong leader 2003 & 2011 . Romania: Strong leader 1998, 2005 & 2012
#. Egypt: Strong leader 2012  exclude from dcpo in order to be the same with claassen

#. Vietnam: Army rule 2001; Strong leader 2001 . Albania: Army rule 1998 
demmood %>% 
  filter(country=="Vietnam" & year== 2001&item %in% c("army_wvs","strong_wvs")) %>%
  summarise(n=n())
nrow(demmood) #16732
demmood <- subset(demmood, !(country=="Vietnam" & year== 2001 & item %in% c("army_wvs","strong_wvs")))
nrow(demmood) #16725

demmood %>% 
  filter(country=="Albania" & year== 1998 & item %in% c("army_wvs")) %>%
  summarise(n=n())
demmood <- subset(demmood, !(country=="Albania" & year== 1998 & item %in% c("army_wvs")))
nrow(demmood) #16721


#. Indonesia: Army rule 2001 & 2006 . Iran: Army rule 2000; Strong leader 2000 & 2005 

demmood %>% 
  filter(country=="Indonesia" & year %in% c(2001,2006) & item %in% c("army_wvs")) %>%
  summarise(n=n())
demmood <- subset(demmood, !(country=="Indonesia" & year %in% c(2001,2006) & item %in% c("army_wvs")))
nrow(demmood) #16713


demmood %>% 
  filter(country=="Iran" & year %in% c(2000) & item %in% c("army_wvs")) %>%
  summarise(n=n())
demmood <- subset(demmood, !(country=="Iran" & year %in% c(2000) & item %in% c("army_wvs")))
nrow(demmood) #16709
demmood %>% 
  filter(country=="Iran" & year %in% c(2000,2007) & item %in% c("strong_wvs")) %>%
  summarise(n=n())  # in our data is 2007
demmood <- subset(demmood, !(country=="Iran" & year %in% c(2000,2007) & item %in% c("strong_wvs")))
nrow(demmood)  #16701


#. India: Strong leader, all years. . Pakistan: Army rule 1996 & 2001; Strong leader 1996 & 2001 

demmood %>% 
  filter(country=="India" & item %in% c("strong_wvs")) %>%
  summarise(n=n())
demmood <- subset(demmood, !(country=="India" & item %in% c("strong_wvs")))
nrow(demmood) #16685

demmood %>% 
  filter(country=="Pakistan" & year %in% c(1997,2001) & item %in% c("army_wvs")) %>%
  summarise(n=n())   #in our data is 1997
demmood <- subset(demmood, !(country=="Pakistan" & year %in% c(1997,2001) & item %in% c("army_wvs")))
nrow(demmood)  #16678

demmood %>% 
  filter(country=="Pakistan" & year %in% c(1997,2001) & item %in% c("strong_wvs")) %>%
  summarise(n=n())   #in our data is 1997
demmood <- subset(demmood, !(country=="Pakistan" & year %in% c(1997,2001) & item %in% c("strong_wvs")))
nrow(demmood)  #16671


#. Kyrgyzstan: Strong leader 2003 & 2011 . Romania: Strong leader 1998, 2005 & 2012
demmood %>% 
  filter(country=="Kyrgyzstan" & year %in% c(2003,2011) & item %in% c("strong_wvs")) %>%
  summarise(n=n())  
demmood <- subset(demmood, !(country=="Kyrgyzstan" & year %in% c(2003,2011) & item %in% c("strong_wvs")))
nrow(demmood)  #16663

demmood %>% 
  filter(country=="Romania" & year %in% c(1998,2005,2012) & item %in% c("strong_wvs")) %>%
  summarise(n=n())  
demmood <- subset(demmood, !(country=="Romania" & year %in% c(1998,2005,2012) & item %in% c("strong_wvs")))
nrow(demmood)  #16651

#. Egypt: Strong leader 2012  exclude from dcpo in order to be the same with claassen
demmood %>% 
  filter(country=="Egypt" & year %in% c(2012) & item %in% c("strong_wvs")) %>%
  summarise(n=n())  
demmood <- subset(demmood, !(country=="Egypt" & year %in% c(2012) & item %in% c("strong_wvs")))
nrow(demmood)  #16647

write.csv(demmood,"data/claassen_cleaned_data.csv")  #for replication one

