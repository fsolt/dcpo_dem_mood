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



get_legend<-function(myggplot){
    tmp <- ggplot_gtable(ggplot_build(myggplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}
