```{r formatResult-AJPS-dup}
result_pooled <- bind_rows(
    mutate(result_clsAJPS[["pooled"]][[1]][,1:3], submodel = "Point Estimates Only"),
    mutate(MOCsumm(mocresult_correctAJPS[["pooled"]]), submodel = "With Uncertainty"),
    mutate(MOCsumm(mocresult_expAJPS[["pooled"]]), submodel = "Uncertainty & Expanded Data")
) %>% 
    mutate(type = "Pooled OLS",
           model = "Model 1 (Pooled OLS)")


result_gmm <- bind_rows(
    mutate(result_clsAJPS[["gmm"]][[1]][,1:3], submodel = "Point Estimates Only"),
    mutate(MOCsumm(mocresult_correctAJPS[["gmm"]]), submodel = "With Uncertainty"),
    mutate(MOCsumm(mocresult_expAJPS[["gmm"]]), submodel = "Uncertainty & Expanded Data")
) %>% 
    mutate(type = "System GMM",
           model = "Model 3 (System GMM)")

result_pooledAJPS <- bind_rows(result_pooled, result_gmm) %>%
    filter(term != "(Intercept)") %>%
    mutate(
        term0 = rep(
            c("Democracy\n (t-1)",
              "Democracy\n (t-2)",
              "Support\n (t-1)",
              "Log GDP\n per capita\n (t-1)",
              "GDP per\n capita growth\n (t-1)",
              "Regional\n democracy\n (t-1)",
              "Percent\n Muslim\n (t-1)",
              "Resource\n dependence\n (t-1)"),
            time = 6), 
        type = factor(type, levels = c("Pooled OLS", "System GMM")),
        submodel = factor(submodel, levels = c("Point Estimates Only", "With Uncertainty", "Uncertainty & Expanded Data")),
        model = factor(model, levels = paste0("Model ", rep(1:4), rep(c(" (Pooled OLS)", " (System GMM)"), each = 2)))
    )



result_regime <- bind_rows(
    mutate(result_clsAJPS[["pooled-regime"]][[1]][,1:3], submodel = "Point Estimates Only"),
    mutate(MOCsumm(mocresult_correctAJPS[["pooled-regime"]]), submodel = "With Uncertainty"),
    mutate(MOCsumm(mocresult_expAJPS[["pooled-regime"]]), submodel = "Uncertainty & Expanded Data")
) %>% 
    mutate(type = "Pooled OLS",
           model = "Model 2 (Pooled OLS)")

result_regimeGmm <- bind_rows(
    mutate(result_clsAJPS[["gmm-regime"]][[1]][,1:3], submodel = "Point Estimates Only"),
    mutate(MOCsumm(mocresult_correctAJPS[["gmm-regime"]]), submodel = "With Uncertainty"),
    mutate(MOCsumm(mocresult_expAJPS[["gmm-regime"]]), submodel = "Uncertainty & Expanded Data")
)%>% 
    mutate(type = "System GMM",
           model = "Model 4 (System GMM)")

result_regimeAJPS <- bind_rows(result_regime, result_regimeGmm) %>%
    filter(term != "(Intercept)") %>%
    mutate(
        term0 = rep(
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
        type = factor(type, levels = c("Pooled OLS", "System GMM")),
        submodel = factor(submodel, levels = c("Point Estimates Only","With Uncertainty", "Uncertainty & Expanded Data")),
        model = factor(model, levels = paste0("Model ", rep(1:4), rep(c(" (Pooled OLS)", " (System GMM)"), each = 2))))


```

bind_rows(result_pooledAJPS, result_regimeAJPS) %>% 
    mutate(term = case_when(term0 == 'Democracy\n (t-1)' ~ "Democracy[t-1]",
                            term0 == 'Democracy\n (t-2)' ~ "Democracy[t-2]",
                            term0 == 'Support\n (t-1)' ~ "Democratic~Support[t-1]",
                            term0 == 'Support\n demo only\n (t-1)' ~ "Democratic~Support[t-1]~(democracies~only)",
                            term0 == 'Support\n auto only\n (t-1)' ~ "Democratic~Support[t-1]~(autocracies~only)",
                            term0 == 'Log GDP\n per capita\n (t-1)' ~ "Log~GDP~per~capita[t-1]",
                            term0 == 'GDP per\n capita growth\n (t-1)' ~ "GDP~per~capita~growth[t-1]",
                            term0 == 'Regional\n democracy\n (t-1)' ~ "Regional~Democracy[t-1]",
                            term0 == 'Percent\n Muslim\n (t-1)' ~ "Percent~Muslim[t-1]",
                            term0 == 'Resource\n dependence\n (t-1)' ~ "Resource~Dependence[t-1]",
                            TRUE ~ str_replace(term0, " ", "~")), 
           term = factor(term, levels =  c(
               "Democracy[t-1]",
               "Democracy[t-2]",
               "Democratic~Support[t-1]",
               "Democratic~Support[t-1]~(democracies~only)",
               "Democratic~Support[t-1]~(autocracies~only)",
               "Log~GDP~per~capita[t-1]",
               "GDP~per~capita~growth[t-1]",
               "Regional~Democracy[t-1]",
               "Percent~Muslim[t-1]",
               "Resource~Dependence[t-1]"))) %>% 
    small_multiple(dot_args = list(size = 0.2)) +
    theme_bw() +
    geom_hline(yintercept = 0, colour = "grey60", linetype = 2) +
    scale_color_grey(start = 0.4, end = 0.8, 
                     name = "Replication",
                     breaks=c("Original", "Uncertainty", "Uncertainty & More Data"),
                     labels=c("Point Estimates Only", "With Uncertainty", "Uncertainty & More Data")) +
    theme(axis.text.x  = element_text(angle = 90, hjust = 1),
          strip.text.y.left = element_text(angle = 0),
          legend.position=c(0, -.01), legend.justification=c(1,1), 
          legend.title = element_text(size=8),
          legend.text = element_text(size=8),
          legend.background = element_rect(color="gray90"),
          legend.spacing = unit(-3, "pt"),
          legend.key.size = unit(10, "pt"))

