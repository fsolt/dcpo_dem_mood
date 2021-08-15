
```{r formatResult-APSR-dup}
# Pooled---------------------------------------

result_ecm1 <- bind_rows(
    mutate(result_clsAPSR[["pooled1"]][[1]][,1:3], submodel = "Original"),
    mutate(MOCsumm2(mocresult_correctAPSR[["pooled1"]]), submodel = "Uncertainty"),
    mutate(MOCsumm2(mocresult_expAPSR[["pooled1"]]), submodel = "Uncertainty & More Data")
) %>% 
    mutate(type = "Error-Correction Model",
           model = "Model 1.1")


result_fd1 <- bind_rows(
    mutate(result_clsAPSR[["fd1"]][[1]][,1:3], submodel = "Original"),
    mutate(MOCsumm2(mocresult_correctAPSR[["fd1"]]), submodel = "Uncertainty"),
    mutate(MOCsumm2(mocresult_expAPSR[["fd1"]]), submodel = "Uncertainty & More Data")
) %>% 
    mutate(type = "First-Difference Model",
           model = "Model 1.3")

result_APSR1 <- bind_rows(result_ecm1, result_fd1) %>%
    filter(term != "(Intercept)") %>%
    mutate(
        term0 = c(rep(
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
        type = factor(type,  levels = c("Error-Correction Model", "First-Difference Model")),
        submodel = factor(submodel, levels = c("Original","Uncertainty", "Uncertainty & More Data")),
        model = factor(model, levels = paste0("Model ", rep(1:2, each = 4), ".", rep(1:4, 2)))
    )

# result_apsrPooled <- result_APSR1 %>% 
#     filter(type == "Error-Correction Model") %>% 
#     mutate(type = "Pooled")



# Institution----------------------------------

result_ecmReg1 <- bind_rows(
    mutate(result_clsAPSR[["pooled-regime1"]][[1]][,1:3], submodel = "Original"),
    mutate(MOCsumm2(mocresult_correctAPSR[["pooled-regime1"]]), submodel = "Uncertainty"),
    mutate(MOCsumm2(mocresult_expAPSR[["pooled-regime1"]]), submodel = "Uncertainty & More Data")
) %>% 
    mutate(type = "Error-Correction Model",
           model = "Model 2.1")


result_fdReg1 <- bind_rows(
    mutate(result_clsAPSR[["fd-regime1"]][[1]][,1:3], submodel = "Original"),
    mutate(MOCsumm2(mocresult_correctAPSR[["fd-regime1"]]), submodel = "Uncertainty"),
    mutate(MOCsumm2(mocresult_expAPSR[["fd-regime1"]]), submodel = "Uncertainty & More Data")
) %>% 
    mutate(type = "First-Difference Model",
           model = "Model 2.3")


result_RegAPSR1 <- bind_rows(result_ecmReg1, result_fdReg1) %>%
    filter(term != "(Intercept)") %>%
    mutate(
        term0 = c(rep(
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
        type = factor(type,  levels = c("Error-Correction Model", "First-Difference Model")),
        submodel = factor(submodel, levels = c("Original","Uncertainty", "Uncertainty & More Data")),
        model = factor(model, levels = paste0("Model ", rep(1:2, each = 4), ".", rep(1:4, 2)))
    )

# result_apsrRegime <- result_RegAPSR1 %>% 
#     filter(type == "Error-Correction Model") %>% 
#     mutate(type = "Institution Specified")


# Pooled-corruption------------------------------

result_ecm2 <- bind_rows(
    mutate(result_clsAPSR[["pooled2"]][[1]][,1:3], submodel = "Original"),
    mutate(MOCsumm2(mocresult_correctAPSR[["pooled2"]]), submodel = "Uncertainty"),
    mutate(MOCsumm2(mocresult_expAPSR[["pooled2"]]), submodel = "Uncertainty & More Data")
) %>% 
    mutate(type = "Error-Correction Model",
           model = "Model 1.2")

result_fd2 <- bind_rows(
    mutate(result_clsAPSR[["fd2"]][[1]][,1:3], submodel = "Original"),
    mutate(MOCsumm2(mocresult_correctAPSR[["fd2"]]), submodel = "Uncertainty"),
    mutate(MOCsumm2(mocresult_expAPSR[["fd2"]]), submodel = "Uncertainty & More Data")
) %>% 
    mutate(type = "First-Difference Model",
           model = "Model 1.4")

result_APSR2 <- bind_rows(result_ecm2, result_fd2) %>%
    filter(term != "(Intercept)") %>%
    mutate(
        term0 = c(rep(
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
        type = factor(type,  levels = c("Error-Correction Model", "First-Difference Model")),
        submodel = factor(submodel, levels = c("Original","Uncertainty", "Uncertainty & More Data")),
        model = factor(model, levels = paste0("Model ", rep(1:2, each = 4), ".", rep(1:4, 2)))
    )

# Institution Corruption--------------------------------

result_ecmReg2 <- bind_rows(
    mutate(result_clsAPSR[["pooled-regime2"]][[1]][,1:3], submodel = "Original"),
    mutate(MOCsumm2(mocresult_correctAPSR[["pooled-regime2"]]), submodel = "Uncertainty"),
    mutate(MOCsumm2(mocresult_expAPSR[["pooled-regime2"]]), submodel = "Uncertainty & More Data")) %>%
    mutate(type = "Error-Correction Model",
           model = "Model 2.2")

result_fdReg2 <- bind_rows(
    mutate(result_clsAPSR[["fd-regime2"]][[1]][,1:3], submodel = "Original"),
    mutate(MOCsumm2(mocresult_correctAPSR[["fd-regime2"]]), submodel = "Uncertainty"),
    mutate(MOCsumm2(mocresult_expAPSR[["fd-regime2"]]), submodel = "Uncertainty & More Data")) %>% 
    mutate(type = "First-Difference Model",
           model = "Model 2.4")

result_RegAPSR2 <- bind_rows(result_ecmReg2, result_fdReg2) %>%
    filter(term != "(Intercept)") %>%
    mutate(
        term0 = c(rep(
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
        type = factor(type,  levels = c("Error-Correction Model", "First-Difference Model")),
        submodel = factor(submodel, levels = c("Original","Uncertainty", "Uncertainty & More Data")),
        model = factor(model, levels = paste0("Model ", rep(1:2, each = 4), ".", rep(1:4, 2)))
    )
```

body(small_multiple)[[17]] <- substitute(
    p <- ggplot(df, aes(y = estimate, ymin = conf.low, ymax = conf.high, 
                        x = as.factor(model), colour = submodel)) +
        do.call(geom_pointrange, point_args) + ylab("") + xlab("") + 
        facet_grid(term ~ .,
                   scales = "free_y",
                   labeller = label_parsed,    # enable LaTeX facet labels
                   switch = "y") +             # put facet labels on left
        scale_y_continuous(position = "right") # put axis label on right
)

bind_rows(result_APSR1, result_APSR2, result_RegAPSR1, result_RegAPSR2) %>% 
    mutate(term = case_when(term0 == 'Democratic Mood\n (t-1)' ~ "Democratic~Support[t-1]",
                            term0 == 'Democratic Mood\n (t-2)' ~ "Democratic~Support[t-2]",
                            term0 == 'Liberal Democracy\n (Difference)' ~ "Delta~Liberal~Democracy",
                            term0 == 'Liberal Democracy\n (t-1)' ~ "Liberal~Democracy[t-1]",
                            term0 == 'Electoral Democracy\n (Difference)' ~ "Delta~Electoral~Democracy",
                            term0 == 'Electoral Democracy\n (t-1)' ~ "Electoral~Democracy[t-1]",
                            term0 == 'Minoritarian Democracy\n (Difference)' ~ "Delta~Minoritarian~Democracy",
                            term0 == 'Minoritarian Democracy\n (t-1)' ~ "Minoritarian~Democracy[t-1]",
                            term0 == 'Log GDP\n per capita\n (Difference)' ~ "Delta~Log~GDP~per~capita",
                            term0 == 'Log GDP\n (t-1)' ~ "Log~GDP[t-1]",
                            term0 == 'Corruption\n (Difference)' ~ "Delta~Corruption",
                            term0 == 'Corruption\n (t-1)' ~ "Corruption[t-1]",
                            TRUE ~ str_replace(term0, " ", "~")), 
           term = factor(term, levels = c("Democratic~Support[t-1]",
                                          "Democratic~Support[t-2]",
                                          "Delta~Liberal~Democracy",
                                          "Liberal~Democracy[t-1]",
                                          "Delta~Electoral~Democracy",
                                          "Electoral~Democracy[t-1]",
                                          "Delta~Minoritarian~Democracy",
                                          "Minoritarian~Democracy[t-1]",
                                          "Delta~Log~GDP~per~capita",
                                          "Log~GDP[t-1]",
                                          "Delta~Corruption",
                                          "Corruption[t-1]"))) %>% 
    filter(type == "Error-Correction Model") %>% 
    small_multiple() +
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





```{r plot-mocAPSR, fig.cap= "The Effect of Democracy on the Change of Public Support", eval = TRUE, fig.width=10, fig.height=10}
terms1 <- factor(rev(
    c(
        "Support[t-1]",
        "Support[t-2]",
        "Delta~Liberal~democracy",
        "Liberal~democracy[t-1]",
        "Delta~Log~GDP~per~capita",
        "Log~GDP[t-1]"
    )
),
levels = rev(
    c(
        "Support[t-1]",
        "Support[t-2]",
        "Delta~Liberal~democracy",
        "Liberal~democracy[t-1]",
        "Delta~Log~GDP~per~capita",
        "Log~GDP[t-1]"
    )
), 
labels = rev(
    c(
        "Support[t-1]",
        "Support[t-2]",
        "Delta~Liberal~Dem",
        "Liberal~Dem[t-1]",
        "Delta~Log~GDP~per~capita",
        "Log~GDP[t-1]"
    )
))

plot_APSR1 <-
    dwplot(result_APSR1, dodge_size = 0.8, by_2sd = FALSE) +
    scale_y_discrete(labels = parse(text = levels(terms1))) +
    labs(title = "DV: The Change of Public Support for Democracy",
         subtitle = "Pooled") +
    facet_wrap( ~ type, drop = T, scales = "free_x") +
    theme(legend.position = "none")  +
    scale_color_gb(palette = "digitMixed")


terms3 <- factor(rev(
    c(
        "Support[t-1]",
        "Support[t-2]",
        "Delta~Electoral~democracy",
        "Electoral~democracy[t-1]",
        "Delta~Minoritarian~democracy",
        "Minoritarian~democracy[t-1]",
        "Delta~Log~GDP~per~capita",
        "Log~GDP[t-1]"
    )
),
levels = rev(
    c(
        "Support[t-1]",
        "Support[t-2]",
        "Delta~Electoral~democracy",
        "Electoral~democracy[t-1]",
        "Delta~Minoritarian~democracy",
        "Minoritarian~democracy[t-1]",
        "Delta~Log~GDP~per~capita",
        "Log~GDP[t-1]"
    )
),
labels = rev(
    c(
        "Support[t-1]",
        "Support[t-2]",
        "Delta~Electoral",
        "Electoral[t-1]",
        "Delta~Minoritarian",
        "Minoritarian[t-1]",
        "Delta~Log~GDP~per~capita",
        "Log~GDP[t-1]"
    )
)
)

plot_RegAPSR1 <-
    dwplot(result_RegAPSR1, dodge_size = 0.8, by_2sd = FALSE) +
    scale_y_discrete(labels = parse(text = levels(terms3))) +
    labs(title = "",
         subtitle = "Institution Specified") +
    facet_wrap( ~ type, scales = "free_x") +
    theme(legend.position = "none")  +
    scale_color_gb(palette = "digitMixed")


terms2 <- factor(rev(
    c(
        "Support[t-1]",
        "Support[t-2]",
        "Delta~Liberal~democracy",
        "Liberal~democracy[t-1]",
        "Delta~Log~GDP~per~capita",
        "Log~GDP[t-1]",
        "Delta~Corruption",
        "Corruption[t-1]"
    )
),
levels = rev(
    c(
        "Support[t-1]",
        "Support[t-2]",
        "Delta~Liberal~democracy",
        "Liberal~democracy[t-1]",
        "Delta~Log~GDP~per~capita",
        "Log~GDP[t-1]",
        "Delta~Corruption",
        "Corruption[t-1]"
    )
),
labels = rev(
    c(
        "Support[t-1]",
        "Support[t-2]",
        "Delta~Liberal~Dem",
        "Liberal~Dem[t-1]",
        "Delta~Log~GDP~per~capita",
        "Log~GDP[t-1]",
        "Delta~Corruption",
        "Corruption[t-1]"
    )
))

plot_APSR2 <-
    dwplot(result_APSR2, dodge_size = 0.8, by_2sd = FALSE) +
    scale_y_discrete(labels = parse(text = levels(terms2))) +
    labs(title = "",
         subtitle = "W. Corruption") +
    facet_wrap( ~ type, scales = "free_x") +
    theme(legend.position = "none")  +
    scale_color_gb(palette = "digitMixed")


terms4 <- factor(rev(
    c(
        "Support[t-1]",
        "Support[t-2]",
        "Delta~Electoral~democracy",
        "Electoral~democracy[t-1]",
        "Delta~Minoritarian~democracy",
        "Minoritarian~democracy[t-1]",
        "Delta~Log~GDP",
        "Log~GDP[t-1]",
        "Delta~Corruption",
        "Corruption[t-1]"
    )
),
levels = rev(
    c(
        "Support[t-1]",
        "Support[t-2]",
        "Delta~Electoral~democracy",
        "Electoral~democracy[t-1]",
        "Delta~Minoritarian~democracy",
        "Minoritarian~democracy[t-1]",
        "Delta~Log~GDP",
        "Log~GDP[t-1]",
        "Delta~Corruption",
        "Corruption[t-1]"
    )
),
labels = rev(
    c(
        "Support[t-1]",
        "Support[t-2]",
        "Delta~Electoral",
        "Electoral[t-1]",
        "Delta~Minoritarian",
        "Minoritarian[t-1]",
        "Delta~Log~GDP",
        "Log~GDP[t-1]",
        "Delta~Corruption",
        "Corruption[t-1]"
    )
))

plot_RegAPSR2 <-
    dwplot(result_RegAPSR2, dodge_size = 0.8, by_2sd = FALSE) +
    scale_y_discrete(labels = parse(text = levels(terms4))) +
    labs(title = "",
         subtitle = "Institution Specified w. Corruption") +
    facet_wrap( ~ type, scales = "free_x") +
    theme(legend.position = "bottom",
          legend.title = element_blank())  +
    scale_color_gb(palette = "digitMixed")

legend <- get_legend(plot_RegAPSR2)

plot_RegAPSR2 <- plot_RegAPSR2 + theme(legend.position = "none")

grid.arrange(plot_APSR1,
             plot_APSR2,
             plot_RegAPSR1,
             plot_RegAPSR2,
             legend,
             ncol = 2)
