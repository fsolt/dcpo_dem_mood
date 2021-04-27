source("../R/broom_utilities.R")

tidy.pgmm <- function(x,
                      conf.int = FALSE,
                      conf.level = 0.95,
                      ...) {
    result <- summary(x)$coefficients %>%
        tibble::as_tibble(rownames = "term") %>%
        dplyr::rename(
            estimate = Estimate,
            std.error = `Std. Error`,
            statistic = `z-value`,
            p.value = `Pr(>|z|)`
        )
    
    if (conf.int) {
        ci <- confint(x, level = conf.level) %>% 
            as.data.frame() %>% 
            rownames_to_column(var = "term") %>% 
            dplyr::rename(
                conf.low = `2.5 %`,
                conf.high = `97.5 %`
            )
        result <- dplyr::left_join(result, ci, by = "term")
    }
    
    result
}


glance.pgmm <- function(x, ...) {
    s <- summary(x)
    as_glance_tibble(
        chisq.sargan = unname(s$sargan$statistic),
        df.sargan = unname(s$sargan$parameter),
        p.sargan = unname(s$sargan$p.value),
        chisq.wald = unname(s$wald.coef$statistic),
        df.wald = unname(s$wald.coef$parameter),
        p.wald = unname(s$wald.coef$p.value),
        nobs = stats::nobs(x),
        na_types = "rrrrrii"
    )
}