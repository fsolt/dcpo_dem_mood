
###Moc summary

pEmp <- function (x) {
    p_pos <- mean(x < 0)
    p_neg <- mean(x > 0)
    p2side <- 2 * min(p_pos, p_neg)
    p2side
}
pNorm <- function (x) {
    z <- abs(mean(x)/sd(x))
    p2side <- 2*(1 - pnorm(z))
    p2side
}


##For AJPS model summary. 
MOCsumm <- function (moc, digits=3,
                     ff=funs(est = mean, se = sd, z = mean(.)/sd(.),
                             pnorm = pNorm(.),
                             pemp = pEmp(.))) {
    
    var_names <- names(moc) %>% 
        str_c(collapse = "|")
    
    summarise_all(moc, .funs=ff) %>%
        mutate_all(funs(round(., digits=digits))) %>%
        reshape2::melt(measure.vars=names(.)) %>%
        separate(variable, into = c("term","temp"),sep ="_(?=est|se|z|pnorm|pemp)") %>%
        pivot_wider(names_from = "temp",values_from = "value") %>%
        transmute(term = term,
                  estimate = est,
                  std.error = se) %>%
        filter(!term =="rsq"&!term =="adjrsq" )
}


##For APSR model Summaries
MOCsumm2 <- function (moc, digits=3,
                      ff=funs(est = mean, se = sd)) {
    var_names <- names(moc) %>% 
        str_c(collapse = "|")
    
    summarise_all(moc, .funs=ff) %>%
        mutate_all(funs(round(., digits=digits))) %>%
        reshape2::melt(measure.vars=names(.)) %>%
        separate(variable, into = c("term","temp"),sep ="_(?=est|se)") %>%
        pivot_wider(names_from = "temp",values_from = "value") %>%
        transmute(term = term,
                  estimate = est,
                  std.error = se) %>%
        filter(!term =="rsq"&!term =="adjrsq" )
}

