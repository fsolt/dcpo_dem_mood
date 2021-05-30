<<<<<<< HEAD
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


MOCsumm <- function (moc, digits=3,
                     ff=funs(est = mean, se = sd, z = mean(.)/sd(.),
                             pnorm = pNorm(.),
                             pemp = pEmp(.))) {
    
    var_names <- names(moc) %>% 
        str_c(collapse = "|")
    
    summarise_at(moc, .vars=vars(matches(var_names)), .funs=ff) %>%
        mutate_all(funs(round(., digits=digits))) %>%
        reshape2::melt(measure.vars=names(.))
}

MOCmean <- function (mod.ls) {
    plyr::llply(mod.ls, function (x) apply(x, 2, mean))
}

MOCsd <- function (mod.ls) {
    plyr::llply(mod.ls, function (x) apply(x, 2, sd))
}


MOCsumm(model.ls[[]])
=======
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


MOCsumm <- function (moc, regex=var_names, digits=3,
                     ff=funs(est = mean, se = sd, z = mean(.)/sd(.),
                             pnorm = pNorm(.),
                             pemp = pEmp(.))) {
    summarise_at(moc, .vars=var_names, .funs=ff) %>%
        mutate_all(funs(round(., digits=digits))) %>%
        reshape2::melt(measure.vars=names(.))
}


MOCmean <- function (mod.ls) {
    plyr::llply(mod.ls, function (x) apply(x, 2, mean))
}

MOCsd <- function (mod.ls) {
    plyr::llply(mod.ls, function (x) apply(x, 2, sd))
}


MOCsumm(model.ls[[]])
>>>>>>> 4dac44d45c2ae215a5086ef9f3c2e3d497d0aeaf
