####Method of composition


plmHC <- function (mod) {
  plm::vcovHC(mod, method="arellano", cluster="group")
}


methodComposition <- function (data, iter, model, vcov=plmHC, rsq=TRUE) {
  coefdf <- as.data.frame(matrix(nrow=length(iter), ncol=length(coef(model))))
  R2 <- data.frame(rsq=rep(NA, length(iter)), adjrsq=rep(NA, length(iter)))
  for (s in seq_along(iter)) {
    ## (1) Sample from p(x)
    it <- iter[s]
    data_sample <- subset(data, iter_se == it)
    ## (2) Sample from p(B|x,y):
    ##     (a) Estimate B_s and Cov(B_s) conditional on x_s.
    mod_sample <- update(model, data=data_sample)
    hatB_sample <- coef(mod_sample)
    hatV_sample <- vcov(mod_sample)
    ##     (b) Sample \data_sample{B_s} from MV(\hat{B_sample}, \hat{Cov(B_sample)}).
    coefdf[s, ] <- MASS::mvrnorm(n = 1, mu = hatB_sample, Sigma = hatV_sample)
    if (rsq) {
      R2$rsq[s] <- summary(mod_sample)$r.squared["rsq"]
      R2$adjrsq[s] <- summary(mod_sample)$r.squared["adjrsq"]
    }
  }
  names(coefdf) <- names(coef(model))
  if (rsq) {
    coefdf$rsq <- R2$rsq
    coefdf$adjrsq <- R2$adjrsq
  }
  return(coefdf)
}


###for analysis
iter <- sort(sample(unique(data$iter), min(900, length(unique(data$iter)))))