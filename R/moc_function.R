####Method of composition
vcovBK <- function(mod) {
  plm::vcovBK(mod, cluster = "time") 
}

plmHC <- function (mod) {
  plm::vcovHC(mod, method="arellano", cluster="group")
}


###For Ajps pooled ols, using vcov=vcovBK
methodComposition1 <- function (data, model, vcov=vcovBK, rsq=TRUE) {
  aData <- data[[1]]
  aMod <- eval(parse(text = model))
  
  coefdf <- as.data.frame(matrix(nrow=length(data), ncol=length(coef(aMod))))
  R2 <- data.frame(rsq=rep(NA, length(data)), adjrsq=rep(NA, length(data)))
  
  for (s in seq_along(data)) {
    ## (1) Sample from p(x)
    data_sample <- data[[s]]
    ## (2) Sample from p(B|x,y):
    ##     (a) Estimate B_s and Cov(B_s) conditional on x_s.
    mod_sample <- update(aMod, data=data_sample)
    hatB_sample <- coef(mod_sample)
    hatV_sample <- vcov(mod_sample)
    ##     (b) Sample \data_sample{B_s} from MV(\hat{B_sample}, \hat{Cov(B_sample)}).
    coefdf[s, ] <- MASS::mvrnorm(n = 1, mu = hatB_sample, Sigma = hatV_sample)
    if (rsq) {
      R2$rsq[s] <- summary(mod_sample)$r.squared["rsq"]
      R2$adjrsq[s] <- summary(mod_sample)$r.squared["adjrsq"]
    }
  }
  names(coefdf) <- names(coef(aMod))
  if (rsq) {
    coefdf$rsq <- R2$rsq
    coefdf$adjrsq <- R2$adjrsq
  }
  return(coefdf)
}


### For AJPS GMM model, using plmHC. 
methodComposition2 <- function (data, model, vcov=plmHC, rsq=TRUE) {
  aData <- data[[1]]
  aMod <- eval(parse(text = model))
  
  coefdf <- as.data.frame(matrix(nrow=length(data), ncol=length(coef(aMod))))
  R2 <- data.frame(rsq=rep(NA, length(data)), adjrsq=rep(NA, length(data)))
  
  for (s in seq_along(data)) {
    data_sample <- data[[s]]
    mod_sample <- update(aMod, data=data_sample)
    hatB_sample <- coef(mod_sample)
    hatV_sample <- vcov(mod_sample)
      coefdf[s, ] <- MASS::mvrnorm(n = 1, mu = hatB_sample, Sigma = hatV_sample)
  
  }
  names(coefdf) <- names(coef(aMod))
  return(coefdf)
}

##### For APSR ECM and First Difference models, using plmhc. 
methodComposition3 <- function (data, model, vcov=plmHC, rsq=TRUE) {
  #df_dcpoUncertainAPSR <- data[[1]]
  aData <- data[[1]]
  aMod <- eval(parse(text = model))
  
  coefdf <- as.data.frame(matrix(nrow=length(data), ncol=length(coef(aMod))))
  R2 <- data.frame(rsq=rep(NA, length(data)), adjrsq=rep(NA, length(data)))
  
  for (s in seq_along(data)) {
    data_sample <- data[[s]]
    mod_sample <- update(aMod, data=data_sample)
    hatB_sample <- coef(mod_sample)
    hatV_sample <- vcov(mod_sample)
    coefdf[s, ] <- MASS::mvrnorm(n = 1, mu = hatB_sample, Sigma = hatV_sample)
    if (rsq) {
      R2$rsq[s] <- summary(mod_sample)$r.squared["rsq"]
      R2$adjrsq[s] <- summary(mod_sample)$r.squared["adjrsq"]
    }
  }
  names(coefdf) <- names(coef(aMod))
  if (rsq) {
    coefdf$rsq <- R2$rsq
    coefdf$adjrsq <- R2$adjrsq
  }
  return(coefdf)
}


