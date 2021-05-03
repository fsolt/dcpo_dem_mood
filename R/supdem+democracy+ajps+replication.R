#### Does Public Support Help Democracy Survive?
#### Christopher Claassen
#### AJPS Replication file: main tables and figures

# install.packages(c('xtable','texreg','plm','pss','arm','dplyr','tidyr','ggplot2','RColorBrewer','rworldmap','rworldmap','statmod','AER','phtt','gtools','rgdal'))

library(xtable)
library(texreg)
library(plm)
library(pss)
library(arm)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(rworldmap)
library(statmod)
library(AER)
library(phtt)
library(gtools)
library(rgdal)

# Working directory
WD = getwd()
setwd(WD)

# Get data
supdem = read.csv("Support_democracy_ajps.csv")
v_dem = read.csv("vdem15.csv")

# Create panel datasets
sd.plm = pdata.frame(supdem, index = c("Country", "Year")) # full dataset 

# Summary table
vars = c("Libdem_VD", "SupDem_m1", "Year", "lnGDP_imp_m1", "GDP_imp_grth_m1", "Libdem_regUN_m1", "Res_cp_WDI_di_m1", 
         "Pr_Muslim", "Satis_m1", "lnInflat_imp", "Corrup_TI")
supdem.trim = supdem[complete.cases(supdem[, c("Libdem_VD", "SupDem_m1")]),]
sum.table = data.frame(
  Mean=sapply(supdem.trim[,vars], mean, na.rm=TRUE),
  SD=sapply(supdem.trim[,vars], sd, na.rm=TRUE),
  Min=sapply(supdem.trim[,vars], min, na.rm=TRUE),
  Max=sapply(supdem.trim[,vars], max, na.rm=TRUE),
  NAs=sapply(supdem.trim[,vars], function(x) sum(is.na(x))))
print(xtable(sum.table, digits=2), type="html", file="table_S1.html")


##### Models


## Full dynamic ADL models 

mod.1 = plm(Libdem_VD ~ plm::lag(Libdem_VD, 1:2) + plm::lag(SupDem_trim, 1) + plm::lag(lnGDP_imp, 1) 
            + plm::lag(GDP_imp_grth, 1) + plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1) 
            + plm::lag(Res_cp_WDI_di, 1), sd.plm, model="pooling")
mod.2 = plm(Libdem_VD ~ plm::lag(Libdem_VD, 1:2) + plm::lag(SupDem_Democ, 1) + plm::lag(SupDem_Autoc, 1) 
            + plm::lag(lnGDP_imp, 1) + plm::lag(GDP_imp_grth, 1) + plm::lag(Libdem_regUN, 1) 
            + plm::lag(Pr_Muslim, 1) + plm::lag(Res_cp_WDI_di, 1), sd.plm, model="pooling")
mod.3 = plm(Libdem_VD ~ plm::lag(Libdem_VD, 1:2) + plm::lag(SupDem_trim, 1) + plm::lag(Satis_trim, 1) 
            + plm::lag(lnGDP_imp, 1) + plm::lag(GDP_imp_grth, 1) + plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1) 
            + plm::lag(Res_cp_WDI_di, 1), sd.plm, model="pooling")
mod.4 = plm(Libdem_VD ~ plm::lag(Libdem_VD, 1:2) + plm::lag(SupDem_Democ, 1) + plm::lag(SupDem_Autoc, 1) 
            + plm::lag(Satis_Democ, 1) + plm::lag(Satis_Autoc, 1) + plm::lag(lnGDP_imp, 1) + plm::lag(GDP_imp_grth, 1) 
            + plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1) + plm::lag(Res_cp_WDI_di, 1), sd.plm, model="pooling")
mod.5 = plm(Libdem_VD ~ plm::lag(Libdem_VD, 1:2) + plm::lag(Satis_trim, 1) + plm::lag(lnGDP_imp, 1) + plm::lag(GDP_imp_grth, 1) 
            + plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1) + plm::lag(Res_cp_WDI_di, 1), sd.plm, model="pooling")
mod.6 = plm(Libdem_VD ~ plm::lag(Libdem_VD, 1:2) + plm::lag(Satis_Democ, 1) + plm::lag(Satis_Autoc, 1) + plm::lag(lnGDP_imp, 1) 
            + plm::lag(GDP_imp_grth, 1) + plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1) + plm::lag(Res_cp_WDI_di, 1), 
            sd.plm, model="pooling")
mod.7 = plm(Libdem_VD ~ plm::lag(Libdem_VD, 1:2) + plm::lag(SupDem_trim, 1) + plm::lag(lnGDP_imp, 1) 
            + plm::lag(GDP_imp_grth, 1) + plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1) + plm::lag(Res_cp_WDI_di, 1) 
            + plm::lag(lnInflat_imp, 1) + plm::lag(Corrup_TI, 1), sd.plm, model="pooling")
mod.8 = plm(Libdem_VD ~ plm::lag(Libdem_VD, 1:2) + plm::lag(SupDem_Democ, 1) + plm::lag(SupDem_Autoc, 1) 
            + plm::lag(lnGDP_imp, 1) + plm::lag(GDP_imp_grth, 1) + plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1) 
            + plm::lag(Res_cp_WDI_di, 1) + plm::lag(lnInflat_imp, 1) + plm::lag(Corrup_TI, 1), sd.plm, model="pooling")


# Main models

mods = list(mod.1, mod.2)
vcm = lapply(mods, function(x) plm::vcovBK(x, cluster="time")) # # Beck-Katz SEs
rse = lapply(vcm, function(x) sqrt(diag(x)))
pval = list()
for(i in 1:length(mods)) {pval[[i]] = pt(abs(coef(mods[[i]]) / rse[[i]]), df=2100, lower.tail=FALSE) * 2}
sapply(mods, function(x) round(sigma(x), 3))
sapply(mods, function(x) round(pbgtest(x, order=1)$p.value, 3)) # Wooldridge test for serial correlation
htmlreg(mods, override.se=rse, override.pvalues=pval, file="table_1A.html", digits=3, leading=FALSE, stars=c(0.05))

# Models with satisfaction

mods = list(mod.3, mod.4)
vcm = lapply(mods, function(x) plm::vcovBK(x, cluster="time")) # Beck-Katz SEs
rse = lapply(vcm, function(x) sqrt(diag(x)))
pval = list()
for(i in 1:length(mods)) {pval[[i]] = pt(abs(coef(mods[[i]]) / rse[[i]]), df=2100, lower.tail=FALSE) * 2}
sapply(mods, function(x) round(sigma(x), 3))
sapply(mods, function(x) round(pbgtest(x, order=1)$p.value, 3)) # Breusch-Godfrey/Wooldridge test for serial correlation
htmlreg(mods, override.se=rse, override.pvalues=pval, file="table_2A.html", digits=3, leading=FALSE, stars=c(0.05))

# Models with satisfaction and without support

mods = list(mod.5, mod.6)
vcm = lapply(mods, function(x) plm::vcovBK(x, cluster="time")) # Beck-Katz SEs
rse = lapply(vcm, function(x) sqrt(diag(x)))
pval = list()
for(i in 1:length(mods)) {pval[[i]] = pt(abs(coef(mods[[i]]) / rse[[i]]), df=2100, lower.tail=FALSE) * 2}
sapply(mods, function(x) round(sigma(x), 3))
sapply(mods, function(x) round(pbgtest(x, order=1)$p.value, 3)) # Breusch-Godfrey/Wooldridge test for serial correlation
htmlreg(mods, override.se=rse, override.pvalues=pval, file="table_S6A.html", digits=3, leading=FALSE, stars=c(0.05))

# With inflation and corruption (appendix)

mods = list(mod.7, mod.8)
vcm = lapply(mods, function(x) plm::vcovBK(x, cluster="group")) # Beck-Katz SEs
rse = lapply(vcm, function(x) sqrt(diag(x)))
pval = list()
for(i in 1:length(mods)) { pval[[i]] = pt(abs(coef(mods[[i]]) / rse[[i]]), df=2100, lower.tail=FALSE) * 2 }
screenreg(mods, override.se=rse, override.pvalues=pval, digits=3, single.row=FALSE, leading.zero=FALSE, stars=c(0.05))
sapply(mods, function(x) round(sigma(x), 3))
sapply(mods, function(x) round(pbgtest(x, order=1)$p.value, 3)) # Breusch-Godfrey/Wooldridge test for serial correlation
htmlreg(mods, override.se=rse, override.pvalues=pval, file="table_S2A.html", digits=3, leading=FALSE, stars=c(0.05))


## Long run effects

# mod 1
vcm.1 = plm::vcovBK(mod.1, cluster="time")
coef(mod.1)[4] / ( 1 - (coef(mod.1)[2] + coef(mod.1)[3]) ) # LRE
n.sims = 1e4
set.seed = 78
mod.1.sims = mvrnorm(n=n.sims, mu=coef(mod.1), Sigma=vcm.1)
lre.sim = mod.1.sims[,4] / ( 1 - (mod.1.sims[,2] + mod.1.sims[,3]) ) # 12.11
quantile(lre.sim, probs=c(0.025, 0.5, 0.975)) # 6.27 : 19.35

# mod 2
vcm.2 = plm::vcovBK(mod.2, cluster="time")
coef(mod.2)[4] / ( 1 - (coef(mod.2)[2] + coef(mod.2)[3]) ) # LRE
n.sims = 1e4
set.seed = 78
mod.2.sims = mvrnorm(n=n.sims, mu=coef(mod.2), Sigma=vcm.2)
# in democ
lre.sim = mod.2.sims[,4] / ( 1 - (mod.2.sims[,2] + mod.2.sims[,3]) ) # 14.78
quantile(lre.sim, probs=c(0.025, 0.5, 0.975)) # 7.39 : 26.78
# in autoc
lre.sim = mod.2.sims[,5] / ( 1 - (mod.2.sims[,2] + mod.2.sims[,3]) ) # 4.08
quantile(lre.sim, probs=c(0.025, 0.5, 0.975)) # -23.02 : 24.41


## Simulated effects (support up 1 SD)

# model 1

mod = plm(Libdem_VD ~ plm::lag(Libdem_VD, 1:2) + plm::lag(SupDem_trim, 1) + plm::lag(lnGDP_imp, 1) 
          + plm::lag(GDP_imp_grth, 1) + plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1) + plm::lag(Res_cp_WDI_di, 1), 
          sd.plm, model="pooling")
coef(mod)
n.coef =  length(coef(mod))
n.yrs = 231
n.burn = 200
coef(mod)[4] / (1 - (coef(mod)[2] + coef(mod)[3])) # lre

pred.data = data.frame(Intercept=1, Libdem_m1=45, Libdem_m2=45, 
                       SupDem_trim=c(rep(0,n.burn), rep(1,n.yrs-n.burn)), 
                       lnGDP_imp=mean(supdem$lnGDP_imp, na.rm=TRUE),
                       GDP_imp_grth=0,
                       Libdem_regUN=45,
                       Pr_Muslim=2,
                       Res_cp_WDI_di=0,
                       ChgDem=NA, Libdem=NA)

for(i in 1:(n.yrs-1)) {
  pred.data[i, "Libdem"] = sum(pred.data[i, 1:n.coef] * coef(mod))
  pred.data[i+1, "Libdem_m1"] = pred.data[i, "Libdem"]
  pred.data[i+1, "Libdem_m2"] = pred.data[i, "Libdem_m1"]
  pred.data[i, "ChgDem"] = pred.data[i, "Libdem"] - pred.data[i, "Libdem_m1"]
}

pred.data[230, "Libdem"] - pred.data[201, "Libdem"] # change over 30 years

n.sims = 1e4
mod.sims = mvrnorm(n=n.sims, mu=coef(mod), Sigma=plm::vcovBK(mod, cluster="time"))
yhat = matrix(NA, ncol=n.yrs, nrow=n.sims)
dem.hat = matrix(NA, ncol=n.yrs, nrow=n.sims)
for(i in 1:(n.yrs-1)) {
  yhat[, i] = mod.sims %*% t(pred.data[i, 1:n.coef])
  dem.hat[, i] = yhat[, i] + rnorm(n.sims, 0, sigma(mod))
}

pe = apply(dem.hat, 2, mean, na.rm=TRUE)
u95 = apply(dem.hat, 2, quantile, 0.975, na.rm=TRUE)
l95 = apply(dem.hat, 2, quantile, 0.025, na.rm=TRUE)
yr.st = n.burn - 9

pdf("figure_3A.pdf", height=4, width=4)
par(mfrow=c(1,1), mar=c(2.5,3,.5,.5), tcl=-0.25, cex=0.9)
plot(x=yr.st:(n.yrs-1), y=pe[yr.st:(n.yrs-1)], type="n", ylim=c(0, 100), 
     xaxs="i", yaxs="i", xaxt="n", yaxt="n", xlab="", ylab="")
abline(v=seq(210, 230, by=10), lty=3, col=rgb(0,0,0,.4))
abline(h=c(25, 50, 75), lty=3, col=rgb(0,0,0,.4))
abline(v=200, lty=2, col=rgb(0,0,0,.75))
axis(side=1, at=seq(200, 220, by=10), labels=seq(0, 20, by=10), cex.axis=0.8, las=1, mgp=c(0.7, 0.2, 0))
axis(side=2, at=c(25, 50, 75), cex.axis=0.8, las=1, mgp=c(1, 0.4, 0))
lines(x=yr.st:(n.yrs-1), y=pe[yr.st:(n.yrs-1)], col=rgb(0,0,0,1), lwd=2)
polygon(x=c(yr.st:(n.yrs-1), (n.yrs-1):yr.st), col=rgb(0,0,0,.5), border=NA, 
        y=c(u95[yr.st:(n.yrs-1)], l95[(n.yrs-1):yr.st]))
mtext(side=1, line=1.2, "Years after shock", cex=0.9)
mtext(side=2, line=1.6, "Level of democracy", cex=0.9)
text(x=199.5, y=4, "support increases one std. dev. in year 0", cex=0.9, pos=4)
dev.off()


# model 2

mod = plm(Libdem_VD ~ plm::lag(Libdem_VD, 1:2) + plm::lag(SupDem_Democ, 1) + plm::lag(SupDem_Autoc, 1) 
            + plm::lag(lnGDP_imp, 1) + plm::lag(GDP_imp_grth, 1) + plm::lag(Libdem_regUN, 1) 
            + plm::lag(Pr_Muslim, 1) + plm::lag(Res_cp_WDI_di, 1), sd.plm, model="pooling")
coef(mod)
n.coef =  length(coef(mod))
n.yrs = 231
n.burn = 200

pred.data.d = data.frame(Intercept=1, Libdem_m1=65, Libdem_m2=65, 
                         SupDem_Democ=c(rep(0,n.burn), rep(1,n.yrs-n.burn)), 
                         SupDem_Autoc=0,
                         lnGDP_imp=mean(supdem$lnGDP_imp, na.rm=TRUE),
                         GDP_imp_grth=0,
                         Libdem_regUN=65,
                         Pr_Muslim=2,
                         Res_cp_WDI_di=0,
                         ChgDem=NA, Libdem=NA)

for(i in 1:(n.yrs-1)) {
  pred.data.d[i, "Libdem"] = sum(pred.data.d[i, 1:n.coef] * coef(mod))
  pred.data.d[i+1, "Libdem_m1"] = pred.data.d[i, "Libdem"]
  pred.data.d[i+1, "Libdem_m2"] = pred.data.d[i, "Libdem_m1"]
  pred.data.d[i, "ChgDem"] = pred.data.d[i, "Libdem"] - pred.data.d[i, "Libdem_m1"]
}

pred.data.d[230, "Libdem"] - pred.data.d[201, "Libdem"] # change over 30 years

n.sims = 1e4
mod.sims = mvrnorm(n=n.sims, mu=coef(mod), Sigma=plm::vcovBK(mod, cluster="time"))
yhat.d = matrix(NA, ncol=n.yrs, nrow=n.sims)
dem.hat.d = matrix(NA, ncol=n.yrs, nrow=n.sims)
for(i in 1:(n.yrs-1)) {
  yhat.d[, i] = mod.sims %*% t(pred.data.d[i, 1:n.coef])
  dem.hat.d[, i] = yhat.d[, i] + rnorm(n.sims, 0, sigma(mod))
}

pe.d = apply(dem.hat.d, 2, mean, na.rm=TRUE)
u95.d = apply(dem.hat.d, 2, quantile, 0.975, na.rm=TRUE)
l95.d = apply(dem.hat.d, 2, quantile, 0.025, na.rm=TRUE)

pred.data.a = data.frame(Intercept=1, Libdem_m1=17, Libdem_m2=17, 
                         SupDem_Democ=0, 
                         SupDem_Autoc=c(rep(0,n.burn), rep(1,n.yrs-n.burn)),
                         lnGDP_imp=mean(supdem$lnGDP_imp, na.rm=TRUE),
                         GDP_imp_grth=0,
                         Libdem_regUN=17,
                         Pr_Muslim=2,
                         Res_cp_WDI_di=0,
                         ChgDem=NA, Libdem=NA)

for(i in 1:(n.yrs-1)) {
  pred.data.a[i, "Libdem"] = sum(pred.data.a[i, 1:n.coef] * coef(mod))
  pred.data.a[i+1, "Libdem_m1"] = pred.data.a[i, "Libdem"]
  pred.data.a[i+1, "Libdem_m2"] = pred.data.a[i, "Libdem_m1"]
  pred.data.a[i, "ChgDem"] = pred.data.a[i, "Libdem"] - pred.data.a[i, "Libdem_m1"]
}
pred.data.a[230, "Libdem"] - pred.data.a[201, "Libdem"] # change over 30 years

n.sims = 1e4
mod.sims = mvrnorm(n=n.sims, mu=coef(mod), Sigma=plm::vcovBK(mod, cluster="time"))
yhat.a = matrix(NA, ncol=n.yrs, nrow=n.sims)
dem.hat.a = matrix(NA, ncol=n.yrs, nrow=n.sims)
for(i in 1:(n.yrs-1)) {
  yhat.a[, i] = mod.sims %*% t(pred.data.a[i, 1:n.coef])
  dem.hat.a[, i] = yhat.a[, i] + rnorm(n.sims, 0, sigma(mod))
}

pe.a = apply(dem.hat.a, 2, mean, na.rm=TRUE)
u95.a = apply(dem.hat.a, 2, quantile, 0.975, na.rm=TRUE)
l95.a = apply(dem.hat.a, 2, quantile, 0.025, na.rm=TRUE)

yr.st = n.burn - 9

pdf("figure_3B.pdf", height=4, width=4)
par(mfrow=c(1,1), mar=c(2.5,3,.5,.5), tcl=-0.25, cex=0.9)
plot(x=yr.st:(n.yrs-1), y=pe.d[yr.st:(n.yrs-1)], type="n", ylim=c(0, 100), 
     xaxs="i", yaxs="i", xaxt="n", yaxt="n", xlab="", ylab="")
abline(v=seq(210, 220, by=10), lty=3, col=rgb(0,0,0,.4))
abline(h=c(25, 50, 75), lty=3, col=rgb(0,0,0,.4))
abline(v=200, lty=2, col=rgb(0,0,0,.75))
axis(side=1, at=seq(200, 220, by=10), labels=seq(0, 20, by=10), cex.axis=0.8, las=1, mgp=c(0.7, 0.2, 0))
axis(side=2, at=c(25, 50, 75), cex.axis=0.8, las=1, mgp=c(1, 0.4, 0))
lines(x=yr.st:(n.yrs-1), y=pe.d[yr.st:(n.yrs-1)], col=rgb(0,0,0,1), lwd=2)
polygon(x=c(yr.st:(n.yrs-1), (n.yrs-1):yr.st), col=rgb(0,0,0,.5), border=NA, 
        y=c(u95.d[yr.st:(n.yrs-1)], l95.d[(n.yrs-1):yr.st]))
text(x=231, y=pe.d[230]+6, "Effect within democracies", col=rgb(0,0,0,1), cex=0.9, pos=2)
lines(x=yr.st:(n.yrs-1), y=pe.a[yr.st:(n.yrs-1)], col=rgb(.35,.35,.35,1), lwd=2)
polygon(x=c(yr.st:(n.yrs-1), (n.yrs-1):yr.st), col=rgb(.35,.35,.35,.5), border=NA, 
        y=c(u95.a[yr.st:(n.yrs-1)], l95.a[(n.yrs-1):yr.st]))
text(x=231, y=pe.a[230]-10, "Effect within autocracies", col=rgb(.35,.35,.35,1), cex=0.9, pos=2)
mtext(side=1, line=1.2, "Years after shock", cex=0.9)
mtext(side=2, line=1.6, "Level of democracy", cex=0.9)
text(x=199.5, y=4, "support increases one std. dev. in year 0", cex=0.9, pos=4)
dev.off()



### System GMM models 

gmm.1 = pgmm(Libdem_VD ~ plm::lag(Libdem_VD, 1:2) + plm::lag(SupDem_trim, 1) + plm::lag(lnGDP_imp, 1) 
             + plm::lag(GDP_imp_grth, 1) + plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1)
             + plm::lag(Res_cp_WDI_di, 1) | plm::lag(Libdem_VD, 3:5), 
             sd.plm, effect="individual", model="onestep", transformation="ld", indexes=c("Country", "Year"))
gmm.2 = pgmm(Libdem_VD ~ plm::lag(Libdem_VD, 1:2) + plm::lag(SupDem_Democ, 1) + plm::lag(SupDem_Autoc, 1) 
             + plm::lag(lnGDP_imp, 1) + plm::lag(GDP_imp_grth, 1) + plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1)
             + plm::lag(Res_cp_WDI_di, 1) | plm::lag(Libdem_VD, 3:5), 
             sd.plm, effect="individual", model="onestep", transformation="ld", indexes=c("Country", "Year"))
gmm.3 = pgmm(Libdem_VD ~ plm::lag(Libdem_VD, 1:2) + plm::lag(SupDem_trim, 1) + plm::lag(Satis_trim, 1) 
             + plm::lag(lnGDP_imp, 1) + plm::lag(GDP_imp_grth, 1) + plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1)
             + plm::lag(Res_cp_WDI_di, 1) | plm::lag(Libdem_VD, 3:5), 
             sd.plm, effect="individual", model="onestep", transformation="ld", indexes=c("Country", "Year"))
gmm.4 = pgmm(Libdem_VD ~ plm::lag(Libdem_VD, 1:2) + plm::lag(SupDem_Democ, 1) + plm::lag(SupDem_Autoc, 1) 
             + plm::lag(Satis_Democ, 1) + plm::lag(Satis_Autoc, 1) + plm::lag(lnGDP_imp, 1) + plm::lag(GDP_imp_grth, 1) 
             + plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1) + plm::lag(Res_cp_WDI_di, 1) | plm::lag(Libdem_VD, 3:5), 
             sd.plm, effect="individual", model="onestep", transformation="ld", indexes=c("Country", "Year"))
gmm.5 = pgmm(Libdem_VD ~ plm::lag(Libdem_VD, 1:2) + plm::lag(Satis_trim, 1) + plm::lag(lnGDP_imp, 1) + plm::lag(GDP_imp_grth, 1) 
             + plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1) + plm::lag(Res_cp_WDI_di, 1) | plm::lag(Libdem_VD, 3:5), 
             sd.plm, effect="individual", model="onestep", transformation="ld", indexes=c("Country", "Year"))
gmm.6 = pgmm(Libdem_VD ~ plm::lag(Libdem_VD, 1:2) + plm::lag(Satis_Democ, 1) + plm::lag(Satis_Autoc, 1) + plm::lag(lnGDP_imp, 1) 
             + plm::lag(GDP_imp_grth, 1) + plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1) 
             + plm::lag(Res_cp_WDI_di, 1) | plm::lag(Libdem_VD, 3:5), 
             sd.plm, effect="individual", model="onestep", transformation="ld", indexes=c("Country", "Year"))
gmm.7 = pgmm(Libdem_VD ~ plm::lag(Libdem_VD, 1:2) + plm::lag(SupDem_trim, 1) 
             + plm::lag(lnGDP_imp, 1) + plm::lag(GDP_imp_grth, 1) + plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1) 
             + plm::lag(Res_cp_WDI_di, 1) + plm::lag(lnInflat_imp, 1) + plm::lag(Corrup_TI, 1) | plm::lag(Libdem_VD, 3:5), 
             sd.plm, effect="individual", model="onestep", transformation="ld", indexes=c("Country", "Year"))
gmm.8 = pgmm(Libdem_VD ~ plm::lag(Libdem_VD, 1:2) + plm::lag(SupDem_Democ, 1) + plm::lag(SupDem_Autoc, 1) 
             + plm::lag(lnGDP_imp, 1) + plm::lag(GDP_imp_grth, 1) + plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1) 
             + plm::lag(Res_cp_WDI_di, 1) + plm::lag(lnInflat_imp, 1) + plm::lag(Corrup_TI, 1) | plm::lag(Libdem_VD, 3:5), 
             sd.plm, effect="individual", model="onestep", transformation="ld", indexes=c("Country", "Year"))

# Main models

mods = list(gmm.1, gmm.2)
vcm = lapply(mods, plm::vcovHC)
rse = lapply(vcm, function(x) sqrt(diag(x)))
pval = list()
for(i in 1:length(mods)) { pval[[i]] = pt(abs(coef(mods[[i]]) / rse[[i]]), df=2100, lower.tail=FALSE) * 2 }
(n.inst = sapply(mods, function(x) dim(x$W[[1]])[2]))
sapply(mods, function(x) mtest(x, order=1, vcov=plm::vcovHC(x))[["p.value"]][[1]] )
sapply(mods, function(x) mtest(x, order=2, vcov=plm::vcovHC(x))[["p.value"]][[1]] ) # AR(2) p-val
sapply(mods, function(x) round(sargan(x, weights="twosteps")[["p.value"]][[1]], 3)) # Hansen test p-val
htmlreg(mods, override.se=rse, override.pvalues=pval, file="table_1B.html", digits=3, leading=FALSE, stars=c(0.05))

# Models with satisfaction and support

mods = list(gmm.3, gmm.4)
vcm = lapply(mods, plm::vcovHC)
rse = lapply(vcm, function(x) sqrt(diag(x)))
pval = list()
for(i in 1:length(mods)) { pval[[i]] = pt(abs(coef(mods[[i]]) / rse[[i]]), df=2100, lower.tail=FALSE) * 2 }
(n.inst = sapply(mods, function(x) dim(x$W[[1]])[2]))
sapply(mods, function(x) mtest(x, order=1, vcov=plm::vcovHC(x))[["p.value"]][[1]] )
round(sapply(mods, function(x) mtest(x, order=2, vcov=plm::vcovHC(x))[["p.value"]][[1]] ), 3) # AR(2) p-val
sapply(mods, function(x) round(sargan(x, weights="twosteps")[["p.value"]][[1]], 3)) # Hansen test p-val
htmlreg(mods, override.se=rse, override.pvalues=pval, file="table_2B.html", digits=3, leading=FALSE, stars=c(0.05))

# Models with satisfaction, without support

mods = list(gmm.5, gmm.6)
vcm = lapply(mods, plm::vcovHC)
rse = lapply(vcm, function(x) sqrt(diag(x)))
pval = list()
for(i in 1:length(mods)) { pval[[i]] = pt(abs(coef(mods[[i]]) / rse[[i]]), df=2100, lower.tail=FALSE) * 2 }
(n.inst = sapply(mods, function(x) dim(x$W[[1]])[2]))
sapply(mods, function(x) mtest(x, order=1, vcov=plm::vcovHC(x))[["p.value"]][[1]] )
round(sapply(mods, function(x) mtest(x, order=2, vcov=plm::vcovHC(x))[["p.value"]][[1]] ), 3) # AR(2) p-val
sapply(mods, function(x) round(sargan(x, weights="twosteps")[["p.value"]][[1]], 3)) # Hansen test p-val
htmlreg(mods, override.se=rse, override.pvalues=pval, file="table_S6B.html", digits=3, leading=FALSE, stars=c(0.05))
#texreg(mods, override.se=rse, override.pvalues=pval, digits=3, leading=FALSE, stars=c(0.05), dcolumn=TRUE)


# Models with inflation and corruption (appendix)

mods = list(gmm.7, gmm.8)
vcm = lapply(mods, plm::vcovHC)
rse = lapply(vcm, function(x) sqrt(diag(x)))
pval = list()
for(i in 1:length(mods)) { pval[[i]] = pt(abs(coef(mods[[i]]) / rse[[i]]), df=2100, lower.tail=FALSE) * 2 }
(n.inst = lapply(mods, function(x) dim(x$W[[1]])[2]))
sapply(mods, function(x) round(mtest(x, order=1, vcov=plm::vcovHC(x))[["p.value"]][[1]], 3))
sapply(mods, function(x) round(mtest(x, order=2, vcov=plm::vcovHC(x))[["p.value"]][[1]], 3)) # AR(2) p-val
sapply(mods, function(x) round(sargan(x, weights="twosteps")[["p.value"]][[1]], 3)) # Hansen test p-val
htmlreg(mods, override.se=rse, override.pvalues=pval, file="table_S2B.html", digits=3, leading=FALSE, stars=c(0.05))


## Long run effects

# gmm 1
vcm.1 = plm::vcovHC(gmm.1)
coef(gmm.1)[3] / (1 - (coef(gmm.1)[1] + coef(gmm.1)[2])) # LRE
n.sims = 1e4
set.seed=78
mod.1.sims = mvrnorm(n=n.sims, mu=coef(gmm.1), Sigma=vcm.1)
lre.sim = mod.1.sims[,3] / (1 - (mod.1.sims[,1] + mod.1.sims[,2])) # 7.83
quantile(lre.sim, probs=c(0.025, 0.5, 0.975)) # 4.30 : 15.91

# gmm 2
vcm.2 = plm::vcovHC(gmm.2)
coef(gmm.2)[3] / (1 - (coef(gmm.2)[1] + coef(gmm.2)[2])) # LRE
n.sims = 1e4
set.seed=78
mod.2.sims = mvrnorm(n=n.sims, mu=coef(gmm.2), Sigma=vcm.2)
# in democ
lre.sim = mod.2.sims[,3] / (1 - (mod.2.sims[,1] + mod.2.sims[,2])) # 7.64
quantile(lre.sim, probs=c(0.025, 0.5, 0.975)) # 3.02 : 19.85
# in autoc
lre.sim = mod.2.sims[,4] / (1 - (mod.2.sims[,1] + mod.2.sims[,2])) # 8.85
quantile(lre.sim, probs=c(0.025, 0.5, 0.975)) # -10.93 : 27.04


## Interactive Fixed Effects (Bai 2009) using phtt() library

ld_wide = as.matrix(spread(supdem[, c("Year", "Country", "Libdem_VD")], key="Country", value="Libdem_VD"))
ldm1_wide = as.matrix(spread(supdem[, c("Year", "Country", "Libdem_m1")], key="Country", value="Libdem_m1"))
ldm2_wide = as.matrix(spread(supdem[, c("Year", "Country", "Libdem_m2")], key="Country", value="Libdem_m2"))
sd_wide = as.matrix(spread(supdem[, c("Year", "Country", "SupDem_m1")], key="Country", value="SupDem_m1"))
lg_wide = as.matrix(spread(supdem[, c("Year", "Country", "lnGDP_imp_m1")], key="Country", value="lnGDP_imp_m1"))
gg_wide = as.matrix(spread(supdem[, c("Year", "Country", "GDP_imp_grth_m1")], key="Country", value="GDP_imp_grth_m1"))
rd_wide = as.matrix(spread(supdem[, c("Year", "Country", "Libdem_regUN_m1")], key="Country", value="Libdem_regUN_m1"))
res_wide = as.matrix(spread(supdem[, c("Year", "Country", "Res_cp_WDI_di_m1")], key="Country", value="Res_cp_WDI_di_m1"))
sdd_wide = as.matrix(spread(supdem[, c("Year", "Country", "SupDem_democ_m1")], key="Country", value="SupDem_democ_m1"))
sda_wide = as.matrix(spread(supdem[, c("Year", "Country", "SupDem_autoc_m1")], key="Country", value="SupDem_autoc_m1"))

# 10 years, 122 countries

eup_1.1 = Eup(ld_wide[,!is.na(sd_wide[22,])] ~ ldm1_wide[,!is.na(sd_wide[22,])] + ldm2_wide[,!is.na(sd_wide[22,])] 
              + sd_wide[,!is.na(sd_wide[22,])] + lg_wide[,!is.na(sd_wide[22,])] + gg_wide[,!is.na(sd_wide[22,])] 
              + rd_wide[,!is.na(sd_wide[22,])] + res_wide[,!is.na(sd_wide[22,])], max.iteration=1000, convergence=1e-7)
sum_eup_1.1 = summary(eup_1.1, error.type=3)
sum_eup_1.1
print(xtable(data.frame(sum_eup_1.1[[2]][,1:2]), digits=3), type="html", file="table_S5A.html")

eup_2.1 = Eup(ld_wide[,!is.na(sd_wide[22,])] ~ ldm1_wide[,!is.na(sd_wide[22,])] + ldm2_wide[,!is.na(sd_wide[22,])] 
              + sdd_wide[,!is.na(sd_wide[22,])] + sda_wide[,!is.na(sd_wide[22,])] + lg_wide[,!is.na(sd_wide[22,])] 
              + gg_wide[,!is.na(sd_wide[22,])] + rd_wide[,!is.na(sd_wide[22,])] + res_wide[,!is.na(sd_wide[22,])], 
              max.iteration=1000, convergence=1e-7)
sum_eup_2.1 = summary(eup_2.1, error.type=3)
sum_eup_2.1
print(xtable(data.frame(sum_eup_2.1[[2]][,1:2]), digits=3), type="html", file="table_S5B.html")

# 20 years, 66 countries

eup_1.2 = Eup(ld_wide[,!is.na(sd_wide[12,])] ~ ldm1_wide[,!is.na(sd_wide[12,])] + ldm2_wide[,!is.na(sd_wide[12,])] 
              + sd_wide[,!is.na(sd_wide[12,])] + lg_wide[,!is.na(sd_wide[12,])] + gg_wide[,!is.na(sd_wide[12,])] 
              + rd_wide[,!is.na(sd_wide[12,])] + res_wide[,!is.na(sd_wide[12,])], max.iteration=1000, convergence=1e-7)
sum_eup_1.2 = summary(eup_1.2, error.type=3)
sum_eup_1.2
print(xtable(data.frame(sum_eup_1.2[[2]][,1:2]), digits=3), type="html", file="table_S5C.html")

eup_2.2 = Eup(ld_wide[,!is.na(sd_wide[12,])] ~ ldm1_wide[,!is.na(sd_wide[12,])] + ldm2_wide[,!is.na(sd_wide[12,])] 
              + sdd_wide[,!is.na(sd_wide[12,])] + sda_wide[,!is.na(sd_wide[12,])] + lg_wide[,!is.na(sd_wide[12,])] 
              + gg_wide[,!is.na(sd_wide[12,])] + rd_wide[,!is.na(sd_wide[12,])] + res_wide[,!is.na(sd_wide[12,])], 
              max.iteration=1000, convergence=1e-7)
sum_eup_2.2 = summary(eup_2.2, error.type=3)
sum_eup_2.2
print(xtable(data.frame(sum_eup_2.2[[2]][,1:2]), digits=3), type="html", file="table_S5D.html")


## Models with democratic change (Teorell)

mod.chg = plm(diff(Libdem_VD, 1) ~ plm::lag(Libdem_VD, 1:2) + plm::lag(SupDem_trim, 1) + plm::lag(lnGDP_imp, 1) 
              + plm::lag(GDP_imp_grth, 1) + plm::lag(Libdem_regUN, 1)  + plm::lag(Pr_Muslim, 1) 
              + plm::lag(Res_cp_WDI_di, 1), sd.plm, model="pooling")
mod.up.1 = plm(UpChgDem ~ plm::lag(Libdem_VD, 1:2) + plm::lag(SupDem_trim, 1) + plm::lag(lnGDP_imp, 1) 
               + plm::lag(GDP_imp_grth, 1) + plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1) 
               + plm::lag(Res_cp_WDI_di, 1), sd.plm, model="pooling")
mod.dn.1 = plm(DwnChgDem ~ plm::lag(Libdem_VD, 1:2) + plm::lag(SupDem_trim, 1) + plm::lag(lnGDP_imp, 1) 
               + plm::lag(GDP_imp_grth, 1) + plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1) 
               + plm::lag(Res_cp_WDI_di, 1), sd.plm, model="pooling")

mods = list(mod.chg, mod.up.1, mod.dn.1)
vcm = lapply(mods, function(x) plm::vcovHC(x, method="arellano", cluster="group")) # SEs robust to serial correlation
rse = lapply(vcm, function(x) sqrt(diag(x)))
pval = list()
for(i in 1:length(mods)) {pval[[i]] = pt(abs(coef(mods[[i]]) / rse[[i]]), df=2100, lower.tail=FALSE) * 2}
sapply(mods, function(x) round(sigma(x), 3))
sapply(mods, function(x) round(pbgtest(x, order=1)$p.value, 3)) # Breusch-Godfrey/Wooldridge test for serial correlation
htmlreg(mods, override.se=rse, override.pvalues=pval, file="table_S3.html", digits=3, leading=FALSE, stars=c(0.05))


## Electoral democracy as dependent variable

# Pooled OLS models
ed.mod.1 = plm(Polyarchy_VD ~ plm::lag(Polyarchy_VD, 1:2) + plm::lag(SupDem_trim, 1) + plm::lag(lnGDP_imp, 1) 
            + plm::lag(GDP_imp_grth, 1) + plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1) 
            + plm::lag(Res_cp_WDI_di, 1), sd.plm, model="pooling")
ed.mod.2 = plm(Polyarchy_VD ~ plm::lag(Polyarchy_VD, 1:2) + plm::lag(SupDem_Democ, 1) + plm::lag(SupDem_Autoc, 1) 
            + plm::lag(lnGDP_imp, 1) + plm::lag(GDP_imp_grth, 1) + plm::lag(Libdem_regUN, 1) 
            + plm::lag(Pr_Muslim, 1) + plm::lag(Res_cp_WDI_di, 1), sd.plm, model="pooling")
mods = list(ed.mod.1, ed.mod.2)
vcm = lapply(mods, function(x) plm::vcovHC(x, method="arellano", cluster="group")) # SEs robust to serial correlation
rse = lapply(vcm, function(x) sqrt(diag(x)))
pval = list()
for(i in 1:length(mods)) {pval[[i]] = pt(abs(coef(mods[[i]]) / rse[[i]]), df=2100, lower.tail=FALSE) * 2}
sapply(mods, function(x) round(sigma(x), 3))
sapply(mods, function(x) round(pbgtest(x, order=1)$p.value, 3)) # Wooldridge test for serial correlation
htmlreg(mods, override.se=rse, override.pvalues=pval, digits=3, file="table_S4A.html", leading.zero=FALSE, stars=c(0.05))

# GMM models
ed.gmm.1 = pgmm(Polyarchy_VD ~ plm::lag(Polyarchy_VD, 1:2) + plm::lag(SupDem_trim, 1) + plm::lag(lnGDP_imp, 1) 
             + plm::lag(GDP_imp_grth, 1) + plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1)
             + plm::lag(Res_cp_WDI_di, 1) | plm::lag(Polyarchy_VD, 3:5), 
             sd.plm, effect="individual", model="onestep", transformation="ld", indexes=c("Country", "Year"))
ed.gmm.2 = pgmm(Polyarchy_VD ~ plm::lag(Polyarchy_VD, 1:2) + plm::lag(SupDem_Democ, 1) + plm::lag(SupDem_Autoc, 1) 
             + plm::lag(lnGDP_imp, 1) + plm::lag(GDP_imp_grth, 1) + plm::lag(Libdem_regUN, 1) + plm::lag(Pr_Muslim, 1)
             + plm::lag(Res_cp_WDI_di, 1) | plm::lag(Polyarchy_VD, 3:5), 
             sd.plm, effect="individual", model="onestep", transformation="ld", indexes=c("Country", "Year"))
mods = list(ed.gmm.1, ed.gmm.2)
vcm = lapply(mods, plm::vcovHC)
rse = lapply(vcm, function(x) sqrt(diag(x)))
pval = list()
for(i in 1:length(mods)) { pval[[i]] = pt(abs(coef(mods[[i]]) / rse[[i]]), df=2100, lower.tail=FALSE) * 2 }
(n.inst = sapply(mods, function(x) dim(x$W[[1]])[2]))
sapply(mods, function(x) mtest(x, order=2, vcov=plm::vcovHC(x))[["p.value"]][[1]] ) # AR(2) p-val
sapply(mods, function(x) round(sargan(x, weights="twosteps")[["p.value"]][[1]], 3)) # Hansen test p-val
htmlreg(mods, override.se=rse, override.pvalues=pval, digits=3, file="table_S4B.html", leading.zero=FALSE, stars=c(0.05))




##### Figures


## Maps of support 

# 2015
map.pal = brewer.pal(n=9, name="Greys")[c(3,5,7,9)]

supdem$SupDem4 = gtools::quantcut(supdem$SupDem_trim, q=4, 
                                  labels=c("lower quartile", "2nd quartile", "3rd quartile", "upper quartile"))
mapdat = supdem[supdem$Year==2015, c("ISO_code", "SupDem4")]
mapdat$ISO_code[is.na(mapdat$ISO_code)] = "TWN"
sPDF = getMap()[-which(getMap()$ADMIN=="Antarctica"),] # create Robinson Projection
sPDF = spTransform(sPDF, CRS=CRS("+proj=robin +ellps=WGS84"))
mapdat_RP = merge(sPDF, mapdat, by.x="ISO3", by.y="ISO_code", all.x=TRUE)

pdf("figure_1B.pdf", width=8, height=4)
par(mar=c(1,0,1.5,0), xaxs="i", yaxs="i", cex=0.85)
mapCountryData(mapdat_RP, nameColumnToPlot="SupDem4", mapTitle="2015",  
               colourPalette=map.pal, borderCol="darkgrey", lwd=0.75, addLegend=TRUE)
dev.off()

# 2005
mapdat = supdem[supdem$Year==2005, c("ISO_code", "SupDem4")]
sPDF = getMap()[-which(getMap()$ADMIN=="Antarctica"),] # create Robinson Projection
sPDF = spTransform(sPDF, CRS=CRS("+proj=robin +ellps=WGS84"))
mapdat_RP = merge(sPDF, mapdat, by.x="ISO3", by.y="ISO_code", all.x=TRUE)

pdf("figure_1A.pdf", width=8, height=4)
par(mar=c(1,0,1.5,0), xaxs="i", yaxs="i", cex=0.85)
mapCountryData(mapdat_RP, nameColumnToPlot="SupDem4", mapTitle="2005", colourPalette=map.pal, 
               borderCol="darkgrey", lwd=0.75, addLegend=TRUE)
dev.off()


## Maps of satisfaction 

# 2015
map.pal = brewer.pal(n=9, name="Greys")[c(3,5,7,9)]
supdem$Satis4 = gtools::quantcut(supdem$Satis_trim, q=4, 
                                 labels=c("lower quartile", "2nd quartile", "3rd quartile", "upper quartile"))
mapdat = supdem[supdem$Year==2015, c("ISO_code", "Satis4")]
sPDF = getMap()[-which(getMap()$ADMIN=="Antarctica"),] # create Robinson Projection
sPDF = spTransform(sPDF, CRS=CRS("+proj=robin +ellps=WGS84"))
mapdat_RP = merge(sPDF, mapdat, by.x="ISO3", by.y="ISO_code", all.x=TRUE)

pdf("figure_S9B.pdf", width=8, height=4)
par(mar=c(1,0,1.5,0), xaxs="i", yaxs="i", cex=0.85)
mapCountryData(mapdat_RP, nameColumnToPlot="Satis4", mapTitle="2015",  
               colourPalette=map.pal, borderCol="darkgrey", lwd=0.75, addLegend=TRUE)
dev.off()

# 2005
mapdat = supdem[supdem$Year==2005, c("ISO_code", "Satis4")]
sPDF = getMap()[-which(getMap()$ADMIN=="Antarctica"),] # create Robinson Projection
sPDF = spTransform(sPDF, CRS=CRS("+proj=robin +ellps=WGS84"))
mapdat_RP = merge(sPDF, mapdat, by.x="ISO3", by.y="ISO_code", all.x=TRUE)

pdf("figure_S9A.pdf", width=8, height=4)
par(mar=c(1,0,1.5,0), xaxs="i", yaxs="i", cex=0.85)
mapCountryData(mapdat_RP, nameColumnToPlot="Satis4", mapTitle="2005", colourPalette=map.pal, 
               borderCol="darkgrey", lwd=0.75, addLegend=TRUE)
dev.off()


## Support by VDem Polico-Geographic Regions

# Drop Belize and Malta: no v_dem
supdem$Region_VD = factor(supdem$Region_VD, labels=
        c("Eastern Europe & Central Asia", "Latin America", "Middle East & North Africa", "Sub-Saharan Africa", 
          "The West", "East Asia", "South-East Asia", "South Asia", "Caribbean"))

regs = levels(supdem$Region_VD)
pal = brewer.pal(12, "Set3")

pdf("figure_S6.pdf", width=8, height=5)
par(mfrow=c(3,3), mar=c(1, 1.2, 1.6, .2), tcl=-0.2, cex=1)
for (i in 1:9) {
  plot(y=rep(0, 30), x=1988:2017, type="n", xlab="", ylab="", ylim=c(-2.5,3), xlim=c(1987,2018), xaxs="i",
       xaxt="n", yaxt="n", main="", bty="n")
  abline(h=c(-2,0,2), lty=3, lwd=.75, col=rgb(0,0,0,0.3))
  abline(v=c(1990, 1995, 2000, 2005, 2010, 2015), lty=3, lwd=.75, col=rgb(0,0,0,0.3))
  cnts = unique(supdem[supdem$Region_VD==regs[i], "Country"])
  for (j in 1:length(cnts)) {
    lines(y=supdem[supdem$Country==cnts[j], "SupDem_trim"][2:31], x=1988:2017, col=rgb(0,0,0,.35), lwd=1.2, lty=1)
  }
  lines(y=as.numeric(by(supdem[supdem$Region_VD==regs[i], "SupDem_trim"],
                        supdem[supdem$Region_VD==regs[i], "Year"], mean, na.rm=TRUE))[2:31],
        x=1988:2017, col=rgb(0,0,0,.9), lwd=2.8, lty=1)
  axis(1, at=c(1990, 1995, 2000, 2005, 2010, 2015), cex.axis=.75, mgp=c(1,.07,0), lwd=0.8)
  axis(2, at=c(-2, 0, 2), cex.axis=.75, mgp=c(1.4,.32,0), las=1, lwd=0.8)
  mtext(regs[i], side=3, line=0.18, cex=0.9)
  box(lwd=0.75)
}
dev.off()


## Satisfaction by VDem Polico-Geographic Regions

# Drop Belize and Malta: no v_dem
supdem$Region_VD = factor(supdem$Region_VD, labels=
        c("Eastern Europe & Central Asia", "Latin America", "Middle East & North Africa", "Sub-Saharan Africa", 
          "The West", "East Asia", "South-East Asia", "South Asia", "Caribbean"))

regs = levels(supdem$Region_VD)
pal = brewer.pal(12, "Set3")

pdf("figure_S8.pdf", width=8, height=5)
par(mfrow=c(3,3), mar=c(1, 1.2, 1.6, .2), tcl=-0.2, cex=1)
for (i in 1:9) {
  plot(y=rep(0, 30), x=1988:2017, type="n", xlab="", ylab="", ylim=c(-2.5,3), xlim=c(1987,2018), xaxs="i",
       xaxt="n", yaxt="n", main="", bty="n")
  abline(h=c(-2,0,2), lty=3, lwd=.75, col=rgb(0,0,0,0.3))
  abline(v=c(1990, 1995, 2000, 2005, 2010, 2015), lty=3, lwd=.75, col=rgb(0,0,0,0.3))
  cnts = unique(supdem[supdem$Region_VD==regs[i], "Country"])
  for (j in 1:length(cnts)) {
    lines(y=supdem[supdem$Country==cnts[j], "Satis_trim"][2:31], x=1988:2017, col=rgb(0,0,0,.35), lwd=1.2, lty=1)
  }
  lines(y=as.numeric(by(supdem[supdem$Region_VD==regs[i], "Satis_trim"], 
                        supdem[supdem$Region_VD==regs[i], "Year"], mean, na.rm=TRUE))[2:31],
        x=1988:2017, col=rgb(0,0,0,.9), lwd=2.8, lty=1)
  axis(1, at=c(1990, 1995, 2000, 2005, 2010, 2015), cex.axis=.75, mgp=c(1,.07,0), lwd=0.8)
  axis(2, at=c(-2, 0, 2), cex.axis=.75, mgp=c(1.4,.32,0), las=1, lwd=0.8)
  mtext(regs[i], side=3, line=0.18, cex=0.9)
  box(lwd=0.75)
}
dev.off()



## V-dem VS FH / Polity plots

pdf("figure_S1A.pdf", width=8, height=2.5)
par(mfrow=c(1,3), mar=c(1.8,3,1,.2), mgp=c(1.8,.5,0), tcl=-0.3, las=1)
hist(v_dem$libdem, ylim=c(0.5, 35), xlab="", main="", breaks=15, cex.axis=0.9, cex.lab=1.2, xaxt="n")
axis(1, mgp=c(1,.3,0), cex.axis=0.9, lwd=0.75)
grid(lty=3, lwd=1, col=rgb(0,0,0,0.3))
hist(v_dem$libdem, breaks=15, col=grey(0.5), add=TRUE)
text(x=45, y=34, adj=0.5, labels="V-Dem Liberal Democracy 2015", cex=1.2)
box()
hist(v_dem$e_polity2, ylim=c(0.5, 35), xlab="", ylab="", main="", breaks=22, cex.axis=0.9, xaxt="n")
axis(1, mgp=c(1,.3,0), cex.axis=0.9, lwd=0.75)
grid(lty=3, lwd=1, col=rgb(0,0,0,0.3))
hist(v_dem$e_polity2, breaks=22, col=grey(0.5), add=TRUE)
text(x=0, y=34, adj=0.5, labels="Polity IV 2015", cex=1.2)
box()
hist(v_dem$fh, ylim=c(0.5, 35), xlab="", ylab= "", main="", breaks=14, cex.axis=0.9, xaxt="n")
axis(1, mgp=c(1,.3,0), cex.axis=0.9, lwd=0.75)
grid(lty=3, lwd=1, col=rgb(0,0,0,0.3))
hist(v_dem$fh, breaks=14, col=grey(0.5), add=TRUE)
text(x=8, y=34, adj=0.5, labels="Freedom House 2015", cex=1.2)
box()
dev.off()

vdem.plot1 = v_dem[, c("country_text_id", "libdem", "e_polity2")]
vdem.plot1 = vdem.plot1[complete.cases(vdem.plot1),]
vdem.plot2 = v_dem[, c("country_text_id", "libdem", "fh")]
vdem.plot2 = vdem.plot2[complete.cases(vdem.plot2),]

pdf("figure_S1B.pdf", width=8, height=4)
par(mfrow=c(1,2), mar=c(2.2,2,.2,.2), mgp=c(1.5,.5,0), tcl=-0.2, las=1)
plot(x=vdem.plot1$libdem, y=vdem.plot1$e_polity2, type="n", axes=FALSE, xlab="", ylab="")
grid(lty=3, lwd=1, col=rgb(0,0,0,0.3))
text(x=vdem.plot1$libdem, y=vdem.plot1$e_polity2, labels=vdem.plot1$country_text_id, col=rgb(0,0,0,0.75), cex=0.5)
axis(side=1, at=c(0, 20, 40, 60, 80), mgp=c(1,.1,0), cex.axis=0.75, lwd=0.75)
axis(side=2, at=c(-10, -5, 0, 5, 10), mgp=c(1,.3,0), cex.axis=0.75, lwd=0.75)
mtext(side=1, line=1.1, at=95, adj=0.5, text="V-Dem Liberal Democracy 2015", cex=0.8)
mtext(side=2, line=1.1, at=0, adj=0.5, text="Polity IV 2015", cex=0.8, las=0)
lines(lowess(x=vdem.plot1$libdem, y=vdem.plot1$e_polity2), col="darkblue", lwd=2)
box()
plot(x=vdem.plot2$libdem, y=vdem.plot2$fh, type="n", axes=FALSE, xlab="", ylab="")
grid(lty=3, lwd=1, col=rgb(0,0,0,0.3))
text(x=vdem.plot2$libdem, y=vdem.plot2$fh, labels=vdem.plot2$country_text_id, col=rgb(0,0,0,0.75), cex=0.5)
axis(side=1, at=c(0, 20, 40, 60, 80), mgp=c(1,.1,0), cex.axis=0.75, lwd=0.75)
axis(side=2, at=seq(2, 14, by=2), mgp=c(1,.3,0), cex.axis=0.75, lwd=0.75)
mtext(side=2, line=1.1, at=8, adj=0.5, text="Freedom House 2015", cex=0.8, las=0)
lines(lowess(x=vdem.plot2$libdem, y=vdem.plot2$fh), col="darkblue", lwd=2)
box()
dev.off()


