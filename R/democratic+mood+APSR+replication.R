#### In the Mood for Democracy
#### Christopher Claassen
#### APSR Replication file: main tables and figures

# install.packages(c('xtable','texreg','plm','dplyr','tidyr','ggplot2','RColorBrewer','AER','MASS','lme4'))

library(xtable)
library(texreg)
library(plm)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(AER)
library(MASS)
library(lme4)


# Setup working directory

WD = getwd()
setwd(WD)

# Get data

supdem = read.csv("dem_mood_apsr.csv")
sd.plm = pdata.frame(supdem, index = c("Country", "Year")) 



##### Models


# ECM models with Lib Dem index

mod1 = plm(diff(SupDem_trim, lag=1) ~ plm::lag(SupDem_trim, 1:2) + diff(Libdem_z, lag=1) 
           + plm::lag(Libdem_z, 1) + diff(lnGDP_imp, lag=1) + plm::lag(lnGDP_imp, 1), 
           sd.plm, model="pooling")
mod2 = plm(diff(SupDem_trim, lag=1) ~ plm::lag(SupDem_trim, 1:2) + diff(Libdem_z, lag=1) 
           + plm::lag(Libdem_z, 1) + diff(lnGDP_imp, lag=1) + plm::lag(lnGDP_imp, 1) 
           + diff(Corrup_TI_z, lag=1) + plm::lag(Corrup_TI_z, 1), sd.plm, model="pooling")

mods = list(mod1, mod2)
vcm = lapply(mods, function(x) plm::vcovHC(x, method="arellano", cluster="group"))
rse = lapply(vcm, function(x) sqrt(diag(x)))
pval = list()
for(i in 1:length(mods)) {pval[[i]] = pt(abs(coef(mods[[i]]) / rse[[i]]), df=2500, lower.tail=FALSE) * 2}
screenreg(mods, override.se=rse, override.pvalues=pval, digits=3, single.row=FALSE, leading.zero=FALSE, 
          stars=c(0.05))

# FD models with Lib Dem index

fd1 = plm(SupDem_trim ~ Libdem_z + lnGDP_imp, model="fd", data=sd.plm, index="Country")
fd2 = plm(SupDem_trim ~ Libdem_z + lnGDP_imp + Corrup_TI_z, model="fd", data=sd.plm, index="Country")

mods = list(fd1, fd2)
vcm = lapply(mods, function(x) plm::vcovHC(x, method="arellano", cluster="group"))
rse = lapply(vcm, function(x) sqrt(diag(x)))
pval = list()
for(i in 1:length(mods)) {pval[[i]] = pt(abs(coef(mods[[i]]) / rse[[i]]), df=2500, lower.tail=FALSE) * 2}
screenreg(mods, override.se=rse, override.pvalues=pval, digits=3, single.row=FALSE, leading.zero=FALSE, 
          stars=c(0.05))

# Table 1
mods = list(mod1, mod2, fd1, fd2)
vcm = lapply(mods, function(x) plm::vcovHC(x, method="arellano", cluster="group"))
rse = lapply(vcm, function(x) sqrt(diag(x)))
pval = list()
for(i in 1:length(mods)) {pval[[i]] = pt(abs(coef(mods[[i]]) / rse[[i]]), df=2500, lower.tail=FALSE) * 2}
sapply(mods, function(x) round(sigma(x), 3))
sapply(mods, function(x) round(pbgtest(x, order=1)$p.value, 3)) # Wooldridge test for serial correlation
varnams = c("Intercept", "Democratic mood_t-1", "Democratic mood_t-2", "Delta Liberal democracy", 
            "Liberal democracy_t-1", "Delta log GDP per capita", "log GDP per capita_t-1", "Delta corruption", 
            "Corruption_t-1",  "Delta Liberal democracy", "Delta log GDP per capita", "Delta corruption")
texreg(mods, file="table1_tex.txt", override.se=rse, override.pvalues=pval, digits=3, single.row=FALSE, 
       stars=c(0.05), leading.zero=FALSE, dcolumn=TRUE, booktabs=TRUE, custom.coef.names=varnams)


### Simulated effects

# Model 1.1: corruption excluded

mod = plm(diff(SupDem_trim, lag=1) ~ plm::lag(SupDem_trim, 1:2) + diff(Libdem_z, lag=1) 
          + plm::lag(Libdem_z, 1) + diff(lnGDP_imp, lag=1) + plm::lag(lnGDP_imp, 1), 
          sd.plm, model="pooling")
coef(mod)
n.coef =  length(coef(mod))
n.yrs = 130
n.burn = 100
pred.data = data.frame(Intercept=1, SupDem_m1=0, SupDem_m2=0, 
                       ChgDem=0, 
                       Libdem_m1=c(rep(0, n.burn), rep(1, n.yrs-n.burn)), 
                       ChglogGDP=0,
                       lnGDP_m1=mean(supdem$lnGDP_imp, na.rm=TRUE),
                       ChgSup=NA, SupDem=NA)

for(i in 1:(n.yrs-1)) {
  pred.data[i, "ChgDem"] = pred.data[i+1, "Libdem_m1"] - pred.data[i, "Libdem_m1"]
  pred.data[i, "ChgSup"] = sum(pred.data[i, 1:n.coef] * coef(mod))
  pred.data[i, "SupDem"] = pred.data[i, "SupDem_m1"] + pred.data[i, "ChgSup"]
  pred.data[i+1, "SupDem_m1"] = pred.data[i, "SupDem"]
  pred.data[i+1, "SupDem_m2"] = pred.data[i, "SupDem_m1"]
}

n.sims = 1e4
mod.sims = mvrnorm(n=n.sims, mu=coef(mod), Sigma=plm::vcovHC(mod))
yhat = matrix(NA, ncol=n.yrs, nrow=n.sims)
sup.hat = matrix(NA, ncol=n.yrs, nrow=n.sims)
for(i in 1:(n.yrs-1)) {
  yhat[, i] = mod.sims %*% t(pred.data[i, 1:n.coef])
  sup.hat[, i] = pred.data[i, "SupDem_m1"] + yhat[, i] + rnorm(n.sims, 0, sigma(mod))
}

pe = apply(sup.hat, 2, mean, na.rm=TRUE)
u95 = apply(sup.hat, 2, quantile, 0.975, na.rm=TRUE)
l95 = apply(sup.hat, 2, quantile, 0.025, na.rm=TRUE)
yr.st = n.burn - 9

pdf("fig5A.pdf", height=4, width=4)
par(mfrow=c(1,1), mar=c(2,3,1.8,.5), tcl=-0.25, cex=0.9)
plot(x=yr.st:(n.yrs-1), y=pe[yr.st:(n.yrs-1)], type="n", ylim=c(-1.5, 0), 
     xaxs="i", xaxt="n", yaxt="n", xlab="", ylab="")
abline(v=seq(110, 130, by=10), lty=3, col=rgb(0,0,0,.4))
abline(h=c(-1.5, -1, -0.5, 0), lty=3, col=rgb(0,0,0,.4))
abline(v=100, lty=2, col=rgb(0,0,0,.75))
axis(side=1, at=seq(100, 130, by=10), labels=seq(0, 30, by=10), cex.axis=0.8, las=1, mgp=c(0.7, 0.2, 0))
axis(side=2, at=c(-1.5, -1, -0.5, 0), cex.axis=0.8, las=1, mgp=c(1, 0.4, 0))
lines(x=yr.st:(n.yrs-1), y=pe[yr.st:(n.yrs-1)], col=rgb(0,.4,.4,1), lwd=2)
polygon(x=c(yr.st:(n.yrs-1), (n.yrs-1):yr.st), col=rgb(0,.4,.4,.5), border=NA, 
        y=c(u95[yr.st:(n.yrs-1)], l95[(n.yrs-1):yr.st]))
mtext(side=1, line=1.1, "Years", cex=0.9)
mtext(side=2, line=1.8, "Democratic mood", cex=0.9)
mtext(side=3, line=0.3, "Model 1.1", cex=0.9, adj=0)
text(x=99.5, y=-0.1, "Democracy increases 1 SD in 1 year", cex=0.85, pos=4)
dev.off()

# Model 1.2: corruption included

mod = plm(diff(SupDem_trim, lag=1) ~ plm::lag(SupDem_trim, 1:2) + diff(Libdem_z, lag=1) 
          + plm::lag(Libdem_z, 1) + diff(lnGDP_imp, lag=1) + plm::lag(lnGDP_imp, 1) 
          + diff(Corrup_TI_z, lag=1) + plm::lag(Corrup_TI_z, 1), sd.plm, model="pooling")
coef(mod)
n.coef =  length(coef(mod))
n.yrs = 130
n.burn = 100
pred.data = data.frame(Intercept=1, SupDem_m1=0, SupDem_m2=0, 
                       ChgDem=0, 
                       Libdem_m1=c(rep(-0.5,n.burn), rep(0.5,n.yrs-n.burn)), 
                       ChglogGDP=0,
                       lnGDP_m1=mean(supdem$lnGDP_imp, na.rm=TRUE),
                       ChgCorr=0,
                       Corrup_m1=0,
                       ChgSup=NA, SupDem=NA)

for(i in 1:(n.yrs-1)) {
  pred.data[i, "ChgDem"] = pred.data[i+1, "Libdem_m1"] - pred.data[i, "Libdem_m1"]
  pred.data[i, "ChgSup"] = sum(pred.data[i, 1:n.coef] * coef(mod))
  pred.data[i, "SupDem"] = pred.data[i, "SupDem_m1"] + pred.data[i, "ChgSup"]
  pred.data[i+1, "SupDem_m1"] = pred.data[i, "SupDem"]
  pred.data[i+1, "SupDem_m2"] = pred.data[i, "SupDem_m1"]
}

n.sims = 1e4
mod.sims = mvrnorm(n=n.sims, mu=coef(mod), Sigma=plm::vcovHC(mod))
yhat = matrix(NA, ncol=n.yrs, nrow=n.sims)
sup.hat = matrix(NA, ncol=n.yrs, nrow=n.sims)
for(i in 1:(n.yrs-1)) {
  yhat[, i] = mod.sims %*% t(pred.data[i, 1:n.coef])
  sup.hat[, i] = pred.data[i, "SupDem_m1"] + yhat[, i] + rnorm(n.sims, 0, sigma(mod))
}

pe = apply(sup.hat, 2, mean, na.rm=TRUE)
u95 = apply(sup.hat, 2, quantile, 0.975, na.rm=TRUE)
l95 = apply(sup.hat, 2, quantile, 0.025, na.rm=TRUE)
yr.st = n.burn - 9

pdf("fig5B.pdf", height=4, width=4)
par(mfrow=c(1,1), mar=c(2,3,1.8,.5), tcl=-0.25, cex=0.9)
plot(x=yr.st:(n.yrs-1), y=pe[yr.st:(n.yrs-1)], type="n", ylim=c(-1, 0.5), 
     xaxs="i", xaxt="n", yaxt="n", xlab="", ylab="")
abline(v=seq(110, 130, by=10), lty=3, col=rgb(0,0,0,.4))
abline(h=c(-1, -0.5, 0, 0.5), lty=3, col=rgb(0,0,0,.4))
abline(v=100, lty=2, col=rgb(0,0,0,.75))
axis(side=1, at=seq(100, 130, by=10), labels=seq(0, 30, by=10), cex.axis=0.8, las=1, mgp=c(0.7, 0.2, 0))
axis(side=2, at=c(-1, -0.5, 0, 0.5), cex.axis=0.8, las=1, mgp=c(1, 0.4, 0))
lines(x=yr.st:(n.yrs-1), y=pe[yr.st:(n.yrs-1)], col=rgb(0,.4,.4,1), lwd=2)
polygon(x=c(yr.st:(n.yrs-1), (n.yrs-1):yr.st), col=rgb(0,.4,.4,.5), border=NA, 
        y=c(u95[yr.st:(n.yrs-1)], l95[(n.yrs-1):yr.st]))
mtext(side=1, line=1.1, "Years", cex=0.9)
mtext(side=2, line=1.8, "Democratic mood", cex=0.9)
mtext(side=3, line=0.3, "Model 1.2", cex=0.9, adj=0)
text(x=99.5, y=0.4, "Democracy increases 1 SD in 1 year", cex=0.85, pos=4)
dev.off()



### Models including electoral democracy and liberalism 

# ECMs

mod2.1 = plm(diff(SupDem_trim, lag=1) ~ plm::lag(SupDem_trim, 1:2) + diff(Polyarchy_z, lag=1) 
             + plm::lag(Polyarchy_z, 1) + diff(Liberal_z, lag=1) + plm::lag(Liberal_z, 1) 
             + diff(lnGDP_imp, lag=1) + plm::lag(lnGDP_imp, 1), 
             sd.plm, model="pooling")
mod2.2 = plm(diff(SupDem_trim, lag=1) ~ plm::lag(SupDem_trim, 1:2) + diff(Polyarchy_z, lag=1) 
             + plm::lag(Polyarchy_z, 1) + diff(Liberal_z, lag=1) + plm::lag(Liberal_z, 1) 
             + diff(lnGDP_imp, lag=1) + plm::lag(lnGDP_imp, 1) + diff(Corrup_TI_z, lag=1) 
             + plm::lag(Corrup_TI_z, 1), sd.plm, model="pooling")

# FD models

fd2.1 = plm(SupDem_trim ~ Polyarchy_z + Liberal_z + lnGDP_imp, model="fd", data=sd.plm, index="Country")
fd2.2 = plm(SupDem_trim ~ Polyarchy_z + Liberal_z + lnGDP_imp + Corrup_TI_z, model="fd", 
            data=sd.plm, index="Country")

# Table 2
mods = list(mod2.1, mod2.2, fd2.1, fd2.2)
vcm = lapply(mods, function(x) plm::vcovHC(x, method="arellano", cluster="group"))
rse = lapply(vcm, function(x) sqrt(diag(x)))
pval = list()
for(i in 1:length(mods)) {pval[[i]] = pt(abs(coef(mods[[i]]) / rse[[i]]), df=2500, lower.tail=FALSE) * 2}
sapply(mods, function(x) round(sigma(x), 3))
sapply(mods, function(x) round(pbgtest(x, order=1)$p.value, 3)) # Wooldridge test for 1st order serial correlation
sapply(mods, function(x) round(pbgtest(x, order=2)$p.value, 3)) # Wooldridge test for 2nd order serial correlation
varnams = c("Intercept", "Democratic mood_t-1", "Democratic mood_t-2", "Delta electoral democracy", 
            "Electoral democracy_t-1", "Delta minoritarian democracy", "Minoritarian democracy_t-1", 
            "Delta log GDP per capita", "log GDP per capita_t-1", "Delta corruption", "Corruption_t-1", 
            "Delta electoral democracy", "Delta minoritarian democracy", "Delta log GDP per capita", 
            "Delta corruption")
texreg(mods, file="table2_tex.txt", override.se=rse, override.pvalues=pval, digits=3, single.row=FALSE, 
       stars=c(0.05), leading.zero=FALSE, dcolumn=TRUE, booktabs=TRUE, custom.coef.names=varnams)


### Simulated effects of change in electoral and minoritarian democracy

## Electoral democracy up 1 SD

# Model 2.1: corruption excluded

mod = plm(diff(SupDem_trim, lag=1) ~ plm::lag(SupDem_trim, 1:2) + diff(Polyarchy_z, lag=1) 
          + plm::lag(Polyarchy_z, 1) + diff(Liberal_z, lag=1) + plm::lag(Liberal_z, 1) 
          + diff(lnGDP_imp, lag=1) + plm::lag(lnGDP_imp, 1), sd.plm, model="pooling")
coef(mod)
n.coef =  length(coef(mod))
n.yrs = 130
n.burn = 100
pred.data = data.frame(Intercept=1, SupDem_m1=0, SupDem_m2=0, 
                       ChgPoly=0, 
                       Poly_m1=c(rep(-0.5,n.burn), rep(0.5,n.yrs-n.burn)), 
                       ChgLib=0, 
                       Liberal_m1=0,                        
                       ChglogGDP=0,
                       lnGDP_m1=mean(supdem$lnGDP_imp, na.rm=TRUE),
                       ChgSup=NA, SupDem=NA)

for(i in 1:(n.yrs-1)) {
  pred.data[i, "ChgPoly"] = pred.data[i+1, "Poly_m1"] - pred.data[i, "Poly_m1"]
  pred.data[i, "ChgSup"] = sum(pred.data[i, 1:n.coef] * coef(mod))
  pred.data[i, "SupDem"] = pred.data[i, "SupDem_m1"] + pred.data[i, "ChgSup"]
  pred.data[i+1, "SupDem_m1"] = pred.data[i, "SupDem"]
  pred.data[i+1, "SupDem_m2"] = pred.data[i, "SupDem_m1"]
}

n.sims = 1e4
mod.sims = mvrnorm(n=n.sims, mu=coef(mod), Sigma=plm::vcovHC(mod))
yhat = matrix(NA, ncol=n.yrs, nrow=n.sims)
sup.hat = matrix(NA, ncol=n.yrs, nrow=n.sims)
for(i in 1:(n.yrs-1)) {
  yhat[, i] = mod.sims %*% t(pred.data[i, 1:n.coef])
  sup.hat[, i] = pred.data[i, "SupDem_m1"] + yhat[, i] + rnorm(n.sims, 0, sigma(mod))
}

pe = apply(sup.hat, 2, mean, na.rm=TRUE)
u95 = apply(sup.hat, 2, quantile, 0.975, na.rm=TRUE)
l95 = apply(sup.hat, 2, quantile, 0.025, na.rm=TRUE)
yr.st = n.burn - 9

pdf("fig6A.pdf", height=4, width=4)
par(mfrow=c(1,1), mar=c(2,3,1.8,.5), tcl=-0.25, cex=0.9)
plot(x=yr.st:(n.yrs-1), y=pe[yr.st:(n.yrs-1)], type="n", ylim=c(-1.5, 0), 
     xaxs="i", xaxt="n", yaxt="n", xlab="", ylab="")
abline(v=seq(110, 130, by=10), lty=3, col=rgb(0,0,0,.4))
abline(h=c(-1.5, -1, -0.5, 0), lty=3, col=rgb(0,0,0,.4))
abline(v=100, lty=2, col=rgb(0,0,0,.75))
axis(side=1, at=seq(100, 130, by=10), labels=seq(0, 30, by=10), cex.axis=0.8, las=1, mgp=c(0.7, 0.2, 0))
axis(side=2, at=c(-1.5, -1, -0.5, 0), cex.axis=0.8, las=1, mgp=c(1, 0.4, 0))
lines(x=yr.st:(n.yrs-1), y=pe[yr.st:(n.yrs-1)], col=rgb(0,.4,.4,1), lwd=2)
polygon(x=c(yr.st:(n.yrs-1), (n.yrs-1):yr.st), col=rgb(0,.4,.4,.5), border=NA, 
        y=c(u95[yr.st:(n.yrs-1)], l95[(n.yrs-1):yr.st]))
mtext(side=1, line=1.1, "Years", cex=0.9)
mtext(side=2, line=1.8, "Democratic mood", cex=0.9)
mtext(side=3, line=0.3, "Model 2.1", cex=0.9, adj=0)
text(x=99.5, y=-0.1, "Electoral democracy increases 1 SD\nin 1 year", cex=0.85, pos=4)
dev.off()

# Model 2.2: corruption included

mod = plm(diff(SupDem_trim, lag=1) ~ plm::lag(SupDem_trim, 1:2) + diff(Polyarchy_z, lag=1) 
          + plm::lag(Polyarchy_z, 1) + diff(Liberal_z, lag=1) + plm::lag(Liberal_z, 1) 
          + diff(lnGDP_imp, lag=1) + plm::lag(lnGDP_imp, 1) + diff(Corrup_TI_z, lag=1) 
          + plm::lag(Corrup_TI_z, 1), sd.plm, model="pooling")
coef(mod)
n.coef =  length(coef(mod))
n.yrs = 130
n.burn = 100
pred.data = data.frame(Intercept=1, SupDem_m1=0, SupDem_m2=0, 
                       ChgPoly=0, 
                       Poly_m1=c(rep(-0.5,n.burn), rep(0.5,n.yrs-n.burn)), 
                       ChgLib=0, 
                       Liberal_m1=0,                        
                       ChglogGDP=0,
                       lnGDP_m1=mean(supdem$lnGDP_imp, na.rm=TRUE),
                       ChgCorr=0,
                       Corrup_m1=mean(supdem$Corrup_TI_z, na.rm=TRUE),
                       ChgSup=NA, SupDem=NA)

for(i in 1:(n.yrs-1)) {
  pred.data[i, "ChgPoly"] = pred.data[i+1, "Poly_m1"] - pred.data[i, "Poly_m1"]
  pred.data[i, "ChgSup"] = sum(pred.data[i, 1:n.coef] * coef(mod))
  pred.data[i, "SupDem"] = pred.data[i, "SupDem_m1"] + pred.data[i, "ChgSup"]
  pred.data[i+1, "SupDem_m1"] = pred.data[i, "SupDem"]
  pred.data[i+1, "SupDem_m2"] = pred.data[i, "SupDem_m1"]
}

n.sims = 1e4
mod.sims = mvrnorm(n=n.sims, mu=coef(mod), Sigma=plm::vcovHC(mod))
yhat = matrix(NA, ncol=n.yrs, nrow=n.sims)
sup.hat = matrix(NA, ncol=n.yrs, nrow=n.sims)
for(i in 1:(n.yrs-1)) {
  yhat[, i] = mod.sims %*% t(pred.data[i, 1:n.coef])
  sup.hat[, i] = pred.data[i, "SupDem_m1"] + yhat[, i] + rnorm(n.sims, 0, sigma(mod))
}

pe = apply(sup.hat, 2, mean, na.rm=TRUE)
u95 = apply(sup.hat, 2, quantile, 0.975, na.rm=TRUE)
l95 = apply(sup.hat, 2, quantile, 0.025, na.rm=TRUE)
yr.st = n.burn - 9

pdf("fig6B.pdf", height=4, width=4)
par(mfrow=c(1,1), mar=c(2,3,1.8,.5), tcl=-0.25, cex=0.9)
plot(x=yr.st:(n.yrs-1), y=pe[yr.st:(n.yrs-1)], type="n", ylim=c(-1, 0.5), 
     xaxs="i", xaxt="n", yaxt="n", xlab="", ylab="")
abline(v=seq(110, 130, by=10), lty=3, col=rgb(0,0,0,.4))
abline(h=c(-1, -0.5, 0, 0.5), lty=3, col=rgb(0,0,0,.4))
abline(v=100, lty=2, col=rgb(0,0,0,.75))
axis(side=1, at=seq(100, 130, by=10), labels=seq(0, 30, by=10), cex.axis=0.8, las=1, mgp=c(0.7, 0.2, 0))
axis(side=2, at=c(-1, -0.5, 0, 0.5), cex.axis=0.8, las=1, mgp=c(1, 0.4, 0))
lines(x=yr.st:(n.yrs-1), y=pe[yr.st:(n.yrs-1)], col=rgb(0,.4,.4,1), lwd=2)
polygon(x=c(yr.st:(n.yrs-1), (n.yrs-1):yr.st), col=rgb(0,.4,.4,.5), border=NA, 
        y=c(u95[yr.st:(n.yrs-1)], l95[(n.yrs-1):yr.st]))
mtext(side=1, line=1.1, "Years", cex=0.9)
mtext(side=2, line=1.8, "Democratic mood", cex=0.9)
mtext(side=3, line=0.3, "Model 2.2", cex=0.9, adj=0)
text(x=99.5, y=0.4, "Electoral democracy increases 1 SD\nin 1 year", cex=0.85, pos=4)
dev.off()


## Minoritarian democracy up 1 SD

# Model 2.1: corruption excluded

mod = plm(diff(SupDem_trim, lag=1) ~ plm::lag(SupDem_trim, 1:2) + diff(Polyarchy_z, lag=1) 
          + plm::lag(Polyarchy_z, 1) + diff(Liberal_z, lag=1) + plm::lag(Liberal_z, 1) 
          + diff(lnGDP_imp, lag=1) + plm::lag(lnGDP_imp, 1), sd.plm, model="pooling")
coef(mod)
n.coef =  length(coef(mod))
n.yrs = 130
n.burn = 100
pred.data = data.frame(Intercept=1, SupDem_m1=0, SupDem_m2=0, 
                       ChgPoly=0, 
                       Poly_m1=0, 
                       ChgLib=0, 
                       Liberal_m1=c(rep(-0.5,n.burn), rep(0.5,n.yrs-n.burn)),                        
                       ChglogGDP=0,
                       lnGDP_m1=mean(supdem$lnGDP_imp, na.rm=TRUE),
                       ChgCorr=0,
                       Corrup_m1=mean(supdem$Corrup_TI_z, na.rm=TRUE),
                       ChgSup=NA, SupDem=NA)

for(i in 1:(n.yrs-1)) {
  pred.data[i, "ChgLib"] = pred.data[i+1, "Liberal_m1"] - pred.data[i, "Liberal_m1"]
  pred.data[i, "ChgSup"] = sum(pred.data[i, 1:n.coef] * coef(mod))
  pred.data[i, "SupDem"] = pred.data[i, "SupDem_m1"] + pred.data[i, "ChgSup"]
  pred.data[i+1, "SupDem_m1"] = pred.data[i, "SupDem"]
  pred.data[i+1, "SupDem_m2"] = pred.data[i, "SupDem_m1"]
}

n.sims = 1e4
mod.sims = mvrnorm(n=n.sims, mu=coef(mod), Sigma=plm::vcovHC(mod))
yhat = matrix(NA, ncol=n.yrs, nrow=n.sims)
sup.hat = matrix(NA, ncol=n.yrs, nrow=n.sims)
for(i in 1:(n.yrs-1)) {
  yhat[, i] = mod.sims %*% t(pred.data[i, 1:n.coef])
  sup.hat[, i] = pred.data[i, "SupDem_m1"] + yhat[, i] + rnorm(n.sims, 0, sigma(mod))
}

pe = apply(sup.hat, 2, mean, na.rm=TRUE)
u95 = apply(sup.hat, 2, quantile, 0.975, na.rm=TRUE)
l95 = apply(sup.hat, 2, quantile, 0.025, na.rm=TRUE)
yr.st = n.burn - 9

pdf("fig6C.pdf", height=4, width=4)
par(mfrow=c(1,1), mar=c(2,3,1.8,.5), tcl=-0.25, cex=0.9)
plot(x=yr.st:(n.yrs-1), y=pe[yr.st:(n.yrs-1)], type="n", ylim=c(-1.5, 0), 
     xaxs="i", xaxt="n", yaxt="n", xlab="", ylab="")
abline(v=seq(110, 130, by=10), lty=3, col=rgb(0,0,0,.4))
abline(h=c(-1.5, -1, -0.5, 0), lty=3, col=rgb(0,0,0,.4))
abline(v=100, lty=2, col=rgb(0,0,0,.75))
axis(side=1, at=seq(100, 130, by=10), labels=seq(0, 30, by=10), cex.axis=0.8, las=1, mgp=c(0.7, 0.2, 0))
axis(side=2, at=c(-1.5, -1, -0.5, 0), cex.axis=0.8, las=1, mgp=c(1, 0.4, 0))
lines(x=yr.st:(n.yrs-1), y=pe[yr.st:(n.yrs-1)], col=rgb(0,.4,.4,1), lwd=2)
polygon(x=c(yr.st:(n.yrs-1), (n.yrs-1):yr.st), col=rgb(0,.4,.4,.5), border=NA, 
        y=c(u95[yr.st:(n.yrs-1)], l95[(n.yrs-1):yr.st]))
mtext(side=1, line=1.1, "Years", cex=0.9)
mtext(side=2, line=1.8, "Democratic mood", cex=0.9)
mtext(side=3, line=0.3, "Model 2.1", cex=0.9, adj=0)
text(x=99.5, y=-0.1, "Minoritarian democracy increases 1 SD\nin 1 year", cex=0.85, pos=4)
dev.off()

# Model 2.2: corruption included

mod = plm(diff(SupDem_trim, lag=1) ~ plm::lag(SupDem_trim, 1:2) + diff(Polyarchy_z, lag=1) 
          + plm::lag(Polyarchy_z, 1) + diff(Liberal_z, lag=1) + plm::lag(Liberal_z, 1) 
          + diff(lnGDP_imp, lag=1) + plm::lag(lnGDP_imp, 1) + diff(Corrup_TI_z, lag=1) 
          + plm::lag(Corrup_TI_z, 1), sd.plm, model="pooling")
coef(mod)
n.coef =  length(coef(mod))
n.yrs = 130
n.burn = 100
pred.data = data.frame(Intercept=1, SupDem_m1=0, SupDem_m2=0, 
                       ChgPoly=0, 
                       Poly_m1=0, 
                       ChgLib=0, 
                       Liberal_m1=c(rep(-0.5,n.burn), rep(0.5,n.yrs-n.burn)),                        
                       ChglogGDP=0,
                       lnGDP_m1=mean(supdem$lnGDP_imp, na.rm=TRUE),
                       ChgCorr=0,
                       Corrup_m1=mean(supdem$Corrup_TI_z, na.rm=TRUE),
                       ChgSup=NA, SupDem=NA)

for(i in 1:(n.yrs-1)) {
  pred.data[i, "ChgLib"] = pred.data[i+1, "Liberal_m1"] - pred.data[i, "Liberal_m1"]
  pred.data[i, "ChgSup"] = sum(pred.data[i, 1:n.coef] * coef(mod))
  pred.data[i, "SupDem"] = pred.data[i, "SupDem_m1"] + pred.data[i, "ChgSup"]
  pred.data[i+1, "SupDem_m1"] = pred.data[i, "SupDem"]
  pred.data[i+1, "SupDem_m2"] = pred.data[i, "SupDem_m1"]
}

n.sims = 1e4
mod.sims = mvrnorm(n=n.sims, mu=coef(mod), Sigma=plm::vcovHC(mod))
yhat = matrix(NA, ncol=n.yrs, nrow=n.sims)
sup.hat = matrix(NA, ncol=n.yrs, nrow=n.sims)
for(i in 1:(n.yrs-1)) {
  yhat[, i] = mod.sims %*% t(pred.data[i, 1:n.coef])
  sup.hat[, i] = pred.data[i, "SupDem_m1"] + yhat[, i] + rnorm(n.sims, 0, sigma(mod))
}

pe = apply(sup.hat, 2, mean, na.rm=TRUE)
u95 = apply(sup.hat, 2, quantile, 0.975, na.rm=TRUE)
l95 = apply(sup.hat, 2, quantile, 0.025, na.rm=TRUE)
yr.st = n.burn - 9

pdf("fig6D.pdf", height=4, width=4)
par(mfrow=c(1,1), mar=c(2,3,1.8,.5), tcl=-0.25, cex=0.9)
plot(x=yr.st:(n.yrs-1), y=pe[yr.st:(n.yrs-1)], type="n", ylim=c(-1, 0.5), 
     xaxs="i", xaxt="n", yaxt="n", xlab="", ylab="")
abline(v=seq(110, 130, by=10), lty=3, col=rgb(0,0,0,.4))
abline(h=c(-1, -0.5, 0, 0.5), lty=3, col=rgb(0,0,0,.4))
abline(v=100, lty=2, col=rgb(0,0,0,.75))
axis(side=1, at=seq(100, 130, by=10), labels=seq(0, 30, by=10), cex.axis=0.8, las=1, mgp=c(0.7, 0.2, 0))
axis(side=2, at=c(-1, -0.5, 0, 0.5), cex.axis=0.8, las=1, mgp=c(1, 0.4, 0))
lines(x=yr.st:(n.yrs-1), y=pe[yr.st:(n.yrs-1)], col=rgb(0,.4,.4,1), lwd=2)
polygon(x=c(yr.st:(n.yrs-1), (n.yrs-1):yr.st), col=rgb(0,.4,.4,.5), border=NA, 
        y=c(u95[yr.st:(n.yrs-1)], l95[(n.yrs-1):yr.st]))
mtext(side=1, line=1.1, "Years", cex=0.9)
mtext(side=2, line=1.8, "Democratic mood", cex=0.9)
mtext(side=3, line=0.3, "Model 2.2", cex=0.9, adj=0)
text(x=99.5, y=0.4, "Minoritarian democracy increases 1 SD\nin 1 year", cex=0.85, pos=4)
dev.off()




### Other analyses for supplementary materials

# Including other covariates: Education and Gini index

mod.1.edu = plm(diff(SupDem_trim, lag=1) ~ plm::lag(SupDem_trim, 1:2) + diff(Libdem_z, lag=1) + plm::lag(Libdem_z, 1) 
              + diff(lnGDP_imp, lag=1) + plm::lag(lnGDP_imp, 1) + diff(Corrup_TI_z, lag=1) + plm::lag(Corrup_TI_z, 1)
              + diff(Educ_yrs_UN, lag=1) + lag(Educ_yrs_UN, 1), sd.plm, model="pooling")
mod.1.gini = plm(diff(SupDem_trim, lag=1) ~ plm::lag(SupDem_trim, 1:2) + diff(Libdem_z, lag=1) + plm::lag(Libdem_z, 1) 
              + diff(lnGDP_imp, lag=1) + plm::lag(lnGDP_imp, 1) + diff(Corrup_TI_z, lag=1) + plm::lag(Corrup_TI_z, 1)
              + diff(Gini_SWIID, lag=1) + lag(Gini_SWIID, 1), sd.plm, model="pooling")
mod.2.edu = plm(diff(SupDem_trim, lag=1) ~ plm::lag(SupDem_trim, 1:2) + diff(Liberal_z, lag=1) + plm::lag(Liberal_z, 1)
                + diff(Polyarchy_z, lag=1) + plm::lag(Polyarchy_z, 1) + diff(lnGDP_imp, lag=1) + plm::lag(lnGDP_imp, 1) 
                + diff(Corrup_TI_z, lag=1) + plm::lag(Corrup_TI_z, 1) + diff(Educ_yrs_UN, lag=1) + lag(Educ_yrs_UN, 1), 
                sd.plm, model="pooling")
mod.2.gini = plm(diff(SupDem_trim, lag=1) ~ plm::lag(SupDem_trim, 1:2) + diff(Liberal_z, lag=1) + plm::lag(Liberal_z, 1)
                + diff(Polyarchy_z, lag=1) + plm::lag(Polyarchy_z, 1) + diff(lnGDP_imp, lag=1) + plm::lag(lnGDP_imp, 1) 
                + diff(Corrup_TI_z, lag=1) + plm::lag(Corrup_TI_z, 1) + diff(Gini_SWIID, lag=1) + lag(Gini_SWIID, 1), 
                sd.plm, model="pooling")

fd.1.edu = plm(SupDem_trim ~ Libdem_z + lnGDP_imp + Corrup_TI_z + Educ_yrs_UN, model="fd", data=sd.plm, index="Country")
fd.1.gini = plm(SupDem_trim ~ Libdem_z + lnGDP_imp + Corrup_TI_z + Gini_SWIID, model="fd", data=sd.plm, index="Country")
fd.2.edu = plm(SupDem_trim ~ Liberal_z + Polyarchy_z + lnGDP_imp + Corrup_TI_z + Educ_yrs_UN, model="fd", data=sd.plm, 
               index="Country")
fd.2.gini = plm(SupDem_trim ~ Liberal_z + Polyarchy_z + lnGDP_imp + Corrup_TI_z + Gini_SWIID, model="fd", data=sd.plm, 
                index="Country")

# Table S3
mods = list(mod.1.edu, mod.2.edu, fd.1.edu, fd.2.edu)
vcm = lapply(mods, plm::vcovHC)
rse = lapply(vcm, function(x) sqrt(diag(x)))
pval = list()
for(i in 1:length(mods)) { 
  pval[[i]] = pt(abs(coef(mods[[i]]) / rse[[i]]), df=2000, lower.tail=FALSE) * 2
}
screenreg(mods, override.se=rse, override.pvalues=pval, digits=3, single.row=FALSE, 
       stars=c(0.05), leading.zero=FALSE)
varnams = c("Intercept", "Democratic mood_t-1", "Democratic mood_t-2", "Delta Liberal democracy", "Liberal democracy_t-1",
            "Delta log GDP per capita", "log GDP per capita_t-1", "Delta Corruption", "Corruption_t-1", "Delta Education", 
            "Education_t-1", "Delta Minoritarian democracy", "Minoritarian democracy_t-1", 
            "Delta Electoral democracy", "Electoral democracy_t-1", "Delta Liberal democracy", "Delta log GDP per capita", 
            "Delta Corruption", "Delta Education", "Delta Minoritarian democracy", "Delta Electoral democracy")
texreg(mods, file="tableS3.txt", override.se=rse, override.pvalues=pval, digits=3, single.row=FALSE, 
       stars=c(0.05), leading.zero=FALSE, dcolumn=TRUE, booktabs=TRUE, custom.coef.names=varnams)

# Table S4
mods = list(mod.1.gini, mod.2.gini, fd.1.gini, fd.2.gini)
vcm = lapply(mods, plm::vcovHC)
rse = lapply(vcm, function(x) sqrt(diag(x)))
pval = list()
for(i in 1:length(mods)) { 
  pval[[i]] = pt(abs(coef(mods[[i]]) / rse[[i]]), df=2000, lower.tail=FALSE) * 2
}
screenreg(mods, override.se=rse, override.pvalues=pval, digits=3, single.row=FALSE, 
       stars=c(0.05), leading.zero=FALSE)
varnams = c("Intercept", "Democratic mood_t-1", "Democratic mood_t-2", "Delta Liberal democracy", "Liberal democracy_t-1",
            "Delta log GDP per capita", "log GDP per capita_t-1", "Delta Corruption", "Corruption_t-1", "Delta Gini", 
            "Gini_t-1", "Delta Minoritarian democracy", "Minoritarian democracy_t-1", 
            "Delta Electoral democracy", "Electoral democracy_t-1", "Delta Liberal democracy", "Delta log GDP per capita", 
            "Delta Corruption", "Delta Gini", "Delta Minoritarian democracy", "Delta Electoral democracy")
texreg(mods, file="tableS4.txt", override.se=rse, override.pvalues=pval, digits=3, single.row=FALSE, 
       stars=c(0.05), leading.zero=FALSE, dcolumn=TRUE, booktabs=TRUE, custom.coef.names=varnams)

# One-way FE models

fe1 = plm(SupDem_trim ~ Libdem_z + lnGDP_imp, model="within", data=sd.plm, index="Country")
fe2 = plm(SupDem_trim ~ Libdem_z + lnGDP_imp + Corrup_TI_z, model="within", data=sd.plm, index="Country")
fe3 = plm(SupDem_trim ~ Polyarchy_z + Liberal_z + lnGDP_imp, model="within", data=sd.plm, index="Country")
fe4 = plm(SupDem_trim ~ Polyarchy_z + Liberal_z + lnGDP_imp + Corrup_TI_z, model="within", data=sd.plm, index="Country")

mods = list(fe1, fe2, fe3, fe4)
vcm = lapply(mods, function(x) plm::vcovHC(x, method="arellano", cluster="group"))
#vcm = lapply(mods, function(x) plm::vcovBK(x, cluster="time")) # cross-sectional heteroskedasticity and correlation
rse = lapply(vcm, function(x) sqrt(diag(x)))
pval = list()
for(i in 1:length(mods)) {pval[[i]] = pt(abs(coef(mods[[i]]) / rse[[i]]), df=2500, lower.tail=FALSE) * 2}
screenreg(mods, override.se=rse, override.pvalues=pval, digits=3, single.row=FALSE, leading.zero=FALSE, stars=c(0.05))
sapply(mods, function(x) round(sigma(x), 3))
sapply(mods, function(x) round(pbgtest(x, order=1)$p.value, 3)) # Wooldridge test for serial correlation
varnams = c("Liberal democracy", "log GDP per capita-1", "Corruption-1", "Electoral democracy-1", "Minoritarian democracy")
texreg(mods, file="tableS5.txt", override.se=rse, override.pvalues=pval, digits=3, single.row=FALSE, 
       stars=c(0.05), leading.zero=FALSE, dcolumn=TRUE, booktabs=TRUE, custom.coef.names=varnams)

# Two-way FE models

fe1 = plm(SupDem_trim ~ Libdem_z + lnGDP_imp, model="within", effect="twoways", data=sd.plm, index="Country")
fe2 = plm(SupDem_trim ~ Libdem_z + lnGDP_imp + Corrup_TI_z, model="within", effect="twoways", data=sd.plm, index="Country")
fe3 = plm(SupDem_trim ~ Polyarchy_z + Liberal_z + lnGDP_imp, model="within", effect="twoways", data=sd.plm, index="Country")
fe4 = plm(SupDem_trim ~ Polyarchy_z + Liberal_z + lnGDP_imp + Corrup_TI_z, model="within", effect="twoways", 
          data=sd.plm, index="Country")

mods = list(fe1, fe2, fe3, fe4)
vcm = lapply(mods, function(x) plm::vcovHC(x, method="arellano", cluster="group"))
#vcm = lapply(mods, function(x) plm::vcovBK(x, cluster="time")) # cross-sectional heteroskedasticity and correlation
rse = lapply(vcm, function(x) sqrt(diag(x)))
pval = list()
for(i in 1:length(mods)) {pval[[i]] = pt(abs(coef(mods[[i]]) / rse[[i]]), df=2500, lower.tail=FALSE) * 2}
screenreg(mods, override.se=rse, override.pvalues=pval, digits=3, single.row=FALSE, leading.zero=FALSE, stars=c(0.05))
sapply(mods, function(x) round(sigma(x), 3))
sapply(mods, function(x) round(pbgtest(x, order=1)$p.value, 3)) # Wooldridge test for serial correlation
varnams = c("Liberal democracy", "log GDP per capita-1", "Corruption-1", "Electoral democracy-1", "Minoritarian democracy")
texreg(mods, file="tableS6.txt", override.se=rse, override.pvalues=pval, digits=3, single.row=FALSE, 
       stars=c(0.05), leading.zero=FALSE, dcolumn=TRUE, booktabs=TRUE, custom.coef.names=varnams)


# Mundlak models

mm1 = lmer(SupDem_trim ~ Libdem_within + Libdem_btw + GDP_cp_btw + GDP_cp_within
           + (1 | ISO_code), data=supdem) 
mm2 = lmer(SupDem_trim ~ Libdem_within + Libdem_btw + GDP_cp_btw + GDP_cp_within + Corrup_within + Corrup_btw
           + (1 | ISO_code), data=supdem) 
mm3 = lmer(SupDem_trim ~ Liberal_within + Poly_within + Liberal_btw + Poly_btw + GDP_cp_btw + GDP_cp_within
           + (1 | ISO_code), data=supdem) 
mm4 = lmer(SupDem_trim ~ Liberal_within + Poly_within + Liberal_btw + Poly_btw + GDP_cp_btw + GDP_cp_within 
           + Corrup_within + Corrup_btw + (1 | ISO_code), data=supdem) 

mods = list(mm1, mm2, mm3, mm4)
screenreg(mods, digits=3, single.row=FALSE, leading.zero=FALSE, stars=c(0.05))
texreg(mods, file="tableS8.txt", digits=3, single.row=FALSE, stars=c(0.05), leading.zero=FALSE, 
       dcolumn=TRUE, booktabs=TRUE)


# ECMs with one lag

mod1l.1 = plm(diff(SupDem_trim, lag=1) ~ plm::lag(SupDem_trim, 1) + diff(Libdem_z, lag=1) + plm::lag(Libdem_z, 1)
           + diff(lnGDP_imp, lag=1) + plm::lag(lnGDP_imp, 1), sd.plm, model="pooling")
mod1l.2 = plm(diff(SupDem_trim, lag=1) ~ plm::lag(SupDem_trim, 1) + diff(Libdem_z, lag=1) + plm::lag(Libdem_z, 1) 
              + diff(lnGDP_imp, lag=1) + plm::lag(lnGDP_imp, 1) + diff(Corrup_TI_z, lag=1) + plm::lag(Corrup_TI_z, 1), 
              sd.plm, model="pooling")
mod1l.3 = plm(diff(SupDem_trim, lag=1) ~ plm::lag(SupDem_trim, 1) + diff(Polyarchy_z, lag=1) + plm::lag(Polyarchy_z, 1)
              + diff(Liberal_z, lag=1) + plm::lag(Liberal_z, 1) + diff(lnGDP_imp, lag=1) + plm::lag(lnGDP_imp, 1), 
              sd.plm, model="pooling")
mod1l.4 = plm(diff(SupDem_trim, lag=1) ~ plm::lag(SupDem_trim, 1) + diff(Polyarchy_z, lag=1) + plm::lag(Polyarchy_z, 1)
              + diff(Liberal_z, lag=1) + plm::lag(Liberal_z, 1) + diff(lnGDP_imp, lag=1) + plm::lag(lnGDP_imp, 1) 
              + diff(Corrup_TI_z, lag=1) + plm::lag(Corrup_TI_z, 1), sd.plm, model="pooling")

mods = list(mod1l.1, mod1l.2, mod1l.3, mod1l.4)
vcm = lapply(mods, function(x) plm::vcovHC(x, method="arellano", cluster="group"))
rse = lapply(vcm, function(x) sqrt(diag(x)))
pval = list()
for(i in 1:length(mods)) {pval[[i]] = pt(abs(coef(mods[[i]]) / rse[[i]]), df=2500, lower.tail=FALSE) * 2}
screenreg(mods, override.se=rse, override.pvalues=pval, digits=3, single.row=FALSE, leading.zero=FALSE, stars=c(0.05))

varnams = c("Intercept", "Democratic mood_t-1", "Delta Liberal democracy", "Liberal democracy_t-1", 
            "Delta log GDP per capita", "log GDP per capita_t-1",  "Delta Corruption", "Corruption_t-1",  
            "Delta electoral democracy", "Electoral democracy_t-1", "Delta minoritarian democracy", 
            "Minoritarian democracy_t-1")
texreg(mods, file="tableS7.txt", override.se=rse, override.pvalues=pval, digits=3, single.row=FALSE, 
       stars=c(0.05), leading.zero=FALSE, dcolumn=TRUE, booktabs=TRUE, custom.coef.names=varnams)


### Supporting tables

## descriptives

supdem.trim = supdem[complete.cases(supdem[, c("Libdem_m1", "SupDem_trim")]),]
vars = c("SupDem_trim", "Libdem_m1", "Year", "lnGDP_m1", "Corrup_TI_m1", "Gini_m1", "Educ_yrs_m1")
sum.table = data.frame(
  Mean=sapply(supdem.trim[,vars], mean, na.rm=TRUE),
  SD=sapply(supdem.trim[,vars], sd, na.rm=TRUE),
  Min=sapply(supdem.trim[,vars], min, na.rm=TRUE),
  Max=sapply(supdem.trim[,vars], max, na.rm=TRUE),
  NAs=sapply(supdem.trim[,vars], function(x) sum(is.na(x))))
print(sum.table)
print(xtable(sum.table, digits=2), file="tableS1.txt")


## Tests of autocorrelation (in text)

mod1 = plm(diff(SupDem_trim, lag=1) ~ plm::lag(SupDem_trim, 1:2) + diff(Libdem_z, lag=1) + plm::lag(Libdem_z, 1)
           + diff(lnGDP_imp, lag=1) + plm::lag(lnGDP_imp, 1), sd.plm, model="pooling")
pbgtest(mod1, order=1)
pdwtest(mod1, order=1)


## Tests of number of lags (in text)

supdem.lagtest = supdem[complete.cases(supdem[, c("SupDem_trim", "SupDem_m2", "Libdem_m1")]), ]
adl1.0 = lm(SupDem_trim ~ Libdem_z + Libdem_m1 + lnGDP_m1, supdem.lagtest)
adl1.1 = lm(SupDem_trim ~ SupDem_m1 + Libdem_z + Libdem_m1 + lnGDP_m1, supdem.lagtest)
adl1.2 = lm(SupDem_trim ~ SupDem_m1 + SupDem_m2 + Libdem_z + Libdem_m1 + lnGDP_m1, supdem.lagtest)
mods = list(adl1.0, adl1.1, adl1.2)
screenreg(mods, single.row=TRUE, digits=3)
lapply(mods, extractAIC)


## Tests of stationarity for panel data

# set up table
purtest.table = matrix(NA, ncol=4, nrow=10)
colnames(purtest.table) = c("IPS_stat", "IPS_pval", "LLC_stat", "LLC_pval")
row.names(purtest.table) = c("SupDem", "SupDem", "ChgSup", "ChgSup", "Libdem_z", "ChgDem", 
                             "lnGDP_imp", "Liberal_z", "Polyarchy_z", "Corrup_TI_z")

# SupDem_trim (20 years, 69 countries)
supdem.test.1 = supdem[, c("Country", "Year", "SupDem_trim")]
supdem.root.test = data.frame(split(supdem["SupDem_trim"], supdem["Country"], drop=TRUE))
supdem.root.test = supdem.root.test[12:31, ]
supdem.root.test = supdem.root.test[, !is.na(supdem.root.test[1,])]
dim(supdem.root.test)
purtest.table[1,1] = purtest(supdem.root.test, test="ips", exo="trend", lags="AIC", pmax=3)[[1]][1][[1]]
purtest.table[1,2] = purtest(supdem.root.test, test="ips", exo="trend", lags="AIC", pmax=3)[[1]][6][[1]]
purtest.table[1,3] = purtest(supdem.root.test, test="levinlin", exo="trend", lags="AIC", pmax=3)[[1]][1][[1]] 
purtest.table[1,4] = purtest(supdem.root.test, test="levinlin", exo="trend", lags="AIC", pmax=3)[[1]][6][[1]] 

# SupDem_trim (10 years, 121 countries)
supdem.test.1 = supdem[, c("Country", "Year", "SupDem_trim")]
supdem.root.test = data.frame(split(supdem.test.1$SupDem_trim, supdem.test.1$Country, drop=TRUE))
supdem.root.test = supdem.root.test[22:31, ]
supdem.root.test = supdem.root.test[, !is.na(supdem.root.test[1,])]
dim(supdem.root.test)
purtest.table[2,1] = purtest(supdem.root.test, test="ips", exo="trend", lags=2)[[1]][1][[1]] 
purtest.table[2,2] = purtest(supdem.root.test, test="ips", exo="trend", lags=2)[[1]][6][[1]]
purtest.table[2,3] = purtest(supdem.root.test, test="levinlin", exo="trend", lags=2)[[1]][1][[1]] 
purtest.table[2,4] = purtest(supdem.root.test, test="levinlin", exo="trend", lags=2)[[1]][6][[1]]

# ChgSup (20 years, 65 countries)
supdem.test.1 = supdem[, c("Country", "Year", "ChgSup")]
supdem.root.test = data.frame(split(supdem.test.1$ChgSup, supdem.test.1$Country, drop=TRUE))
supdem.root.test = supdem.root.test[12:31, ]
supdem.root.test = supdem.root.test[, !is.na(supdem.root.test[1,])]
purtest.table[3,1] = purtest(supdem.root.test, test="ips", exo="trend", lags=2)[[1]][1][[1]] 
purtest.table[3,2] = purtest(supdem.root.test, test="ips", exo="trend", lags=2)[[1]][6][[1]]
purtest.table[3,3] = purtest(supdem.root.test, test="levinlin", exo="trend", lags=2)[[1]][1][[1]] 
purtest.table[3,4] = purtest(supdem.root.test, test="levinlin", exo="trend", lags=2)[[1]][6][[1]]

# ChgSup (10 years, 120 countries)
supdem.test.1 = supdem[, c("Country", "Year", "ChgSup")]
supdem.root.test = data.frame(split(supdem.test.1$ChgSup, supdem.test.1$Country, drop=TRUE))
supdem.root.test = supdem.root.test[22:31, ]
supdem.root.test = supdem.root.test[, !is.na(supdem.root.test[1,])]
purtest.table[4,1] = purtest(supdem.root.test, test="ips", exo="trend", lags=2)[[1]][1][[1]] 
purtest.table[4,2] = purtest(supdem.root.test, test="ips", exo="trend", lags=2)[[1]][6][[1]]
purtest.table[4,3] = purtest(supdem.root.test, test="levinlin", exo="trend", lags=2)[[1]][1][[1]] 
purtest.table[4,4] = purtest(supdem.root.test, test="levinlin", exo="trend", lags=2)[[1]][6][[1]]

# Libdem_VD (30 years, 134 countries)
supdem.test.1 = supdem[, c("Country", "Year", "Libdem_z")]
supdem.root.test = data.frame(split(supdem.test.1$Libdem_z, supdem.test.1$Country, drop=TRUE))
supdem.root.test = supdem.root.test[2:31, ]
supdem.root.test = supdem.root.test[, !is.na(supdem.root.test[1,])]
dim(supdem.root.test)
purtest.table[5,1] = purtest(supdem.root.test, test="ips", exo="trend", lags="AIC", pmax=4)[[1]][1][[1]] 
purtest.table[5,2] = purtest(supdem.root.test, test="ips", exo="trend", lags="AIC", pmax=4)[[1]][6][[1]]
purtest.table[5,3] = purtest(supdem.root.test, test="levinlin", exo="trend", lags="AIC", pmax=4)[[1]][1][[1]] 
purtest.table[5,4] = purtest(supdem.root.test, test="levinlin", exo="trend", lags="AIC", pmax=4)[[1]][6][[1]] 

# ChgDem (30 years, 134 countries)
supdem.test.1 = supdem[, c("Country", "Year", "ChgDem")]
supdem.root.test = data.frame(split(supdem.test.1$ChgDem, supdem.test.1$Country, drop=TRUE))
supdem.root.test = supdem.root.test[2:31, ]
supdem.root.test = supdem.root.test[, !is.na(supdem.root.test[1,])]
purtest.table[6,1] = purtest(supdem.root.test, test="ips", exo="trend", lags="AIC", pmax=4)[[1]][1][[1]] 
purtest.table[6,2] = purtest(supdem.root.test, test="ips", exo="trend", lags="AIC", pmax=4)[[1]][6][[1]]
purtest.table[6,3] = purtest(supdem.root.test, test="levinlin", exo="trend", lags="AIC", pmax=4)[[1]][1][[1]] 
purtest.table[6,4] = purtest(supdem.root.test, test="levinlin", exo="trend", lags="AIC", pmax=4)[[1]][6][[1]]

# log GDP
supdem.test.1 = supdem[, c("Country", "Year", "lnGDP_imp")]
supdem.root.test = data.frame(split(supdem.test.1$lnGDP_imp, supdem.test.1$Country, drop=TRUE))
supdem.root.test = supdem.root.test[2:31, ]
supdem.root.test = supdem.root.test[, !is.na(supdem.root.test[1,])]
purtest.table[7,1] = purtest(supdem.root.test, test="ips", exo="trend", lags="AIC", pmax=4)[[1]][1][[1]] 
purtest.table[7,2] = purtest(supdem.root.test, test="ips", exo="trend", lags="AIC", pmax=4)[[1]][6][[1]]
purtest.table[7,3] = purtest(supdem.root.test, test="levinlin", exo="trend", lags="AIC", pmax=4)[[1]][1][[1]] 
purtest.table[7,4] = purtest(supdem.root.test, test="levinlin", exo="trend", lags="AIC", pmax=4)[[1]][6][[1]] 

# Liberal_z (30 years, 134 countries)
supdem.test.1 = supdem[, c("Country", "Year", "Liberal_z")]
supdem.root.test = data.frame(split(supdem.test.1$Liberal_z, supdem.test.1$Country, drop=TRUE))
supdem.root.test = supdem.root.test[2:31, ]
supdem.root.test = supdem.root.test[, !is.na(supdem.root.test[1,])]
purtest.table[8,1] = purtest(supdem.root.test, test="ips", exo="trend", lags="AIC", pmax=4)[[1]][1][[1]] 
purtest.table[8,2] = purtest(supdem.root.test, test="ips", exo="trend", lags="AIC", pmax=4)[[1]][6][[1]] 
purtest.table[8,3] = purtest(supdem.root.test, test="levinlin", exo="trend", lags="AIC", pmax=4)[[1]][1][[1]] 
purtest.table[8,4] = purtest(supdem.root.test, test="levinlin", exo="trend", lags="AIC", pmax=4)[[1]][6][[1]] 

# Polyarchy_z 
supdem.test.1 = supdem[, c("Country", "Year", "Polyarchy_z")]
supdem.root.test = data.frame(split(supdem.test.1$Polyarchy_z, supdem.test.1$Country, drop=TRUE))
supdem.root.test = supdem.root.test[2:31, ]
supdem.root.test = supdem.root.test[, !is.na(supdem.root.test[1,])]
purtest.table[9,1] = purtest(supdem.root.test, test="ips", exo="trend", lags="AIC", pmax=4)[[1]][1][[1]] 
purtest.table[9,2] = purtest(supdem.root.test, test="ips", exo="trend", lags="AIC", pmax=4)[[1]][6][[1]]
purtest.table[9,3] = purtest(supdem.root.test, test="levinlin", exo="trend", lags="AIC", pmax=4)[[1]][1][[1]] 
purtest.table[9,4] = purtest(supdem.root.test, test="levinlin", exo="trend", lags="AIC", pmax=4)[[1]][6][[1]] 

# Corrup_z (15 years, 99 countries)
supdem.test.1 = supdem[, c("Country", "Year", "Corrup_TI_z")]
supdem.root.test = data.frame(split(supdem.test.1$Corrup_TI_z, supdem.test.1$Country, drop=TRUE))
supdem.root.test = supdem.root.test[16:30, ]
supdem.root.test = supdem.root.test[, !is.na(supdem.root.test[1,])]
dim(supdem.root.test)
purtest.table[10,1] = purtest(supdem.root.test, test="ips", exo="trend", lags="AIC", pmax=4)[[1]][1][[1]] 
purtest.table[10,2] = purtest(supdem.root.test, test="ips", exo="trend", lags="AIC", pmax=4)[[1]][6][[1]]
purtest.table[10,3] = purtest(supdem.root.test, test="levinlin", exo="trend", lags="AIC", pmax=4)[[1]][1][[1]] 
purtest.table[10,4] = purtest(supdem.root.test, test="levinlin", exo="trend", lags="AIC", pmax=4)[[1]][6][[1]] 

round(purtest.table, 3)
print(xtable(purtest.table, digits=3), file="tableS2.txt")



## Plot and analyze support and democracy time-series by regime type

drop.cnts.plot = c("Malta", "Belize")
supdem.plot = supdem[!(supdem$Country %in% drop.cnts.plot), ]
supdem.plot = supdem.plot[supdem.plot$Year > 1987,]

# Create democracy categories: Democracy (ED), New Democracy (ND), Stable Autocracy (SA), Regime in transition (RT)
# If never democratic (V-Dem regime) between First_yr and 2015, then SA
# If always democratic between 1971 and 2015 then ED
# If always democratic between First_yr and 2015, but not between 1971 and First_yr, then ND
# Otherwise RT

dc.dat = supdem.plot[, c("Country", "Year", "First_yr", "Regime_di", "DemYears")] 
dc.dat = dc.dat[dc.dat$Year >= dc.dat$First_yr, ]
dc.dat$Country = factor(dc.dat$Country)
ed.inds = by(dc.dat$Regime_di, dc.dat$Country, mean, na.rm=TRUE) == 1 & 
          by(dc.dat$DemYears, dc.dat$Country, mean, na.rm=TRUE) > 0.8
nd.inds = by(dc.dat$Regime_di, dc.dat$Country, mean, na.rm=TRUE) == 1 & 
          by(dc.dat$DemYears, dc.dat$Country, mean, na.rm=TRUE) <= 0.8

ed.inds[["India"]] = TRUE # add India to established democracies
nd.inds[["India"]] = FALSE

cnts.ed = levels(dc.dat$Country)[ed.inds]
cnts.nd = levels(dc.dat$Country)[nd.inds]
cnts.sa = levels(dc.dat$Country)[by(dc.dat$Regime_di, dc.dat$Country, mean, na.rm=TRUE) == 0]
cnts.rt = levels(dc.dat$Country)[!(levels(dc.dat$Country) %in% cnts.ed) & !(levels(dc.dat$Country) %in% cnts.nd) & 
                                 !(levels(dc.dat$Country) %in% cnts.sa)]
cnts.dem = c(cnts.ed, cnts.nd)

yr.labs = c(1990, 2000, 2010)

# Examine within and between variation (in text)

aov.all = anova(lm(Libdem_z ~ Country, supdem))
aov.all[[2]][1]/sum(aov.all[[2]])
aov.rt = anova(lm(Libdem_z ~ Country, supdem[supdem$Country %in% cnts.rt, ]))
aov.rt[[2]][1]/sum(aov.rt[[2]])
aov.nd = anova(lm(Libdem_z ~ Country, supdem[supdem$Country %in% cnts.nd, ]))
aov.nd[[2]][1]/sum(aov.nd[[2]])
aov.ed = anova(lm(Libdem_z ~ Country, supdem[supdem$Country %in% cnts.ed, ]))
aov.ed[[2]][1]/sum(aov.ed[[2]])
aov.sa = anova(lm(Libdem_z ~ Country, supdem[supdem$Country %in% cnts.sa, ]))
aov.sa[[2]][1]/sum(aov.sa[[2]])


## Plot support and democracy for all countries in four groups

# new democracies

length(cnts.nd) # 42
nd.axis.x = vector("logical", length(cnts.nd))
nd.axis.x[41:42] = TRUE
nd.axis.y = vector("logical", length(cnts.nd))
nd.axis.y[c(1,6,11,16,21,26,31,36,41)] = TRUE
cnt.nam = cnts.nd
cnt.nam[cnt.nam=="Trinidad and Tobago"] = "Trinidad & Tob."
cnt.nam[cnt.nam=="Dominican Republic"] = "Dominican Rep."

pdf('fig2.pdf', width=6.5, height=9)
par(mfrow=c(9, 5), mar=c(1, 1, 0.25, 0.25), tcl=-0.2, las=1, cex=0.7)
for(i in 1:length(cnts.nd)) {
  sup = supdem.plot[supdem.plot$Country == cnts.nd[i], "SupDem_trim"]
  sup.u = supdem.plot[supdem.plot$Country == cnts.nd[i], "SupDem_u90"]
  sup.l = supdem.plot[supdem.plot$Country == cnts.nd[i], "SupDem_l90"]
  yr = as.numeric(supdem.plot[supdem.plot$Country == cnts.nd[i], "Year"])
  resp = supdem.plot[supdem.plot$Country == cnts.nd[i], "N_response"] > 0
  dem = supdem.plot[supdem.plot$Country == cnts.nd[i], "Libdem_z"]
  plot(y=sup, x=yr, type="n", xlab="", ylab="", main="", ylim=c(-3, 3.5), xaxt="n", yaxt="n", bty="n")
  axis(side=1, at=yr.labs, labels=nd.axis.x[i], las=1, mgp=c(1,-0.1,0), tcl=-0.1, cex.axis=0.75, lwd=0.75)
  axis(side=2, at=c(-2,0,2), labels=nd.axis.y[i], las=1, mgp=c(1,.2,0), tcl=-0.1, cex.axis=0.75, lwd=0.75)
  abline(v=yr.labs, col=rgb(.6,.6,.6,1), lwd=0.5, lty=3)
  abline(h=0, col=rgb(.6,.6,.6,1), lwd=0.5, lty=3)
  text(x=1986, y=-2.8, pos=4, cnt.nam[i], cex=0.85)
  lines(y=dem, x=yr, col=rgb(0,0,0,0.8), lwd=1.5, lty=1)
  lines(y=sup, x=yr, col=rgb(0,.4,.4,1), lwd=1.5, lty=1)
  polygon(y=c(sup.u, rev(sup.l)), x=c(yr, rev(yr)), col=rgb(0,.4,.4,0.3), border=NA)
  points(x=yr[resp], y=sup[resp], pch="I", cex=0.7, col=rgb(0,.4,.4,1))
  box(lty=1, lwd=0.75)
  if(i==1) {
    text(x=1987, y=dem[2]+0.4, pos=4, "Democracy", col=rgb(0,0,0,1), cex=0.75)
    text(x=2007, y=sup[23]-0.6, pos=4, "Mood", col=rgb(0,.4,.4,1), cex=0.75)
  }
}
dev.off()

# established democracies

length(cnts.ed) # 23
ed.axis.x = vector("logical", length(cnts.ed))
ed.axis.x[21:23] = TRUE
ed.axis.y = vector("logical", length(cnts.ed))
ed.axis.y[c(1,6,11,16,21)] = TRUE
cnt.nam = cnts.ed
cnt.nam[cnt.nam=="United States of America"] = "USA"

pdf('fig1.pdf', width=6.5, height=5)
par(mfrow=c(5, 5), mar=c(1, 1, 0.25, 0.25), tcl=-0.2, las=1, cex=0.7)
for(i in 1:length(cnts.ed)) {
  sup = supdem.plot[supdem.plot$Country == cnts.ed[i], "SupDem_trim"]
  sup.u = supdem.plot[supdem.plot$Country == cnts.ed[i], "SupDem_u90"]
  sup.l = supdem.plot[supdem.plot$Country == cnts.ed[i], "SupDem_l90"]
  yr = as.numeric(supdem.plot[supdem.plot$Country == cnts.ed[i], "Year"])
  dem = supdem.plot[supdem.plot$Country == cnts.ed[i], "Libdem_z"]
  resp = supdem.plot[supdem.plot$Country == cnts.ed[i], "N_response"] > 0
  plot(y=sup, x=yr, type="n", xlab="", ylab="", main="", ylim=c(-3, 3.5), xaxt="n", yaxt="n", bty="n")
  axis(side=1, at=yr.labs, labels=ed.axis.x[i], las=1, mgp=c(1,-0.1,0), tcl=-0.1, cex.axis=0.75, lwd=0.75)
  axis(side=2, at=c(-2,0,2), labels=ed.axis.y[i], las=1, mgp=c(1,.2,0), tcl=-0.1, cex.axis=0.75, lwd=0.75)
  abline(v=yr.labs, col=rgb(.6,.6,.6,1), lwd=0.5, lty=3)
  abline(h=0, col=rgb(.6,.6,.6,1), lwd=0.5, lty=3)
  text(x=1986, y=-2.8, pos=4, cnt.nam[i], cex=0.85)
  lines(y=dem, x=yr, col=rgb(0,0,0,0.8), lwd=1.5, lty=1)
  lines(y=sup, x=yr, col=rgb(0,.4,.4,1), lwd=1.5, lty=1)
  points(x=yr[resp], y=sup[resp], pch="I", cex=0.7, col=rgb(0,.4,.4,1))
  polygon(y=c(sup.u, rev(sup.l)), x=c(yr, rev(yr)), col=rgb(0,.4,.4,0.3), border=NA)
  box(lty=1, lwd=0.75)
  if(i==1) {
    text(x=1987, y=dem[2]+0.35, pos=4, "Democracy", col=rgb(0,0,0,1), cex=0.75)
    text(x=2007, y=sup[23]-0.55, pos=4, "Mood", col=rgb(0,.4,.4,1), cex=0.75)
  }
}
dev.off()

# stable autocracies

length(cnts.sa) # 30
sa.axis.x = vector("logical", length(cnts.sa))
sa.axis.x[26:30] = TRUE
sa.axis.y = vector("logical", length(cnts.sa))
sa.axis.y[c(1,6,11,16,21,26)] = TRUE
cnt.nam = cnts.sa
cnt.nam[cnt.nam=="Bosnia and Herzegovina"] = "Bosnia & Herz."

pdf('fig3.pdf', width=6.5, height=6)
par(mfrow=c(6, 5), mar=c(1, 1, 0.25, 0.25), tcl=-0.2, las=1, cex=0.7)
for(i in 1:length(cnts.sa)) {
  sup = supdem.plot[supdem.plot$Country == cnts.sa[i], "SupDem_trim"]
  sup.u = supdem.plot[supdem.plot$Country == cnts.sa[i], "SupDem_u90"]
  sup.l = supdem.plot[supdem.plot$Country == cnts.sa[i], "SupDem_l90"]
  yr = as.numeric(supdem.plot[supdem.plot$Country == cnts.sa[i], "Year"])
  dem = supdem.plot[supdem.plot$Country == cnts.sa[i], "Libdem_z"]
  resp = supdem.plot[supdem.plot$Country == cnts.sa[i], "N_response"] > 0
  plot(y=sup, x=yr, type="n", xlab="", ylab="", main="", ylim=c(-3, 3.5), xaxt="n", yaxt="n", bty="n")
  axis(side=1, at=yr.labs, labels=sa.axis.x[i], las=1, mgp=c(1,-0.1,0), tcl=-0.1, cex.axis=0.75, lwd=0.75)
  axis(side=2, at=c(-2,0,2), labels=sa.axis.y[i], las=1, mgp=c(1,.2,0), tcl=-0.1, cex.axis=0.75, lwd=0.75)
  abline(v=yr.labs, col=rgb(.6,.6,.6,1), lwd=0.5, lty=3)
  abline(h=0, col=rgb(.6,.6,.6,1), lwd=0.5, lty=3)
  text(x=1986, y=-2.8, pos=4, cnt.nam[i], cex=0.85)
  lines(y=dem, x=yr, col=rgb(0,0,0,0.8), lwd=1.5, lty=1)
  lines(y=sup, x=yr, col=rgb(0,.4,.4,1), lwd=1.5, lty=1)
  points(x=yr[resp], y=sup[resp], pch="I", cex=0.7, col=rgb(0,.4,.4,1))
  polygon(y=c(sup.u, rev(sup.l)), x=c(yr, rev(yr)), col=rgb(0,.4,.4,0.3), border=NA)
  box(lty=1, lwd=0.75)
  if(i==1) {
    text(x=1987, y=dem[2]+0.5, pos=4, "Democracy", col=rgb(0,0,0,1), cex=0.75)
    text(x=2007, y=sup[26]+0.4, pos=4, "Mood", col=rgb(0,.4,.4,1), cex=0.75)
  }
}
dev.off()

# transitioning regimes

length(cnts.rt) # 40
rt.axis.x = vector("logical", length(cnts.rt))
rt.axis.x[36:40] = TRUE
rt.axis.y = vector("logical", length(cnts.rt))
rt.axis.y[c(1,6,11,16,21,26,31,36)] = TRUE
cnt.nam = cnts.rt

pdf('fig4.pdf', width=6.5, height=8)
par(mfrow=c(8, 5), mar=c(1, 1, 0.25, 0.25), tcl=-0.2, las=1, cex=0.7)
for(i in 1:length(cnts.rt)) {
  sup = supdem.plot[supdem.plot$Country == cnts.rt[i], "SupDem_trim"]
  sup.u = supdem.plot[supdem.plot$Country == cnts.rt[i], "SupDem_u90"]
  sup.l = supdem.plot[supdem.plot$Country == cnts.rt[i], "SupDem_l90"]
  yr = as.numeric(supdem.plot[supdem.plot$Country == cnts.rt[i], "Year"])
  dem = supdem.plot[supdem.plot$Country == cnts.rt[i], "Libdem_z"]
  resp = supdem.plot[supdem.plot$Country == cnts.rt[i], "N_response"] > 0
  plot(y=sup, x=yr, type="n", xlab="", ylab="", main="", ylim=c(-3, 3.5), xaxt="n", yaxt="n", bty="n")
  axis(side=1, at=yr.labs, labels=rt.axis.x[i], las=1, mgp=c(1,-0.1,0), tcl=-0.1, cex.axis=0.75, lwd=0.75)
  axis(side=2, at=c(-2,0,2), labels=rt.axis.y[i], las=1, mgp=c(1,.2,0), tcl=-0.1, cex.axis=0.75, lwd=0.75)
  abline(v=yr.labs, col=rgb(.6,.6,.6,1), lwd=0.5, lty=3)
  abline(h=0, col=rgb(.6,.6,.6,1), lwd=0.5, lty=3)
  text(x=1986, y=-2.8, pos=4, cnt.nam[i], cex=0.85)
  lines(y=dem, x=yr, col=rgb(0,0,0,0.8), lwd=1.5, lty=1)
  lines(y=sup, x=yr, col=rgb(0,.4,.4,1), lwd=1.5, lty=1)
  points(x=yr[resp], y=sup[resp], pch="I", cex=0.7, col=rgb(0,.4,.4,1))
  polygon(y=c(sup.u, rev(sup.l)), x=c(yr, rev(yr)), col=rgb(0,.4,.4,0.3), border=NA)
  box(lty=1, lwd=0.75)
  if(i==1) {
    text(x=1990, y=dem[10]-0.5, pos=4, "Democracy", col=rgb(0,0,0,1), cex=0.75)
    text(x=2007, y=sup[20]+0.6, pos=4, "Mood", col=rgb(0,.4,.4,1), cex=0.75)
  }
}
dev.off()



