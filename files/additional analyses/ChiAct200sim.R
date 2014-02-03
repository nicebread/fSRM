library(lavaan)

# simulate data with different sample sizes (H0): actoreffects are equal
SRM_sim <- '
ffam  =~ 1*YMOFA+1*YFAMO+1*YMOC1+1*YC1MO+1*YMOC2+1*YC2MO+
1*YFAC1+1*YC1FA+1*YFAC2+1*YC2FA+1*YC1C2+1*YC2C1
factmo=~ 1*YMOFA+1*YMOC1+1*YMOC2; 
factfa=~ 1*YFAMO+1*YFAC1+1*YFAC2
factc1=~ 1*YC1FA+1*YC1MO+1*YC1C2; 
factc2=~ 1*YC2MO+1*YC2C1+1*YC2FA
fparmo=~ 1*YFAMO+1*YC1MO+1*YC2MO; 
fparfa=~ 1*YMOFA+1*YC1FA+1*YC2FA
fparc1=~ 1*YFAC1+1*YMOC1+1*YC2C1; 
fparc2=~ 1*YMOC2+1*YC1C2+1*YFAC2
fmofa=~ 1*YMOFA;
ffamo=~1*YFAMO;
fmoc1=~ 1*YMOC1;
fc1mo=~1*YC1MO;
fmoc2=~ 1*YMOC2;
fc2mo=~1*YC2MO;
ffac1=~ 1*YFAC1;
fc1fa=~1*YC1FA;
ffac2=~ 1*YFAC2;
fc2fa=~1*YC2FA;
fc1c2=~ 1*YC1C2;
fc2c1=~1*YC2C1;

ffam~~0.053*ffam
factmo~~0.199*factmo;
factfa~~0.232*factfa; 
factc1~~0.149*factc1; 
factc2~~0.265*factc2;
fparmo~~0.035*fparmo; 
fparfa~~0.108*fparfa; 
fparc1~~0.039*fparc1; 
fparc2~~0.090*fparc2;
fmofa~~0.251*fmofa;
ffamo~~0.643*ffamo;
fmoc1~~0.182*fmoc1;
fc1mo~~0.119*fc1mo;
fmoc2~~0.254*fmoc2;
fc2mo~~0.178*fc2mo;
ffac1~~0.16*ffac1;
fc1fa~~0.173*fc1fa;
ffac2~~0.112*ffac2;
fc2fa~~0.181*fc2fa;
fc2c1~~0.321*fc2c1;
fc1c2~~0.149*fc1c2;
factmo~~0.001*fparmo; 
factfa~~0.011*fparfa; 
factc1~~0.034*fparc1;
factc2~~0.082*fparc2;
ffamo~~0.041*fmofa;
fc1mo~~0.041*fmoc1;
fc2mo~~0.046*fmoc2;
fc1fa~~0.060*ffac1;
fc2fa~~0.041*ffac2;
fc1c2~~0.01*fc2c1;

ffam   ~ 1.801*1 
factmo ~ 0*1
factfa ~ 0*1
factc1 ~ 0*1
factc2 ~ 0*1
fparmo ~ -0.141*1
fparfa ~ 0.049*1
fparc1 ~ -0.026*1
fparc2 ~ 0.118*1
fmofa ~ 0.009*1
ffamo ~ 0.1*1
fmoc1 ~ -0.022*1
fc1mo ~ -0.048*1
fmoc2 ~ 0.013*1
fc2mo ~ -0.052*1
ffac1 ~ -0.028*1
fc1fa ~ -0.011*1
ffac2 ~ -0.071*1
fc2fa ~ 0.002*1
fc1c2 ~ 0.059*1
fc2c1 ~ 0.05*1

# mean/intercepts observed
YMOFA ~ 0
YFAMO ~ 0
YMOC1 ~ 0
YC1MO ~ 0
YMOC2 ~ 0
YC2MO ~ 0
YFAC1 ~ 0
YC1FA ~ 0
YFAC2 ~ 0
YC2FA ~ 0
YC1C2 ~ 0
YC2C1 ~ 0

# variances observed
YMOFA~~0*YMOFA
YFAMO~~0*YFAMO
YMOC1~~0*YMOC1
YC1MO~~0*YC1MO
YMOC2~~0*YMOC2
YC2MO~~0*YC2MO
YFAC1~~0*YFAC1
YC1FA~~0*YC1FA
YFAC2~~0*YFAC2
YC2FA~~0*YC2FA
YC1C2~~0*YC1C2
YC2C1~~0*YC2C1
'

# Fit simulated data with an unconstrained model
SRM <- '
ffam  =~ 1*YMOFA+1*YFAMO+1*YMOC1+1*YC1MO+1*YMOC2+1*YC2MO+
1*YFAC1+1*YC1FA+1*YFAC2+1*YC2FA+1*YC1C2+1*YC2C1
factmo=~ 1*YMOFA+1*YMOC1+1*YMOC2; factfa=~ 1*YFAMO+1*YFAC1+1*YFAC2
factc1=~ 1*YC1FA+1*YC1MO+1*YC1C2; factc2=~ 1*YC2MO+1*YC2C1+1*YC2FA
fparmo=~ 1*YFAMO+1*YC1MO+1*YC2MO; fparfa=~ 1*YMOFA+1*YC1FA+1*YC2FA
fparc1=~ 1*YFAC1+1*YMOC1+1*YC2C1; fparc2=~ 1*YMOC2+1*YC1C2+1*YFAC2
fmofa=~ 1*YMOFA;ffamo=~1*YFAMO;
fmoc1=~ 1*YMOC1;fc1mo=~1*YC1MO;
fmoc2=~ 1*YMOC2;fc2mo=~1*YC2MO;
ffac1=~ 1*YFAC1;fc1fa=~1*YC1FA;
ffac2=~ 1*YFAC2;fc2fa=~1*YC2FA;
fc1c2=~ 1*YC1C2;fc2c1=~1*YC2C1;


ffam~~ffam
factmo~~factmo; factfa~~factfa; factc1~~factc1; factc2~~factc2;
fparmo~~fparmo; fparfa~~fparfa; fparc1~~fparc1; fparc2~~fparc2;
fmofa~~fmofa;ffamo~~ffamo;fmoc1~~fmoc1;fc1mo~~fc1mo;fmoc2~~fmoc2;fc2mo~~fc2mo;
ffac1~~ffac1;fc1fa~~fc1fa;ffac2~~ffac2;fc2fa~~fc2fa;fc2c1~~fc2c1;fc1c2~~fc1c2;
factmo~~fparmo; factfa~~fparfa; factc1~~fparc1; factc2~~fparc2;
ffamo~~fmofa;fc1mo~~fmoc1;fc2mo~~fmoc2;fc1fa~~ffac1;fc2fa~~ffac2;fc1c2~~fc2c1;

ffam   ~ fam*1
factmo ~ actmo*1
factfa ~ actfa*1
factc1 ~ actc1*1
factc2 ~ actc2*1
fparmo ~ parmo*1
fparfa ~ parfa*1
fparc1 ~ parc1*1
fparc2 ~ parc2*1
fmofa ~ mofa*1
ffamo ~ famo*1
fmoc1 ~ moc1*1
fc1mo ~ c1mo*1
fmoc2 ~ moc2*1
fc2mo ~ c2mo*1
ffac1 ~ fac1*1
fc1fa ~ c1fa*1
ffac2 ~ fac2*1
fc2fa ~ c2fa*1
fc1c2 ~ c1c2*1
fc2c1 ~ c2c1*1

# mean/intercepts observed
YMOFA ~ 0
YFAMO ~ 0
YMOC1 ~ 0
YC1MO ~ 0
YMOC2 ~ 0
YC2MO ~ 0
YFAC1 ~ 0
YC1FA ~ 0
YFAC2 ~ 0
YC2FA ~ 0
YC1C2 ~ 0
YC2C1 ~ 0

# variances observed
YMOFA~~0*YMOFA
YFAMO~~0*YFAMO
YMOC1~~0*YMOC1
YC1MO~~0*YC1MO
YMOC2~~0*YMOC2
YC2MO~~0*YC2MO
YFAC1~~0*YFAC1
YC1FA~~0*YC1FA
YFAC2~~0*YFAC2
YC2FA~~0*YC2FA
YC1C2~~0*YC1C2
YC2C1~~0*YC2C1


# Constraints
actmo+actfa+actc1+actc2==0
parmo+parfa+parc1+parc2==0
mofa+moc1+moc2==0
famo+c1mo+c2mo==0
famo+fac1+fac2==0
mofa+c1fa+c2fa==0
c1mo+c1fa+c1c2==0
moc1+fac1+c2c1==0
c2mo+c2fa+c2c1==0
moc2+fac2+c1c2==0
'



# Fit simulated data with a constrained model

SRM_act <- '
ffam  =~ 1*YMOFA+1*YFAMO+1*YMOC1+1*YC1MO+1*YMOC2+1*YC2MO+
1*YFAC1+1*YC1FA+1*YFAC2+1*YC2FA+1*YC1C2+1*YC2C1
factmo=~ 1*YMOFA+1*YMOC1+1*YMOC2; factfa=~ 1*YFAMO+1*YFAC1+1*YFAC2
factc1=~ 1*YC1FA+1*YC1MO+1*YC1C2; factc2=~ 1*YC2MO+1*YC2C1+1*YC2FA
fparmo=~ 1*YFAMO+1*YC1MO+1*YC2MO; fparfa=~ 1*YMOFA+1*YC1FA+1*YC2FA
fparc1=~ 1*YFAC1+1*YMOC1+1*YC2C1; fparc2=~ 1*YMOC2+1*YC1C2+1*YFAC2
fmofa=~ 1*YMOFA;ffamo=~1*YFAMO;
fmoc1=~ 1*YMOC1;fc1mo=~1*YC1MO;
fmoc2=~ 1*YMOC2;fc2mo=~1*YC2MO;
ffac1=~ 1*YFAC1;fc1fa=~1*YC1FA;
ffac2=~ 1*YFAC2;fc2fa=~1*YC2FA;
fc1c2=~ 1*YC1C2;fc2c1=~1*YC2C1;


ffam~~ffam
factmo~~factmo; factfa~~factfa; factc1~~factc1; factc2~~factc2;
fparmo~~fparmo; fparfa~~fparfa; fparc1~~fparc1; fparc2~~fparc2;
fmofa~~fmofa;ffamo~~ffamo;fmoc1~~fmoc1;fc1mo~~fc1mo;fmoc2~~fmoc2;fc2mo~~fc2mo;
ffac1~~ffac1;fc1fa~~fc1fa;ffac2~~ffac2;fc2fa~~fc2fa;fc2c1~~fc2c1;fc1c2~~fc1c2;
factmo~~fparmo; factfa~~fparfa; factc1~~fparc1; factc2~~fparc2;
ffamo~~fmofa;fc1mo~~fmoc1;fc2mo~~fmoc2;fc1fa~~ffac1;fc2fa~~ffac2;fc1c2~~fc2c1;

ffam   ~ fam*1
factmo ~ actmo*1
factfa ~ actfa*1
factc1 ~ actc1*1
factc2 ~ actc2*1
fparmo ~ parmo*1
fparfa ~ parfa*1
fparc1 ~ parc1*1
fparc2 ~ parc2*1
fmofa ~ mofa*1
ffamo ~ famo*1
fmoc1 ~ moc1*1
fc1mo ~ c1mo*1
fmoc2 ~ moc2*1
fc2mo ~ c2mo*1
ffac1 ~ fac1*1
fc1fa ~ c1fa*1
ffac2 ~ fac2*1
fc2fa ~ c2fa*1
fc1c2 ~ c1c2*1
fc2c1 ~ c2c1*1

# mean/intercepts observed
YMOFA ~ 0
YFAMO ~ 0
YMOC1 ~ 0
YC1MO ~ 0
YMOC2 ~ 0
YC2MO ~ 0
YFAC1 ~ 0
YC1FA ~ 0
YFAC2 ~ 0
YC2FA ~ 0
YC1C2 ~ 0
YC2C1 ~ 0

# variances observed
YMOFA~~0*YMOFA
YFAMO~~0*YFAMO
YMOC1~~0*YMOC1
YC1MO~~0*YC1MO
YMOC2~~0*YMOC2
YC2MO~~0*YC2MO
YFAC1~~0*YFAC1
YC1FA~~0*YC1FA
YFAC2~~0*YFAC2
YC2FA~~0*YC2FA
YC1C2~~0*YC1C2
YC2C1~~0*YC2C1


# Constraints
actmo+actfa+actc1+actc2==0
parmo+parfa+parc1+parc2==0
mofa+moc1+moc2==0
famo+c1mo+c2mo==0
famo+fac1+fac2==0
mofa+c1fa+c2fa==0
c1mo+c1fa+c1c2==0
moc1+fac1+c2c1==0
c2mo+c2fa+c2c1==0
moc2+fac2+c1c2==0


actmo == actfa
actfa == actc1
actc1 == actc2
'

# test whether the constrained and the unconstrained model differ significantly (with different sample sizes)
chi25 <- c()
p25 <- c()
N <- 200
for(i in 1:N) {
  set.seed(i)
  sim.data <- simulateData(SRM_sim, sample.nobs=25)  
  fit <- lavaan(SRM, data=sim.data)
  fit_act <- lavaan(SRM_act, data=sim.data)
  chi25 <- c(chi25, anova(fit,fit_act)[2,5])
  p25 <- c(p25, anova(fit,fit_act)[2,7])
}

TI_act25 <- length(chi25[chi25>7.815])
actTI25 <- cbind(chi25, p25, TI_act25)
write.table(actTI25, "chi_act_TI25.csv")



chi50 <- c()
p50 <- c()
N <- 200
for(i in 1:N) {
  set.seed(i)
  sim.data <- simulateData(SRM_sim, sample.nobs=50)  
  fit <- lavaan(SRM, data=sim.data)
  fit_act <- lavaan(SRM_act, data=sim.data)
  chi50 <- c(chi50, anova(fit,fit_act)[2,5])
  p50 <- c(p50, anova(fit,fit_act)[2,7])
}

TI_act50 <- length(chi50[chi50>7.815])
actTI50 <- cbind(chi50, p50, TI_act50)
write.table(actTI50, "chi_act_TI50.csv")



chi100 <- c()
p100 <- c()
N <- 200
for(i in 1:N) {
  set.seed(i)
  sim.data <- simulateData(SRM_sim, sample.nobs=100)  
  fit <- lavaan(SRM, data=sim.data)
  fit_act <- lavaan(SRM_act, data=sim.data)
  chi100 <- c(chi100, anova(fit,fit_act)[2,5])
  p100 <- c(p100, anova(fit,fit_act)[2,7])
}

TI_act100 <- length(chi100[chi100>7.815])
actTI100 <- cbind(chi100, p100, TI_act100)
write.table(actTI100, "chi_act_TI100.csv")


