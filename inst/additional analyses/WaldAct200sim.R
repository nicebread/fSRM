library(lavaan)
library(semTools)


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




# Fit data with basic model (both variances and means are estimated)
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

#variances
ffam~~ffam
factmo~~factmo; factfa~~factfa; factc1~~factc1; factc2~~factc2;
fparmo~~fparmo; fparfa~~fparfa; fparc1~~fparc1; fparc2~~fparc2;
fmofa~~fmofa;ffamo~~ffamo;fmoc1~~fmoc1;fc1mo~~fc1mo;fmoc2~~fmoc2;fc2mo~~fc2mo;
ffac1~~ffac1;fc1fa~~fc1fa;ffac2~~ffac2;fc2fa~~fc2fa;fc2c1~~fc2c1;fc1c2~~fc1c2;

# gen. rec.
factmo~~fparmo; factfa~~fparfa; factc1~~fparc1; factc2~~fparc2;

# dyad. rec.
ffamo~~fmofa;fc1mo~~fmoc1;fc2mo~~fmoc2;fc1fa~~ffac1;fc2fa~~ffac2;fc1c2~~fc2c1;

# define labels for all latent variables
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


# apply the formulas for the Wald-tests and calculate the TI-error rate of this method (for different sample sizes)
#n=25
N <- 200
actc2_25 <- c()
actc1_25 <- c()
actmo_25 <- c()
actfa_25 <- c()
for(i in 1:N) {
  set.seed(i)
  data_h025 <- simulateData(SRM_sim, sample.nobs=25)  
  fit25 <- lavaan(SRM, data=data_h025)
  coeff25 <- cbind(parameterEstimates(fit25)[80:100,4], parameterEstimates(fit25)[80:100,5])
  
  
  # take the actor variances, always leave one out
  act_c225 <- as.double(coeff25[2:4,2])
  act_c125 <- as.double(coeff25[c(2,3,5),2])
  act_mo25 <- as.double(coeff25[3:5,2])  
  act_fa25 <- as.double(coeff25[c(2,4,5),2])
  vcov25 <- vcov(fit25)
  
  vcov_c225 <- vcov25[33:35, 33:35]
  actc2_25 <- c(actc2_25, act_c225 %*% solve(vcov_c225) %*% act_c225)
  
  vcov_c125 <- vcov25[c(33,34,36), c(33,34,36)]
  actc1_25 <- c(actc1_25, act_c125 %*% solve(vcov_c125) %*% act_c125)
  vcov_mo25 <- vcov25[34:36, 34:36]
  actmo_25 <- c(actmo_25, act_mo25 %*% solve(vcov_mo25) %*% act_mo25)
  vcov_fa25 <- vcov25[c(33,35,36), c(33,35,36)]
  actfa_25 <- c(actfa_25, act_fa25 %*% solve(vcov_fa25) %*% act_fa25)
  }

write.table(actc2_25, "actc2_h025.csv")
a <- length(actc2_25[actc2_25>7.815])
write.table(actc1_25, "actc1_h025.csv")
d <- length(actc1_25[actc1_25>7.815])
write.table(actmo_25, "actmo_h025.csv")
g <- length(actmo_25[actmo_25>7.815])
write.table(actfa_25, "actfa_h025.csv")
j <- length(actfa_25[actfa_25>7.815])


#n=50
N <- 200
actc2_50 <- c()
actc1_50 <- c()
actmo_50 <- c()
actfa_50 <- c()
for(i in 1:N) {
  set.seed(i)
  data_h050 <- simulateData(SRM_sim, sample.nobs=50)  
  fit50 <- lavaan(SRM, data=data_h050)
  coeff50 <- cbind(parameterEstimates(fit50)[80:100,4], parameterEstimates(fit50)[80:100,5])
  act_c250 <- as.double(coeff50[2:4,2])
  act_c150 <- as.double(coeff50[c(2,3,5),2])
  act_mo50 <- as.double(coeff50[3:5,2]) 
  act_fa50 <- as.double(coeff50[c(2,4,5),2])
  vcov50 <- vcov(fit50)
  vcov_c250 <- vcov50[33:35, 33:35]
  actc2_50 <- c(actc2_50, act_c250 %*% solve(vcov_c250) %*% act_c250)
  vcov_c150 <- vcov50[c(33,34,36), c(33,34,36)]
  actc1_50 <- c(actc1_50, act_c150 %*% solve(vcov_c150) %*% act_c150)
  vcov_mo50 <- vcov50[34:36, 34:36]
  actmo_50 <- c(actmo_50, act_mo50 %*% solve(vcov_mo50) %*% act_mo50)
  vcov_fa50 <- vcov50[c(33,35,36), c(33,35,36)]
  actfa_50 <- c(actfa_50, act_fa50 %*% solve(vcov_fa50) %*% act_fa50)
}

write.table(actc2_50, "actc2_h050.csv")
b <- length(actc2_50[actc2_50>7.815])
write.table(actc1_50, "actc1_h050.csv")
e <- length(actc1_50[actc1_50>7.815])
write.table(actmo_50, "actmo_h050.csv")
h <- length(actmo_50[actmo_50>7.815])
write.table(actfa_50, "actfa_h050.csv")
k <- length(actfa_50[actfa_50>7.815])



#n=100
N <- 200
actc2_100 <- c()
actc1_100 <- c()
actmo_100 <- c()
actfa_100 <- c()
for(i in 1:N) {
  set.seed(i)
  data_h0100 <- simulateData(SRM_sim, sample.nobs=100)  
  fit100 <- lavaan(SRM, data=data_h0100)
  coeff100 <- cbind(parameterEstimates(fit100)[80:100,4], parameterEstimates(fit100)[80:100,5])
  act_c2100 <- as.double(coeff100[2:4,2])
  act_c1100 <- as.double(coeff100[c(2,3,5),2])
  act_mo100 <- as.double(coeff100[3:5,2])  
  act_fa100 <- as.double(coeff100[c(2,4,5),2])
  vcov100 <- vcov(fit100)
  vcov_c2100 <- vcov100[33:35, 33:35]
  actc2_100 <- c(actc2_100, act_c2100 %*% solve(vcov_c2100) %*% act_c2100)
  vcov_c1100 <- vcov100[c(33,34,36), c(33,34,36)]
  actc1_100 <- c(actc1_100, act_c1100 %*% solve(vcov_c1100) %*% act_c1100)
  vcov_mo100 <- vcov100[34:36, 34:36]
  actmo_100 <- c(actmo_100, act_mo100 %*% solve(vcov_mo100) %*% act_mo100)
  vcov_fa100 <- vcov100[c(33,35,36), c(33,35,36)]
  actfa_100 <- c(actfa_100, act_fa100 %*% solve(vcov_fa100) %*% act_fa100)
}

write.table(actc2_100, "actc2_h0100.csv")
c <- length(actc2_100[actc2_100>7.815])
write.table(actc1_100, "actc1_h0100.csv")
f <- length(actc1_100[actc1_100>7.815])
write.table(actmo_100, "actmo_h0100.csv")
i <- length(actmo_100[actmo_100>7.815])
write.table(actfa_100, "actfa_h0100.csv")
l <- length(actfa_100[actfa_100>7.815])



# Combine results
n <- c(25, 50, 100)
w_actc2 <- c(a,b,c)
w_actc1 <- c(d,e,f)
w_actmo <- c(g,h,i)
w_actfa <- c(j,k,l)
wald <- cbind(n, w_actmo,w_actfa, w_actc1, w_actc2)
write.table(wald, "wald_TI_act.csv")
