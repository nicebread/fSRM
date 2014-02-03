library(foreign)
# data from book Kenny et al. p.240
setwd("C:\\SRM\\")
anx<-read.spss("4person.sav",to.data.frame = TRUE)
# rename variables
names(anx) <- c("id2", "YMOFA", "YMOC1", "YMOC2", "YFAMO", "YFAC1", "YFAC2", 
                "YC1MO", "YC1FA", "YC1C2", "YC2MO", "YC2FA", "YC2C1")
anx$groep<-c(rep(0,104),rep(1,104))


library(lavaan)

SRM3 <- '
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


ffam   ~ fam*1+beta0f*groep
factmo ~ actmo*1+beta0AM*groep
factfa ~ actfa*1+beta0AF*groep
factc1 ~ actc1*1+beta0A1*groep
factc2 ~ actc2*1+beta0A2*groep
fparmo ~ parmo*1+beta0PM*groep
fparfa ~ parfa*1+beta0PF*groep
fparc1 ~ parc1*1+beta0P1*groep
fparc2 ~ parc2*1+beta0P2*groep
fmofa ~ mofa*1+beta0MF*groep
ffamo ~ famo*1+beta0FM*groep
fmoc1 ~ moc1*1+beta0M1*groep
fc1mo ~ c1mo*1+beta01M*groep
fmoc2 ~ moc2*1+beta0M2*groep
fc2mo ~ c2mo*1+beta02M*groep
ffac1 ~ fac1*1+beta0F1*groep
fc1fa ~ c1fa*1+beta01F*groep
ffac2 ~ fac2*1+beta0F2*groep
fc2fa ~ c2fa*1+beta02F*groep
fc1c2 ~ c1c2*1+beta012*groep
fc2c1 ~ c2c1*1+beta021*groep

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

beta0AM+beta0AF+beta0A1+beta0A2==0
beta0PM+beta0PF+beta0P1+beta0P2==0
beta0MF+beta0M1+beta0M2==0
beta0FM+beta01M+beta02M==0
beta0FM+beta0F1+beta0F2==0
beta0MF+beta01F+beta02F==0
beta01M+beta01F+beta012==0
beta0M1+beta0F1+beta021==0
beta02M+beta02F+beta021==0
beta0M2+beta0F2+beta012==0
'


fit3 <- lavaan(SRM3, data=anx,auto.var=TRUE)
summary(fit3)

