
#################################################### Read in the dataset + datacleaning ##################################################################

library(lavaan) 
library(Hmisc)
library(foreign)

# Better point to a relative path in the package directory
Branje <- spss.get("datasets/Branje.sav", use.value.labels=T)
attach(Branje)
BranjeSRM <- cbind(group, nrinegmv1, nrinegmv2, nrinegms1, nrinegms2, nrinegma1, nrinegma2, 
                   nrinegvm1, nrinegvm2, nrinegvs1, nrinegvs2, nrinegva1, nrinegva2, nrinegsm1, nrinegsm2,
                   nrinegsv1, nrinegsv2, nrinegsa1, nrinegsa2, nrinegam1, nrinegam2, nrinegav1, nrinegav2,
                   nrinegas1, nrinegas2)
              # group: 1 = above norm, 2 = below norm
detach(Branje)
BranjeSRM <- as.data.frame(BranjeSRM)
names(BranjeSRM) <- c('group', 'MOFAa', 'MOFAb', 'MOC1a', 'MOC1b', 'MOC2a', 'MOC2b', 'FAMOa', 'FAMOb','FAC1a','FAC1b','FAC2a', 'FAC2b', 'C1MOa','C1MOb', 'C1FAa', 'C1FAb', 'C1C2a', 'C1C2b', 'C2MOa' , 'C2MOb','C2FAa','C2FAb','C2C1a','C2C1b')

# MD:
  # EQS deletes automatically all the observations with missing data: 47 in group 1 en 17 in group 2 
BranjeSRM[BranjeSRM==999]  <-  NA
BranjeSRM <- na.omit(BranjeSRM)


# clarification code:
  # small a en b represent the indicators
  # capital A en B represent the two different groups 
  # in the original data S = C1 and A = TARGET = C2
  # group 1 (original data) = B (our code) = problematic families ; A = nonproblematic families




######################################################### DESCRIPTIVES ########################################################################################
# Calculations of mean scores and sd per relationships are calculated, together with an anova
attach(BranjeSRM)
library(doBy)
C2MO <- (C2MOa + C2MOb)/2
summaryBy(C2MO~group, data=BranjeSRM, FUN=c(mean, sd))
fit <- lm(C2MO ~ group)
anova(fit)
# same results

C2FA <- (C2FAa + C2FAb)/2
summaryBy(C2FA~group, data=BranjeSRM, FUN=c(mean, sd))
fit <- lm(C2FA ~ group)
anova(fit)
# same results

C2C1 <- (C2C1a + C2C1b)/2
summaryBy(C2C1~group, data=BranjeSRM, FUN=c(mean, sd))
fit <- lm(C2C1 ~ group)
anova(fit)
# same results

MOC2 <- (MOC2a + MOC2b)/2
summaryBy(MOC2~group, data=BranjeSRM, FUN=c(mean, sd))
fit <- lm(MOC2 ~ group)
anova(fit)
# same results, only F-value is 66.66 (not 66.60)

FAC2 <- (FAC2a + FAC2b)/2
summaryBy(FAC2~group, data=BranjeSRM, FUN=c(mean, sd))
fit <- lm(FAC2 ~ group)
anova(fit)
# same results, only F-value is 52.33 (not 52.37)

C1C2 <- (C1C2a + C1C2b)/2
summaryBy(C1C2~group, data=BranjeSRM, FUN=c(mean, sd))
fit <- lm(C1C2 ~ group)
anova(fit)
# same results

C1MO <- (C1MOa + C1MOb)/2
summaryBy(C1MO~group, data=BranjeSRM, FUN=c(mean, sd))
fit <- lm(C1MO ~ group)
anova(fit)
# same results

C1FA <- (C1FAa + C1FAb)/2
summaryBy(C1FA~group, data=BranjeSRM, FUN=c(mean, sd))
fit <- lm(C1FA ~ group)
anova(fit)
# same results

MOC1 <- (MOC1a + MOC1b)/2
summaryBy(MOC1~group, data=BranjeSRM, FUN=c(mean, sd))
fit <- lm(MOC1 ~ group)
anova(fit)
# same results

MOFA <- (MOFAa + MOFAb)/2
summaryBy(MOFA~group, data=BranjeSRM, FUN=c(mean, sd))
fit <- lm(MOFA ~ group)
anova(fit)
# same results, only P-value is 0.012 (not < 0.01)         

FAC1 <- (FAC1a + FAC1b)/2
summaryBy(FAC1~group, data=BranjeSRM, FUN=c(mean, sd))
fit <- lm(FAC1 ~ group)
anova(fit)
# same results

FAMO <- (FAMOa + FAMOb)/2
summaryBy(FAMO~group, data=BranjeSRM, FUN=c(mean, sd))
fit <- lm(FAMO ~ group)
anova(fit)
# same results, only F-value is 2.16







######################################################### SRM ANALYSES ########################################################################################

# This code is identical to the EQS-code Eichelsheim et al. (2011) used. 
  # The errorterms that were accidently missing are also left out (MOFAb ~~ C2C1b and MOC2b ~~ C2C1b) and the same reciprocities are used. 
  # The mother actor effect was also set equal for both groups
  # mimic="EQS" was used to resemble the EQS-results more closely

SRM2grp_actmo<- '
ffam  =~ 1*MOFAa+1*FAMOa+1*MOC1a+1*C1MOa+1*MOC2a+1*C2MOa+ 1*FAC1a+1*C1FAa+1*FAC2a+1*C2FAa+1*C1C2a+1*C2C1a + 
          1*MOFAb+1*FAMOb+1*MOC1b+1*C1MOb+1*MOC2b+1*C2MOb+ 1*FAC1b+1*C1FAb+1*FAC2b+1*C2FAb+1*C1C2b+1*C2C1b
factmo=~ 1*MOFAa+1*MOC1a+1*MOC2a + 
1*MOFAb+1*MOC1b+1*MOC2b 
factfa=~ 1*FAMOa+1*FAC1a+1*FAC2a +
1*FAMOb+1*FAC1b+1*FAC2b
factc1=~ 1*C1FAa+1*C1MOa+1*C1C2a +
1*C1FAb+1*C1MOb+1*C1C2b
factc2=~ 1*C2MOa+1*C2C1a+1*C2FAa +
1*C2MOb+1*C2C1b+1*C2FAb
fparmo=~ 1*FAMOa+1*C1MOa+1*C2MOa +
1*FAMOb+1*C1MOb+1*C2MOb
fparfa=~ 1*MOFAa+1*C1FAa+1*C2FAa +
1*MOFAb+1*C1FAb+1*C2FAb
fparc1=~ 1*FAC1a+1*MOC1a+1*C2C1a +
1*FAC1b+1*MOC1b+1*C2C1b
fparc2=~ 1*MOC2a+1*C1C2a+1*FAC2a +
1*MOC2b+1*C1C2b+1*FAC2b
fmofa=~ 1*MOFAa + 1*MOFAb
ffamo=~ 1*FAMOa + 1*FAMOb
fmoc1=~ 1*MOC1a + 1*MOC1b
fc1mo=~ 1*C1MOa + 1*C1MOb
fmoc2=~ 1*MOC2a + 1*MOC2b
fc2mo=~ 1*C2MOa + 1*C2MOb
ffac1=~ 1*FAC1a + 1*FAC1b
fc1fa=~ 1*C1FAa + 1*C1FAb
ffac2=~ 1*FAC2a + 1*FAC2b
fc2fa=~ 1*C2FAa + 1*C2FAb 
fc1c2=~ 1*C1C2a + 1*C1C2b
fc2c1=~ 1*C2C1a + 1*C2C1b

ffam~~c(varfamA,varfamB)*ffam
factmo~~c(varactmoA,varactmoB)*factmo
factfa~~c(varactfaA,varactfaB)*factfa
factc1~~c(varactc1A,varactc1B)*factc1
factc2~~c(varactc2A,varactc2B)*factc2
fparmo~~c(varparmoA,varparmoB)*fparmo
fparfa~~c(varparfaA,varparfaB)*fparfa
fparc1~~c(varparc1A,varparc1B)*fparc1
fparc2~~c(varparc2A,varparc2B)*fparc2
fmofa~~c(varmofaA,varmofaB)*fmofa
ffamo~~c(varfamoA,varfamoB)*ffamo
fmoc1~~c(varmoc1A,varmoc1B)*fmoc1
fc1mo~~c(varc1moA,varc1moB)*fc1mo
fmoc2~~c(varmoc2A,varmoc2B)*fmoc2
fc2mo~~c(varc2moA,varc2moB)*fc2mo
ffac1~~c(varfac1A,varfac1B)*ffac1
fc1fa~~c(varc1faA,varc1faB)*fc1fa
ffac2~~c(varfac2A,varfac2B)*ffac2
fc2fa~~c(varc2faA,varc2faB)*fc2fa
fc2c1~~c(varc2c1A,varc2c1B)*fc2c1
fc1c2~~c(varc1c2A,varc1c2B)*fc1c2

# reciprocities
factmo~~c(covarmoA,covarmoB)*fparmo
factfa~~c(covarfaA,covarfaB)*fparfa
factc1~~c(covarc1A,covarc1B)*fparc1
factc2~~c(covarc2A,covarc2B)*fparc2
ffamo~~c(covarmofaA,covarmofaB)*fmofa
fc1mo~~c(covarmofaA,covarmofaB)*fmoc1
fc2mo~~c(covarmoc2A,covarmoc2B)*fmoc2
fc1fa~~c(covarfac1A,covarfac1B)*ffac1
fc2fa~~c(covarc2faA,covarc2faB)*ffac2
fc1c2~~c(covarc2c1A,covarc2c1B)*fc2c1

ffam   ~ c(famA,famB)*1
factmo ~ c(actmoA,actmoB)*1
factfa ~ c(actfaA,actfaB)*1
factc1 ~ c(actc1A,actc1B)*1
factc2 ~ c(actc2A,actc2B)*1
fparmo ~ c(parmoA,parmoB)*1
fparfa ~ c(parfaA,parfaB)*1
fparc1 ~ c(parc1A,parc1B)*1
fparc2 ~ c(parc2A,parc2B)*1
fmofa ~ c(mofaA,mofaB)*1
ffamo ~ c(famoA,famoB)*1
fmoc1 ~ c(moc1A,moc1B)*1
fc1mo ~ c(c1moA,c1moB)*1
fmoc2 ~ c(moc2A,moc2B)*1
fc2mo ~ c(c2moA,c2moB)*1
ffac1 ~ c(fac1A,fac1B)*1
fc1fa ~ c(c1faA,c1faB)*1
ffac2 ~ c(fac2A,fac2B)*1
fc2fa ~ c(c2faA,c2faB)*1
fc1c2 ~ c(c1c2A,c1c2B)*1
fc2c1 ~ c(c2c1A,c2c1B)*1

# mean/intercepts observed
MOFAa ~ 0
FAMOa ~ 0
MOC1a ~ 0
C1MOa ~ 0
MOC2a ~ 0
C2MOa ~ 0
FAC1a ~ 0
C1FAa ~ 0
FAC2a ~ 0
C2FAa ~ 0
C1C2a ~ 0
C2C1a ~ 0

MOFAb ~ 0
FAMOb ~ 0
MOC1b ~ 0
C1MOb ~ 0
MOC2b ~ 0
C2MOb ~ 0
FAC1b ~ 0
C1FAb ~ 0
FAC2b ~ 0
C2FAb ~ 0
C1C2b ~ 0
C2C1b ~ 0


# fouttermen laten correleren zoals Cook ook deed:
# -> diegene die hij vergeten was er hier ook uitgelaten
MOFAa ~~ MOC1a
MOFAa ~~ MOC2a
MOFAa ~~ FAMOa
MOFAa ~~ FAC1a
MOFAa ~~ FAC2a
MOFAa ~~ C1MOa
MOFAa ~~ C1FAa
MOFAa ~~ C1C2a
MOFAa ~~ C2MOa
MOFAa ~~ C2FAa
MOFAa ~~ C2C1a

MOC1a ~~ MOC2a
MOC1a ~~ FAMOa
MOC1a ~~ FAC1a
MOC1a ~~ FAC2a
MOC1a ~~ C1MOa
MOC1a ~~ C1FAa
MOC1a ~~ C1C2a
MOC1a ~~ C2MOa
MOC1a ~~ C2FAa
MOC1a ~~ C2C1a

MOC2a ~~ FAMOa
MOC2a ~~ FAC1a
MOC2a ~~ FAC2a
MOC2a ~~ C1MOa
MOC2a ~~ C1FAa
MOC2a ~~ C1C2a
MOC2a ~~ C2MOa
MOC2a ~~ C2FAa
MOC2a ~~ C2C1a

FAMOa ~~ FAC1a
FAMOa ~~ FAC2a
FAMOa ~~ C1MOa
FAMOa ~~ C1FAa
FAMOa ~~ C1C2a
FAMOa ~~ C2MOa
FAMOa ~~ C2FAa
FAMOa ~~ C2C1a

FAC1a ~~ FAC2a
FAC1a ~~ C1MOa
FAC1a ~~ C1FAa
FAC1a ~~ C1C2a
FAC1a ~~ C2MOa
FAC1a ~~ C2FAa
FAC1a ~~ C2C1a


FAC2a ~~ C1MOa
FAC2a ~~ C1FAa
FAC2a ~~ C1C2a
FAC2a ~~ C2MOa
FAC2a ~~ C2FAa
FAC2a ~~ C2C1a 


C1MOa ~~ C1FAa
C1MOa ~~ C1C2a
C1MOa ~~ C2MOa
C1MOa ~~ C2FAa
C1MOa ~~ C2C1a

C1FAa ~~ C1C2a
C1FAa ~~ C2MOa
C1FAa ~~ C2FAa
C1FAa ~~ C2C1a

C1C2a ~~ C2MOa
C1C2a ~~ C2FAa
C1C2a ~~ C2C1a

C2MOa ~~ C2FAa
C2MOa ~~ C2C1a

C2FAa ~~ C2C1a 

MOFAb ~~ MOC1b
MOFAb ~~ MOC2b
MOFAb ~~ FAMOb
MOFAb ~~ FAC1b
MOFAb ~~ FAC2b
MOFAb ~~ C1MOb
MOFAb ~~ C1FAb
MOFAb ~~ C1C2b
MOFAb ~~ C2MOb
MOFAb ~~ C2FAb


MOC1b ~~ MOC2b
MOC1b ~~ FAMOb
MOC1b ~~ FAC1b
MOC1b ~~ FAC2b
MOC1b ~~ C1MOb
MOC1b ~~ C1FAb
MOC1b ~~ C1C2b
MOC1b ~~ C2MOb
MOC1b ~~ C2FAb
MOC1b ~~ C2C1b

MOC2b ~~ FAMOb
MOC2b ~~ FAC1b
MOC2b ~~ FAC2b
MOC2b ~~ C1MOb
MOC2b ~~ C1FAb
MOC2b ~~ C1C2b
MOC2b ~~ C2MOb
MOC2b ~~ C2FAb

FAMOb ~~ FAC1b
FAMOb ~~ FAC2b
FAMOb ~~ C1MOb
FAMOb ~~ C1FAb
FAMOb ~~ C1C2b
FAMOb ~~ C2MOb
FAMOb ~~ C2FAb
FAMOb ~~ C2C1b

FAC1b ~~ FAC2b
FAC1b ~~ C1MOb
FAC1b ~~ C1FAb
FAC1b ~~ C1C2b
FAC1b ~~ C2MOb
FAC1b ~~ C2FAb
FAC1b ~~ C2C1b


FAC2b ~~ C1MOb
FAC2b ~~ C1FAb
FAC2b ~~ C1C2b
FAC2b ~~ C2MOb
FAC2b ~~ C2FAb
FAC2b ~~ C2C1b

C1MOb ~~ C1FAb
C1MOb ~~ C1C2b
C1MOb ~~ C2MOb
C1MOb ~~ C2FAb
C1MOb ~~ C2C1b

C1FAb ~~ C1C2b
C1FAb ~~ C2MOb
C1FAb ~~ C2FAb
C1FAb ~~ C2C1b

C1C2b ~~ C2MOb
C1C2b ~~ C2FAb
C1C2b ~~ C2C1b

C2MOb ~~ C2FAb
C2MOb ~~ C2C1b

C2FAb ~~ C2C1b

# Constraints
actmoA+actfaA+actc1A+actc2A==0
parmoA+parfaA+parc1A+parc2A==0
mofaA+moc1A+moc2A==0
famoA+c1moA+c2moA==0
famoA+fac1A+fac2A==0
mofaA+c1faA+c2faA==0
c1moA+c1faA+c1c2A==0
moc1A+fac1A+c2c1A==0
c2moA+c2faA+c2c1A==0
moc2A+fac2A+c1c2A==0

actmoB+actfaB+actc1B+actc2B==0
parmoB+parfaB+parc1B+parc2B==0
mofaB+moc1B+moc2B==0
famoB+c1moB+c2moB==0
famoB+fac1B+fac2B==0
mofaB+c1faB+c2faB==0
c1moB+c1faB+c1c2B==0
moc1B+fac1B+c2c1B==0
c2moB+c2faB+c2c1B==0
moc2B+fac2B+c1c2B==0

actmoA==actmoB
'

fit2 <- lavaan(SRM2grp_actmo, data = BranjeSRM,group="group", auto.var=T,mimic="EQS")
summary(fit2, fit.measures=T)



# negative variances (varfamA en varfamB) were fixed to zero. 
SRM2grp_actmo2<- '
ffam  =~ 1*MOFAa+1*FAMOa+1*MOC1a+1*C1MOa+1*MOC2a+1*C2MOa+ 1*FAC1a+1*C1FAa+1*FAC2a+1*C2FAa+1*C1C2a+1*C2C1a + 
1*MOFAb+1*FAMOb+1*MOC1b+1*C1MOb+1*MOC2b+1*C2MOb+ 1*FAC1b+1*C1FAb+1*FAC2b+1*C2FAb+1*C1C2b+1*C2C1b
factmo=~ 1*MOFAa+1*MOC1a+1*MOC2a + 
1*MOFAb+1*MOC1b+1*MOC2b 
factfa=~ 1*FAMOa+1*FAC1a+1*FAC2a +
1*FAMOb+1*FAC1b+1*FAC2b
factc1=~ 1*C1FAa+1*C1MOa+1*C1C2a +
1*C1FAb+1*C1MOb+1*C1C2b
factc2=~ 1*C2MOa+1*C2C1a+1*C2FAa +
1*C2MOb+1*C2C1b+1*C2FAb
fparmo=~ 1*FAMOa+1*C1MOa+1*C2MOa +
1*FAMOb+1*C1MOb+1*C2MOb
fparfa=~ 1*MOFAa+1*C1FAa+1*C2FAa +
1*MOFAb+1*C1FAb+1*C2FAb
fparc1=~ 1*FAC1a+1*MOC1a+1*C2C1a +
1*FAC1b+1*MOC1b+1*C2C1b
fparc2=~ 1*MOC2a+1*C1C2a+1*FAC2a +
1*MOC2b+1*C1C2b+1*FAC2b
fmofa=~ 1*MOFAa + 1*MOFAb
ffamo=~ 1*FAMOa + 1*FAMOb
fmoc1=~ 1*MOC1a + 1*MOC1b
fc1mo=~ 1*C1MOa + 1*C1MOb
fmoc2=~ 1*MOC2a + 1*MOC2b
fc2mo=~ 1*C2MOa + 1*C2MOb
ffac1=~ 1*FAC1a + 1*FAC1b
fc1fa=~ 1*C1FAa + 1*C1FAb
ffac2=~ 1*FAC2a + 1*FAC2b
fc2fa=~ 1*C2FAa + 1*C2FAb 
fc1c2=~ 1*C1C2a + 1*C1C2b
fc2c1=~ 1*C2C1a + 1*C2C1b

ffam~~0*ffam
factmo~~c(varactmoA,varactmoB)*factmo
factfa~~c(varactfaA,varactfaB)*factfa
factc1~~c(varactc1A,varactc1B)*factc1
factc2~~c(varactc2A,varactc2B)*factc2
fparmo~~c(varparmoA,varparmoB)*fparmo
fparfa~~c(varparfaA,varparfaB)*fparfa
fparc1~~c(varparc1A,varparc1B)*fparc1
fparc2~~c(varparc2A,varparc2B)*fparc2
fmofa~~c(varmofaA,varmofaB)*fmofa
ffamo~~c(varfamoA,varfamoB)*ffamo
fmoc1~~c(varmoc1A,varmoc1B)*fmoc1
fc1mo~~c(varc1moA,varc1moB)*fc1mo
fmoc2~~c(varmoc2A,varmoc2B)*fmoc2
fc2mo~~c(varc2moA,varc2moB)*fc2mo
ffac1~~c(varfac1A,varfac1B)*ffac1
fc1fa~~c(varc1faA,varc1faB)*fc1fa
ffac2~~c(varfac2A,varfac2B)*ffac2
fc2fa~~c(varc2faA,varc2faB)*fc2fa
fc2c1~~c(varc2c1A,varc2c1B)*fc2c1
fc1c2~~c(varc1c2A,varc1c2B)*fc1c2

# reciprocities
factmo~~c(covarmoA,covarmoB)*fparmo
factfa~~c(covarfaA,covarfaB)*fparfa
factc1~~c(covarc1A,covarc1B)*fparc1
factc2~~c(covarc2A,covarc2B)*fparc2
ffamo~~c(covarmofaA,covarmofaB)*fmofa
fc1mo~~c(covarmofaA,covarmofaB)*fmoc1
fc2mo~~c(covarmoc2A,covarmoc2B)*fmoc2
fc1fa~~c(covarfac1A,covarfac1B)*ffac1
fc2fa~~c(covarc2faA,covarc2faB)*ffac2
fc1c2~~c(covarc2c1A,covarc2c1B)*fc2c1

ffam   ~ c(famA,famB)*1
factmo ~ c(actmoA,actmoB)*1
factfa ~ c(actfaA,actfaB)*1
factc1 ~ c(actc1A,actc1B)*1
factc2 ~ c(actc2A,actc2B)*1
fparmo ~ c(parmoA,parmoB)*1
fparfa ~ c(parfaA,parfaB)*1
fparc1 ~ c(parc1A,parc1B)*1
fparc2 ~ c(parc2A,parc2B)*1
fmofa ~ c(mofaA,mofaB)*1
ffamo ~ c(famoA,famoB)*1
fmoc1 ~ c(moc1A,moc1B)*1
fc1mo ~ c(c1moA,c1moB)*1
fmoc2 ~ c(moc2A,moc2B)*1
fc2mo ~ c(c2moA,c2moB)*1
ffac1 ~ c(fac1A,fac1B)*1
fc1fa ~ c(c1faA,c1faB)*1
ffac2 ~ c(fac2A,fac2B)*1
fc2fa ~ c(c2faA,c2faB)*1
fc1c2 ~ c(c1c2A,c1c2B)*1
fc2c1 ~ c(c2c1A,c2c1B)*1

# mean/intercepts observed
MOFAa ~ 0
FAMOa ~ 0
MOC1a ~ 0
C1MOa ~ 0
MOC2a ~ 0
C2MOa ~ 0
FAC1a ~ 0
C1FAa ~ 0
FAC2a ~ 0
C2FAa ~ 0
C1C2a ~ 0
C2C1a ~ 0

MOFAb ~ 0
FAMOb ~ 0
MOC1b ~ 0
C1MOb ~ 0
MOC2b ~ 0
C2MOb ~ 0
FAC1b ~ 0
C1FAb ~ 0
FAC2b ~ 0
C2FAb ~ 0
C1C2b ~ 0
C2C1b ~ 0


# fouttermen laten correleren zoals Cook ook deed:
# -> diegene die hij vergeten was er hier ook uitgelaten
MOFAa ~~ MOC1a
MOFAa ~~ MOC2a
MOFAa ~~ FAMOa
MOFAa ~~ FAC1a
MOFAa ~~ FAC2a
MOFAa ~~ C1MOa
MOFAa ~~ C1FAa
MOFAa ~~ C1C2a
MOFAa ~~ C2MOa
MOFAa ~~ C2FAa
MOFAa ~~ C2C1a

MOC1a ~~ MOC2a
MOC1a ~~ FAMOa
MOC1a ~~ FAC1a
MOC1a ~~ FAC2a
MOC1a ~~ C1MOa
MOC1a ~~ C1FAa
MOC1a ~~ C1C2a
MOC1a ~~ C2MOa
MOC1a ~~ C2FAa
MOC1a ~~ C2C1a

MOC2a ~~ FAMOa
MOC2a ~~ FAC1a
MOC2a ~~ FAC2a
MOC2a ~~ C1MOa
MOC2a ~~ C1FAa
MOC2a ~~ C1C2a
MOC2a ~~ C2MOa
MOC2a ~~ C2FAa
MOC2a ~~ C2C1a

FAMOa ~~ FAC1a
FAMOa ~~ FAC2a
FAMOa ~~ C1MOa
FAMOa ~~ C1FAa
FAMOa ~~ C1C2a
FAMOa ~~ C2MOa
FAMOa ~~ C2FAa
FAMOa ~~ C2C1a

FAC1a ~~ FAC2a
FAC1a ~~ C1MOa
FAC1a ~~ C1FAa
FAC1a ~~ C1C2a
FAC1a ~~ C2MOa
FAC1a ~~ C2FAa
FAC1a ~~ C2C1a


FAC2a ~~ C1MOa
FAC2a ~~ C1FAa
FAC2a ~~ C1C2a
FAC2a ~~ C2MOa
FAC2a ~~ C2FAa
FAC2a ~~ C2C1a 


C1MOa ~~ C1FAa
C1MOa ~~ C1C2a
C1MOa ~~ C2MOa
C1MOa ~~ C2FAa
C1MOa ~~ C2C1a

C1FAa ~~ C1C2a
C1FAa ~~ C2MOa
C1FAa ~~ C2FAa
C1FAa ~~ C2C1a

C1C2a ~~ C2MOa
C1C2a ~~ C2FAa
C1C2a ~~ C2C1a

C2MOa ~~ C2FAa
C2MOa ~~ C2C1a

C2FAa ~~ C2C1a 

MOFAb ~~ MOC1b
MOFAb ~~ MOC2b
MOFAb ~~ FAMOb
MOFAb ~~ FAC1b
MOFAb ~~ FAC2b
MOFAb ~~ C1MOb
MOFAb ~~ C1FAb
MOFAb ~~ C1C2b
MOFAb ~~ C2MOb
MOFAb ~~ C2FAb


MOC1b ~~ MOC2b
MOC1b ~~ FAMOb
MOC1b ~~ FAC1b
MOC1b ~~ FAC2b
MOC1b ~~ C1MOb
MOC1b ~~ C1FAb
MOC1b ~~ C1C2b
MOC1b ~~ C2MOb
MOC1b ~~ C2FAb
MOC1b ~~ C2C1b

MOC2b ~~ FAMOb
MOC2b ~~ FAC1b
MOC2b ~~ FAC2b
MOC2b ~~ C1MOb
MOC2b ~~ C1FAb
MOC2b ~~ C1C2b
MOC2b ~~ C2MOb
MOC2b ~~ C2FAb

FAMOb ~~ FAC1b
FAMOb ~~ FAC2b
FAMOb ~~ C1MOb
FAMOb ~~ C1FAb
FAMOb ~~ C1C2b
FAMOb ~~ C2MOb
FAMOb ~~ C2FAb
FAMOb ~~ C2C1b

FAC1b ~~ FAC2b
FAC1b ~~ C1MOb
FAC1b ~~ C1FAb
FAC1b ~~ C1C2b
FAC1b ~~ C2MOb
FAC1b ~~ C2FAb
FAC1b ~~ C2C1b


FAC2b ~~ C1MOb
FAC2b ~~ C1FAb
FAC2b ~~ C1C2b
FAC2b ~~ C2MOb
FAC2b ~~ C2FAb
FAC2b ~~ C2C1b

C1MOb ~~ C1FAb
C1MOb ~~ C1C2b
C1MOb ~~ C2MOb
C1MOb ~~ C2FAb
C1MOb ~~ C2C1b

C1FAb ~~ C1C2b
C1FAb ~~ C2MOb
C1FAb ~~ C2FAb
C1FAb ~~ C2C1b

C1C2b ~~ C2MOb
C1C2b ~~ C2FAb
C1C2b ~~ C2C1b

C2MOb ~~ C2FAb
C2MOb ~~ C2C1b

C2FAb ~~ C2C1b

# Constraints
actmoA+actfaA+actc1A+actc2A==0
parmoA+parfaA+parc1A+parc2A==0
mofaA+moc1A+moc2A==0
famoA+c1moA+c2moA==0
famoA+fac1A+fac2A==0
mofaA+c1faA+c2faA==0
c1moA+c1faA+c1c2A==0
moc1A+fac1A+c2c1A==0
c2moA+c2faA+c2c1A==0
moc2A+fac2A+c1c2A==0

actmoB+actfaB+actc1B+actc2B==0
parmoB+parfaB+parc1B+parc2B==0
mofaB+moc1B+moc2B==0
famoB+c1moB+c2moB==0
famoB+fac1B+fac2B==0
mofaB+c1faB+c2faB==0
c1moB+c1faB+c1c2B==0
moc1B+fac1B+c2c1B==0
c2moB+c2faB+c2c1B==0
moc2B+fac2B+c1c2B==0

actmoA==actmoB
'
fit3 <- lavaan(SRM2grp_actmo2, data = BranjeSRM,group="group", auto.var=T, mimic="EQS")
summary(fit3, fit.measures=T)
