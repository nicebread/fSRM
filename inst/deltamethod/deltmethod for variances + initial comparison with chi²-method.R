

################################################# Part 1: simulate a dataset ######################################################
# the simulated data are based on Kenny's book p.240 (4person.sav)
SRM2grp <- '
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

# mean/intercepts latents
ffam~~c(0.053,0.021)*ffam
factmo~~c(0.199,0.125)*factmo;
factfa~~c(0.232,0.187)*factfa; 
factc1~~c(0.149,0.26)*factc1; 
factc2~~c(0.265,0.206)*factc2;
fparmo~~c(0.035,0.058)*fparmo; 
fparfa~~c(0.108,0.016)*fparfa; 
fparc1~~c(0.039,0.087)*fparc1; 
fparc2~~c(0.090,0.064)*fparc2;
fmofa~~c(0.251,0.627)*fmofa;
ffamo~~c(0.643,0.524)*ffamo;
fmoc1~~c(0.182,0.332)*fmoc1;
fc1mo~~c(0.119,0.114)*fc1mo;
fmoc2~~c(0.254,0.403)*fmoc2;
fc2mo~~c(0.178,0.203)*fc2mo;
ffac1~~c(0.16,0.168)*ffac1;
fc1fa~~c(0.173,0.227)*fc1fa;
ffac2~~c(0.112,0.3)*ffac2;
fc2fa~~c(0.181,0.459)*fc2fa;
fc2c1~~c(0.321,0.395)*fc2c1;
fc1c2~~c(0.149,0.272)*fc1c2;
factmo~~c(-0.001,0.067)*fparmo; 
factfa~~c(0,0.011)*fparfa; 
factc1~~c(0.034,0.068)*fparc1;
factc2~~c(0.082,0.063)*fparc2;
ffamo~~c(0.041,0.077)*fmofa;
fc1mo~~c(0.041,0.077)*fmoc1;
fc2mo~~c(0.046,-0.041)*fmoc2;
fc1fa~~c(0.060,0.017)*ffac1;
fc2fa~~c(0.041,0.070)*ffac2;
fc1c2~~c(-0.01,-0.031)*fc2c1;

ffam   ~ c(1.801,1.875)*1 
factmo ~ c(-0.067,-0.107)*1
factfa ~ c(0.067,0.138)*1
factc1 ~ c(-0.167,-0.101)*1
factc2 ~ c(0.166,0.069)*1
fparmo ~ c(-0.141,-0.196)*1
fparfa ~ c(0.049,0.026)*1
fparc1 ~ c(-0.026,0.07)*1
fparc2 ~ c(0.118,0.1)*1
fmofa ~ c(0.009,0.07)*1
ffamo ~ c(0.1,0.132)*1
fmoc1 ~ c(-0.022,-0.033)*1
fc1mo ~ c(-0.048,-0.07)*1
fmoc2 ~ c(0.013,-0.037)*1
fc2mo ~ c(-0.052,-0.062)*1
ffac1 ~ c(-0.028,-0.104)*1
fc1fa ~ c(-0.011,0.005)*1
ffac2 ~ c(-0.071,-0.028)*1
fc2fa ~ c(0.002,-0.075)*1
fc1c2 ~ c(0.059,0.065)*1
fc2c1 ~ c(0.05,0.138)*1

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
sim.data <- simulateData(SRM2grp, sample.nobs=c(500,500), group.label=c("group 1", "group 2"))


################################### PART 2: DELTAMETHOD ################################################################
# Deltamethod for the variances. New parameters for the variances are defined.

SRM2grpb <- '
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

ffam~~c(varfamA,varfamB)*ffam
factmo~~c(varactmoA,varactmoB)*factmo; factfa~~c(varactfaA,varactfaB)*factfa; factc1~~c(varactc1A,varactc1B)*factc1; factc2~~c(varactc2A,varactc2B)*factc2;
fparmo~~c(varparmoA,varparmoB)*fparmo; fparfa~~c(varparfaA,varparfaB)*fparfa; fparc1~~c(varparc1A,varparc1B)*fparc1; fparc2~~c(varparc2A,varparc2B)*fparc2;
fmofa~~c(varmofaA,varmofaB)*fmofa;ffamo~~c(varfamoA,varfamoB)*ffamo;fmoc1~~c(varmoc1A,varmoc1B)*fmoc1;fc1mo~~c(varc1moA,varc1moB)*fc1mo;
fmoc2~~c(varmoc2A,varmoc2B)*fmoc2;fc2mo~~c(varc2moA,varc2moB)*fc2mo;
ffac1~~c(varfac1A,varfac1B)*ffac1;fc1fa~~c(varc1faA,varc1faB)*fc1fa;ffac2~~c(varfac2A,varfac2B)*ffac2;fc2fa~~c(varc2faA,varc2faB)*fc2fa;
fc2c1~~c(varc2c1A,varc2c1B)*fc2c1;fc1c2~~c(varc1c2A,varc1c2B)*fc1c2;
factmo~~c(covarmoA,covarmoB)*fparmo; factfa~~c(covarfaA,covarfaB)*fparfa; factc1~~c(covarc1A,covarc1B)*fparc1; factc2~~c(covarc2A,covarc2B)*fparc2;
ffamo~~c(covarmofaA,covarmofaB)*fmofa;fc1mo~~c(covarmofaA,covarmofaB)*fmoc1;fc2mo~~c(covarmoc2A,covarmoc2B)*fmoc2;fc1fa~~c(covarfac1A,covarfac1B)*ffac1;
fc2fa~~c(covarc2faA,covarc2faB)*ffac2;fc1c2~~c(covarc2c1A,covarc2c1B)*fc2c1;
# varianties worden telkens gemeten door deze 2 (bij version 2 zijn de varianties gelijk voor beide groepen)

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


# defined parameters (function of existing *labeled* parameters)
varfam    := varfamA - varfamB
varactmo := varactmoA - varactmoB
varactfa := varactfaA - varactfaB
varactc1 := varactc1A - varactc1B
varactc2 := varactc2A - varactc2B
varparmo := varparmoA - varparmoB
varparfa := varparfaA - varparfaB
varparc1 := varparc1A - varparc1B
varparc2 := varparc2A - varparc2B
varmofa  := varmofaA - varmofaB
varmoc1  := varmoc1A - varmoc1B
varmoc2  := varmoc2A - varmoc2B
varfamo  := varfamoA - varfamoB
varfac1  := varfac1A - varfac1B
varfac2  := varfac2A - varfac2B
varc1mo  := varc1moA - varc1moB
varc1fa  := varc1faA - varc1faB
varc1c2  := varc1c2A - varc1c2B
varc2mo  := varc2moA - varc2moB
varc2fa  := varc2faA - varc2faB
varc2c1  := varc2c1A - varc2c1B

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
'
fit <- lavaan(SRM2grpb, data=sim.data,group="group")
summary(fit)








#################################### PART 3: COMPARING THIS RESULT WITH THE CHI²-DIFFERENCE TEST #######################################
# this part was added to this script in order to initially compare the deltamethod with this chi²-difference test. 
# (later we used the previous script to run simulations and test this more comprehensivly)
# I just add this part to be comprehensive.

SRM2grp_cons<- '
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

ffam~~c(varfamA,varfamB)*ffam
factmo~~c(varactmoA,varactmoB)*factmo; factfa~~c(varactfaA,varactfaB)*factfa; factc1~~c(varactc1A,varactc1B)*factc1; factc2~~c(varactc2A,varactc2B)*factc2;
fparmo~~c(varparmoA,varparmoB)*fparmo; fparfa~~c(varparfaA,varparfaB)*fparfa; fparc1~~c(varparc1A,varparc1B)*fparc1; fparc2~~c(varparc2A,varparc2B)*fparc2;
fmofa~~c(varmofaA,varmofaB)*fmofa;ffamo~~c(varfamoA,varfamoB)*ffamo;fmoc1~~c(varmoc1A,varmoc1B)*fmoc1;fc1mo~~c(varc1moA,varc1moB)*fc1mo;
fmoc2~~c(varmoc2A,varmoc2B)*fmoc2;fc2mo~~c(varc2moA,varc2moB)*fc2mo;
ffac1~~c(varfac1A,varfac1B)*ffac1;fc1fa~~c(varc1faA,varc1faB)*fc1fa;ffac2~~c(varfac2A,varfac2B)*ffac2;fc2fa~~c(varc2faA,varc2faB)*fc2fa;
fc2c1~~c(varc2c1A,varc2c1B)*fc2c1;fc1c2~~c(varc1c2A,varc1c2B)*fc1c2;
factmo~~c(covarmoA,covarmoB)*fparmo; factfa~~c(covarfaA,covarfaB)*fparfa; factc1~~c(covarc1A,covarc1B)*fparc1; factc2~~c(covarc2A,covarc2B)*fparc2;
ffamo~~c(covarmofaA,covarmofaB)*fmofa;fc1mo~~c(covarmofaA,covarmofaB)*fmoc1;fc2mo~~c(covarmoc2A,covarmoc2B)*fmoc2;fc1fa~~c(covarfac1A,covarfac1B)*ffac1;
fc2fa~~c(covarc2faA,covarc2faB)*ffac2;fc1c2~~c(covarc2c1A,covarc2c1B)*fc2c1;

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

varmoc2A==varmoc2B
'

fit2 <- lavaan(SRM2grp_cons, data=sim.data,auto.var=TRUE,group="group")

anova(fit,fit2)


# chi² difference tests
# varfam:    x² = 5,  p = 0.025            
# varactmo:  x² = 3,   p = 0.08
# varactfa:  x² = 0.06,   p = 0.81
# varactc1:  x² = 16.48,   p <0.001
# varactc2:  x² = 2.5,   p = 0.11
# varparmo:  x² = 0.55,   p = 0.46
# varparfa:  x² = 2.89,   p = 0.09
# varparc1:  x² = 5.82,   p = 0.02
# varparc2:  x² = 0.78,   p = 0.38
# varmofa:   x² = 38.25,   p < 0.001
# varmoc1:   x² = 8.53,   p = 0.003                                                    
# varmoc2:   x² = 5.72,   p = 0.02



summary(fit)
Defined parameters:
  #  varfam            0.052    0.023    2.249    0.025
  #varactmo          0.087    0.049    1.773    0.076
  #varactfa          0.011    0.045    0.246    0.806
  #varactc1         -0.161    0.041   -3.896    0.000
  #varactc2          0.072    0.046    1.558    0.119
  #varparmo         -0.018    0.024   -0.750    0.453
  #varparfa          0.049    0.028    1.723    0.085
  #varparc1         -0.062    0.026   -2.363    0.018
  #varparc2          0.026    0.029    0.895    0.371
  #varmofa          -0.469    0.084   -5.613    0.000
  #varmoc1          -0.154    0.055   -2.783    0.005
#varmoc2          -0.135    0.058   -2.333    0.020
#varfamo           0.074    0.094    0.789    0.430
#varfac1           0.010    0.043    0.228    0.819
#varfac2          -0.122    0.047   -2.597    0.009
#varc1mo           0.001    0.035    0.020    0.984
#varc1fa           0.022    0.047    0.477    0.633
#varc1c2          -0.123    0.046   -2.685    0.007
#varc2mo          -0.001    0.052   -0.014    0.988
#varc2fa          -0.203    0.061   -3.333    0.001
#varc2c1          -0.034    0.071   -0.474    0.635

# ==> IDENTICAL P-VALUES WHERE WITHDRWAN FOR BOTH METHODS
# => this was further investigated with simulation studies. The power and type I error rate were taking into account