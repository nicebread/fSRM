library(lavaan)

# Part 1: the code for simulating data.
  # different means and variances are specified based on the real data of Kenny (4person.sav). This code was used for power calculation
  # (a simular code was used where the Type I error rate was computed. Therfor the means and variances where equal because we wanted to assume that the null hypothesis was correct)
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
fc1c2~~c(-0.01,0.031)*fc2c1;

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




# Part 2: the code to fit the simulated data with this model. New paramaters are specified

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
fam    := famA - famB
actmo := actmoA - actmoB
actfa := actfaA - actfaB
actc1 := actc1A - actc1B
actc2 := actc2A - actc2B
parmo := parmoA - parmoB
parfa := parfaA - parfaB
parc1 := parc1A - parc1B
parc2 := parc2A - parc2B
mofa  := mofaA - mofaB
moc1  := moc1A - moc1B
moc2  := moc2A - moc2B
famo  := famoA - famoB
fac1  := fac1A - fac1B
fac2  := fac2A - fac2B
c1mo  := c1moA - c1moB
c1fa  := c1faA - c1faB
c1c2  := c1c2A - c1c2B
c2mo  := c2moA - c2moB
c2fa  := c2faA - c2faB
c2c1  := c2c1A - c2c1B

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

fam <-c()
actmo <-c()
actfa <- c()
actc1 <- c()
actc2 <- c()
parmo <- c()
parfa <- c() 
parc1 <- c()
parc2 <- c()
mofa <- c()
moc1 <- c()
moc2 <- c()
famo <- c()
fac1 <- c()
fac2 <- c()
c1mo <- c()
c1fa <- c()
c1c2 <- c()
c2mo <- c()
c2fa <- c()
c2c1 <- c()
fit <- c()
sim.data <- matrix(NA, 416, 13)
N <- 200
for(i in 1:N) {
  set.seed(i)
  sim.data <- simulateData(SRM2grp, sample.nobs=c(50,50), group.label=c("group 1", "group 2"))  
  fit <- lavaan(SRM2grpb, data=sim.data,group="group")
  fam <- c(fam, parameterEstimates(fit)[249,]$pvalue)
  actmo  <- c(actmo, parameterEstimates(fit)[250,]$pvalue)
  actfa <- c(actfa, parameterEstimates(fit)[251,]$pvalue)
  actc1 <- c(actc1, parameterEstimates(fit)[252,]$pvalue)
  actc2 <- c(actc2, parameterEstimates(fit)[253,]$pvalue)
  parmo <- c(parmo, parameterEstimates(fit)[254,]$pvalue)
  parfa <- c(parfa, parameterEstimates(fit)[255,]$pvalue)
  parc1 <- c(parc1, parameterEstimates(fit)[256,]$pvalue)
  parc2 <- c(parc2, parameterEstimates(fit)[257,]$pvalue)
  mofa <- c(mofa, parameterEstimates(fit)[258,]$pvalue)
  moc1 <- c(moc1, parameterEstimates(fit)[259,]$pvalue)
  moc2 <- c(moc2, parameterEstimates(fit)[260,]$pvalue)
  famo <- c(famo, parameterEstimates(fit)[261,]$pvalue)
  fac1 <- c(fac1, parameterEstimates(fit)[262,]$pvalue)
  fac2 <- c(fac2, parameterEstimates(fit)[263,]$pvalue)
  c1mo <- c(c1mo, parameterEstimates(fit)[264,]$pvalue)
  c1fa <- c(c1fa, parameterEstimates(fit)[265,]$pvalue)
  c1c2 <- c(c1c2, parameterEstimates(fit)[266,]$pvalue)
  c2mo <- c(c2mo, parameterEstimates(fit)[267,]$pvalue)
  c2fa <- c(c2fa, parameterEstimates(fit)[268,]$pvalue)
  c2c1 <- c(c2c1, parameterEstimates(fit)[269,]$pvalue)
  }

deltamethode<- cbind(fam, actmo, actfa, actc1,actc2,parmo,parfa,parc1,parc2,mofa,moc1,moc2, famo, fac1,fac2,c1mo,c1fa,c1c2,c2mo,c2fa,c2c1)

write.table(deltamethode, "pparam_delta_50sim.csv")