

SRM2grp <- '
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

'



SRM2grpactmo<- '
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

actmoA==actmoB

'