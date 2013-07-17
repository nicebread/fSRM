#################################################### Read in the datafile + datacleaning ###############################################################
setwd("H:/home/Doctoraat/r-package/dataset Branje")
Branje <- spss.get("H:/home/Doctoraat/r-package/dataset Branje/Branje.sav", use.value.labels=T)
attach(Branje)
BranjeSRM <- cbind(group, nrinegmv1, nrinegmv2, nrinegms1, nrinegms2, nrinegma1, nrinegma2, 
                   nrinegvm1, nrinegvm2, nrinegvs1, nrinegvs2, nrinegva1, nrinegva2, nrinegsm1, nrinegsm2,
                   nrinegsv1, nrinegsv2, nrinegsa1, nrinegsa2, nrinegam1, nrinegam2, nrinegav1, nrinegav2,
                   nrinegas1, nrinegas2)
detach(Branje)
BranjeSRM <- as.data.frame(BranjeSRM)
names(BranjeSRM) <- c('group', 'MOFAa', 'MOFAb', 'MOC1a', 'MOC1b', 'MOC2a', 'MOC2b', 'FAMOa', 'FAMOb','FAC1a','FAC1b','FAC2a', 'FAC2b', 'C1MOa',
                      'C1MOb', 'C1FAa', 'C1FAb', 'C1C2a', 'C1C2b', 'C2MOa' , 'C2MOb','C2FAa','C2FAb','C2C1a','C2C1b')

# Delete missing data like EQS automatically did
BranjeSRM[BranjeSRM==999] = NA
BranjeSRM <- na.omit(BranjeSRM)

Branjegr1 <- subset(BranjeSRM, group==1)
Branjegr2 <- subset(BranjeSRM, group==2)
# group: 1 = above norm, 2 = below norm

attach(Branjegr1)
# detach(Branjegr1)
# attach(Branjegr2)
# detach(Branjegr2)



################ apply the formulas of Cook & Kenny (2004) #######################
ffama <- (MOFAa+MOC1a+FAMOa+FAC1a+C1MOa+C1FAa+MOC2a+FAC2a+C1C2a+C2MOa+C2FAa+C2C1a)/12
rowa_m <- (MOFAa +MOC1a+MOC2a)/3
rowa_f <- (FAMOa + FAC1a+FAC2a)/3
rowa_c1 <- (C1MOa + C1FAa+C1C2a)/3
rowa_c2 <- (C2MOa + C2FAa+C2C1a)/3
cola_m <- (FAMOa + C1MOa + C2MOa)/3
cola_f <- (MOFAa +C1FAa+C2FAa)/3
cola_c1 <- (MOC1a+FAC1a+C2C1a)/3
cola_c2<-(MOC2a+FAC2a+C1C2a)/3
n <- 4
gew1 <-(n-1)^2/(n*(n-2))
gew2 <- (n-1)/(n*(n-2))
gew3 <- (n-1)/(n-2)

factmoa <- (rowa_m*gew1) +(cola_m*gew2) -(ffama*gew3)
factfaa <- (rowa_f*gew1) +(cola_f*gew2) -(ffama*gew3)
factc1a <- (rowa_c1*gew1) +(cola_c1*gew2) -(ffama*gew3)
factc2a <- (rowa_c2*gew1) +(cola_c2*gew2) -(ffama*gew3)
fparmoa <- (cola_m*gew1)+(rowa_m*gew2)-(ffama*gew3)
fparfaa <- (cola_f*gew1)+(rowa_f*gew2)-(ffama*gew3)
fparc1a <- (cola_c1*gew1)+(rowa_c1*gew2)-(ffama*gew3)
fparc2a <- (cola_c2*gew1)+(rowa_c2*gew2)-(ffama*gew3)
fmofaa <- MOFAa - factmoa - fparfaa - ffama
fmoc1a <- MOC1a - factmoa - fparc1a - ffama
fmoc2a <- MOC2a - factmoa - fparc2a - ffama
ffamoa <- FAMOa - factfaa - fparmoa - ffama
ffac1a <- FAC1a - factfaa - fparc1a - ffama
ffac2a <- FAC2a - factfaa - fparc2a - ffama
fc1moa <- C1MOa - factc1a - fparmoa - ffama
fc1faa <- C1FAa - factc1a - fparfaa - ffama
fc1c2a <- C1C2a - factc1a - fparc2a - ffama
fc2moa <- C2MOa - factc2a - fparmoa - ffama
fc2faa <- C2FAa - factc2a - fparfaa - ffama
fc2c1a <- C2C1a - factc2a - fparc1a - ffama


ffamb <- (MOFAb+MOC1b+FAMOb+FAC1b+C1MOb+C1FAb+MOC2b+FAC2b+C1C2b+C2MOb+C2FAb+C2C1b)/12
rowb_m <- (MOFAb +MOC1b+MOC2b)/3
rowb_f <- (FAMOb + FAC1b+FAC2b)/3
rowb_c1 <- (C1MOb + C1FAb+C1C2b)/3
rowb_c2 <- (C2MOb + C2FAb+C2C1b)/3
rowb_m <- (FAMOb + C1MOb + C2MOb)/3
rowb_f <- (MOFAb +C1FAb+C2FAb)/3
rowb_c1 <- (MOC1b+FAC1b+C2C1b)/3
rowb_c2<-(MOC2b+FAC2b+C1C2b)/3


factmob <- (rowb_m*gew1) +(rowb_m*gew2) -(ffamb*gew3)
factfab <- (rowb_f*gew1) +(rowb_f*gew2) -(ffamb*gew3)
factc1b <- (rowb_c1*gew1) +(rowb_c1*gew2) -(ffamb*gew3)
factc2b <- (rowb_c2*gew1) +(rowb_c2*gew2) -(ffamb*gew3)
fparmob <- (rowb_m*gew1)+(rowb_m*gew2)-(ffamb*gew3)
fparfab <- (rowb_f*gew1)+(rowb_f*gew2)-(ffamb*gew3)
fparc1b <- (rowb_c1*gew1)+(rowb_c1*gew2)-(ffamb*gew3)
fparc2b <- (rowb_c2*gew1)+(rowb_c2*gew2)-(ffamb*gew3)
fmofab <- MOFAb - factmob - fparfab - ffamb
fmoc1b <- MOC1b - factmob - fparc1b - ffamb
fmoc2b <- MOC2b - factmob - fparc2b - ffamb
ffamob <- FAMOb - factfab - fparmob - ffamb
ffac1b <- FAC1b - factfab - fparc1b - ffamb
ffac2b <- FAC2b - factfab - fparc2b - ffamb
fc1mob <- C1MOb - factc1b - fparmob - ffamb
fc1fab <- C1FAb - factc1b - fparfab - ffamb
fc1c2b <- C1C2b - factc1b - fparc2b - ffamb
fc2mob <- C2MOb - factc2b - fparmob - ffamb
fc2fab <- C2FAb - factc2b - fparfab - ffamb
fc2c1b <- C2C1b - factc2b - fparc1b - ffamb


fam <- (ffama+ ffamb)/2
actmo <- (factmoa+factmob)/2
actfa <- (factfaa+factfab)/2
actc1 <- (factc1a+factc1b)/2
actc2 <- (factc2a+factc2b)/2
parmo <- (fparmoa+fparmob)/2
parfa <- (fparfaa+fparfab)/2
parc1 <- (fparc1a+fparc1b)/2
parc2 <- (fparc2a+fparc2b)/2
mofa <- (fmofaa+fmofab)/2
moc1 <- (fmoc1a+fmoc1b)/2
moc2 <- (fmoc2a+fmoc2b)/2
famo <- (ffamoa+ffamob)/2
fac1 <- (ffac1a+ffac1b)/2
fac2 <- (ffac2a+ffac2b)/2
c1mo <- (fc1moa+ fc1mob)/2
c1fa <- (fc1faa+fc1fab)/2
c1c2 <- (fc1c2a+fc1c2b)/2
c2mo <- (fc2moa+ fc2mob)/2
c2fa <- (fc2faa+fc2fab)/2
c2c1 <- (fc2c1a+fc2c1b)/2

ffam <- mean(fam)
factmo<-mean(actmo) 
factfa <- mean(actfa)
factc1 <- mean(actc1)
factc2 <- mean(actc2)
fparmo <- mean(parmo)
fparfa <- mean(parfa)
fparc1 <- mean(parc1)
fparc2 <- mean(parc2)
fmofa <- mean(mofa)
fmoc1 <- mean(moc1)
fmoc2 <- mean(moc2)
ffamo <- mean(famo)
ffac1 <- mean(fac1)
ffac2 <- mean(fac2)
fc1mo <- mean(c1mo)
fc1fa <- mean(c1fa)
fc1c2 <- mean(c1c2)
fc2mo <- mean(c2mo)
fc2fa <- mean(c2fa)
fc2c1 <- mean(c2c1)

ffam;factmo;factfa;factc1;factc2;fparmo;fparfa;fparc1;fparc2; fmofa;fmoc1;fmoc2; ffamo;ffac1; ffac2; fc1mo;fc1fa; fc1c2;fc2mo;fc2fa;fc2c1

# result +/- the same as in the paper. Little differences can be due to manual up/off rounding. 







##### After playing with the data there is a method that gives the correct result:
setwd("H:/home/Doctoraat/r-package/dataset Branje")
Branje <- spss.get("H:/home/Doctoraat/r-package/dataset Branje/Branje.sav", use.value.labels=T)
attach(Branje)
BranjeSRM <- cbind(group, nrinegmv1, nrinegmv2, nrinegms1, nrinegms2, nrinegma1, nrinegma2, 
                   nrinegvm1, nrinegvm2, nrinegvs1, nrinegvs2, nrinegva1, nrinegva2, nrinegsm1, nrinegsm2,
                   nrinegsv1, nrinegsv2, nrinegsa1, nrinegsa2, nrinegam1, nrinegam2, nrinegav1, nrinegav2,
                   nrinegas1, nrinegas2)
# group: 1 = above norm, 2 = below norm
detach(Branje)
BranjeSRM <- as.data.frame(BranjeSRM)
names(BranjeSRM) <- c('group', 'MOFAa', 'MOFAb', 'MOC1a', 'MOC1b', 'MOC2a', 'MOC2b', 'FAMOa', 'FAMOb','FAC1a','FAC1b','FAC2a', 'FAC2b', 'C1MOa',
                      'C1MOb', 'C1FAa', 'C1FAb', 'C1C2a', 'C1C2b', 'C2MOa' , 'C2MOb','C2FAa','C2FAb','C2C1a','C2C1b')
detach(BranjeSRM)
BranjeSRM[BranjeSRM==999] = NA # so do not remove the missing values
Branjegr1 <- subset(BranjeSRM, group==1)
attach(Branjegr1)
ffama <- (MOFAa+MOC1a+FAMOa+FAC1a+C1MOa+C1FAa+MOC2a+FAC2a+C1C2a+C2MOa+C2FAa+C2C1a)/12
ffamb <- mean(c(MOFAb,MOC1b,FAMOb,FAC1b,C1MOb,C1FAb,MOC2b,FAC2b,C1C2b,C2MOb,C2FAb,C2C1b), na.rm=T)
(mean(ffama, na.rm=T)+ ffamb)/2