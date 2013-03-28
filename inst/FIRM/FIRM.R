load("FIRMdemo.RData")
head(RRdat4)

#------------------------------------------------------------
# ----  Base models for target effects (without self-ratings)
#------------------------------------------------------------

# POWER: 2 indicators

pow2 <- fSRM(RR_UMS2_1/RR_UMS2_3 ~ role.p*role.t | gid, RRdat4, reestimate=1, err=1, IGSIM=list(c("Mother", "Father"), c("Older", "Younger")))

mod(pow2)

pow2.add <- "
# manual adjustments based on MI
Older_Younger_RR_UMS2_1 ~~ Older_Father_RR_UMS2_3
Father_Mother_RR_UMS2_1 ~~ Father_Older_RR_UMS2_3
"

pow2b <- fSRM(RR_UMS2_1/RR_UMS2_3 ~ role.p*role.t | gid, RRdat4, reestimate=1, err=1, IGSIM=list(c("Mother", "Father"), c("Older", "Younger")), add=pow2.add)

(pow2c <- add.fSRM(pow2, pow2.add))

anovaList(list(pow2, pow2b, pow2c))


# POWER: 3 indicators

pow3 <- fSRM(RR_UMS1_2/RR_UMS2_1/RR_UMS2_3 ~ role.p*role.t | gid, RRdat4, reestimate=1, err=1, IGSIM=list(c("Mother", "Father"), c("Older", "Younger")))
