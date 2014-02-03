load("FIRMdemo.RData")
head(RRdat4)

#------------------------------------------------------------
# ----  Base models for target effects (without self-ratings)
#------------------------------------------------------------

# POWER: 1 indicator

pow1 <- fSRM(RR_UMS2_1 ~ role.p*role.t | gid, RRdat4)
pow1 <- fSRM(RR_UMS1_2 ~ role.p*role.t | gid, RRdat4)


n1 <- fSRM(RR_UMS2_7/RR_UMS2_1 ~ role.p*role.t | gid, RRdat4, setZero="negative")
n1 <- fSRM(RR_UMS2_7/RR_UMS2_1 ~ role.p*role.t | gid, RRdat4, setZero="nonsig")

n1 <- fSRM(RR_UMS2_7/RR_UMS2_1 ~ role.p*role.t | gid, RRdat4, setZero="negative", mean=TRUE)



pow2 <- fSRM(RR_UMS2_1 ~ role.p*role.t | gid, RRdat4, IGSIM=list(c("Mother", "Father"), c("Older", "Younger")), means=TRUE)

pow1$fit
pow2$fit
summary(pow2$fit)

# POWER: include self rating
pow1 <- fSRM(RR_UMS2_1 ~ role.p*role.t | gid, RRdat4, IGSIM=list(c("Mother", "Father"), c("Older", "Younger")), self=TRUE)

# Dominance
dom <- fSRM(RR_dom ~ role.p*role.t | gid, RRdat4, IGSIM=list(c("Mother", "Father"), c("Older", "Younger")), means=TRUE)
summary(dom$fit)

emo <- fSRM(RR_emoSupp1 ~ role.p*role.t | gid, RRdat4, IGSIM=list(c("Mother", "Father"), c("Older", "Younger")), means=TRUE)
summary(emo$fit)

# POWER: 2 indicators

pow1 <- fSRM(RR_UMS2_1/RR_UMS2_3 ~ role.p*role.t | gid, RRdat4, err="all", IGSIM=list(c("Mother", "Father"), c("Older", "Younger")))

pow2 <- fSRM(RR_UMS2_1/RR_UMS2_3 ~ role.p*role.t | gid, RRdat4, err="all", IGSIM=list(c("Mother", "Father"), c("Older", "Younger")), means=TRUE)

pow1$fit
pow2$fit

summary(pow2$fit)


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

pow3 <- fSRM(RR_UMS1_2/RR_UMS2_1 ~ role.p*role.t | gid, RRdat4, IGSIM=list(c("Mother", "Father"), c("Older", "Younger")))

pow3r <- fSRM(RR_UMS1_2/RR_UMS2_1 ~ role.p*role.t | gid, RRdat4, reestimate=1, err=1, IGSIM=list(c("Mother", "Father"), c("Older", "Younger")))

m <- pow3r$syntax
pow3r3 <- fSRM(RR_UMS1_2/RR_UMS2_1 ~ role.p*role.t | gid, RRdat4, reestimate=0, err=1, IGSIM=list(c("Mother", "Father"), c("Older", "Younger")), model=pow3r.model)

# self included

pow2b <- fSRM(RR_UMS2_1/RR_UMS2_3 ~ role.p*role.t | gid, RRdat4, reestimate=1, err=1, IGSIM=list(c("Mother", "Father"), c("Older", "Younger")), self=TRUE)