load(attanx4)

#------------------------------------------------------------
# ----  4 family design, one indicator
#------------------------------------------------------------

s1 <- fSRM(anx ~ perceiver.id*target.id | family.id, attanx4)
s1

s2 <- fSRM(anx ~ perceiver.id*target.id | family.id, attanx4, means=TRUE)
s2
summary(s2$fit)

s3 <- fSRM(anx ~ perceiver.id*target.id | family.id, attanx4[-1,], IGSIM=list(c("m", "f"), c("c", "y")))
s3
# --> identical to DDA, p. ???


#------------------------------------------------------------
# ----  3 family design, two indicator
#------------------------------------------------------------

data(attanx3)
s1 <- fSRM(anx1 ~ perceiver.id*target.id | family.id, attanx3, drop="family")
s1

s2 <- fSRM(anx1 ~ perceiver.id*target.id | family.id, attanx3, drop="actor", means=TRUE)
s2

s3 <- fSRM(anx1/anx2 ~ perceiver.id*target.id | family.id, attanx3, drop="family")
s4  <- fSRM(anx1/anx2 ~ perceiver.id*target.id | family.id, attanx3, drop="family", means=TRUE)
