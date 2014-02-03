data(four.person)

#------------------------------------------------------------
# ----  4 family design, one indicator
#------------------------------------------------------------

s1 <- fSRM(anx ~ actor.id*partner.id | family.id, four.person)
s1

s2 <- fSRM(anx ~ actor.id*partner.id | family.id, four.person, means=TRUE)
s2
summary(s2$fit)

s3 <- fSRM(anx ~ actor.id*partner.id | family.id, four.person[-1,], IGSIM=list(c("m", "f"), c("c", "y")))
s3
# --> identical to DDA, p. ???


#------------------------------------------------------------
# ----  3 family design, two indicator
#------------------------------------------------------------

data(three.person)
s1 <- fSRM(anx1 ~ actor.id*partner.id | family.id, three.person, drop="family")
s1

s2 <- fSRM(anx1 ~ actor.id*partner.id | family.id, three.person, drop="actor", means=TRUE)
s2

s3 <- fSRM(anx1/anx2 ~ actor.id*partner.id | family.id, three.person, drop="family")
s4  <- fSRM(anx1/anx2 ~ actor.id*partner.id | family.id, three.person, drop="family", means=TRUE)
