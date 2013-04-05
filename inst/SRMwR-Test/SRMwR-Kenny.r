

#------------------------------------------------------------
# ----  4 family design, one indicator
#------------------------------------------------------------

library(foreign)
dat <- read.spss("4person.sav", to.data.frame=TRUE)

# convert to long format
dat2 <- melt(dat, id.vars="id2")
dat2$pid <- substr(dat2$variable, 1, 1)
dat2$tid <- substr(dat2$variable, 2, 2)
dat2$v <- substr(dat2$variable, 3, 6)

s1 <- fSRM(value ~ pid*tid | id2, dat2)
s2 <- fSRM(value ~ pid*tid | id2, dat2, means=TRUE)
summary(s2$fit)

s1 <- fSRM(value ~ pid*tid | id2, dat2[-1,], IGSIM=list(c("m", "f"), c("c", "y")))

# --> identical to DDA

#------------------------------------------------------------
# ----  3 family design, two indicator
#------------------------------------------------------------

library(foreign)
library(reshape2)
dat <- read.spss("3person.sav", to.data.frame=TRUE)

# convert to long format
dat$id2 <- 1:nrow(dat)
dat2 <- melt(dat, id.vars="id2")
dat2$pid <- substr(dat2$variable, 1, 1)
dat2$tid <- substr(dat2$variable, 2, 2)
dat2$v <- substr(dat2$variable, 3, 6)
dat3 <- dcast(dat2, id2 + pid + tid ~ v, value.var="value")

s1 <- fSRM(anx1 ~ pid*tid | id2, dat3, fe=FALSE)
s2 <- fSRM(anx1 ~ pid*tid | id2, dat3, fe=FALSE, means=TRUE)

s3 <- fSRM(anx1/anx2 ~ pid*tid | id2, dat3, fe=FALSE)
s4  <- fSRM(anx1/anx2 ~ pid*tid | id2, dat3, fe=TRUE, means=TRUE)
s4b <- fSRM(anx1/anx2 ~ pid*tid | id2, dat3, model=noc)
s4c <- fSRM(anx1/anx2 ~ pid*tid | id2, dat3, model=noc2)


# --> identical to DDA

s2 <- fSRM(anx1 ~ pid*tid | id2, dat3, fe=FALSE, means=TRUE)