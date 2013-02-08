

#------------------------------------------------------------
# ----  4 family design, one indicator
#------------------------------------------------------------

## Koeffizienten und model fits passen alle!
## Es fehlen noch die Korrelationen (generalized and dyadic covariances)
## --> wie kommt man von den Kovarianzen zu den Korrelationen?
## Außerdem: lavaan berechnet keine Standardfehler!

library(foreign)
dat <- read.spss("4person.sav", to.data.frame=TRUE)

# convert to long format
dat2 <- melt(dat, id.vars="id2")
dat2$pid <- substr(dat2$variable, 1, 1)
dat2$tid <- substr(dat2$variable, 2, 2)
dat2$v <- substr(dat2$variable, 3, 6)


s1 <- RR.roles(value ~ pid*tid | id2, dat2)
s1 <- RR.roles(value ~ pid*tid | id2, dat2, IGSIM=list(c("m", "f"), c("c", "y")))

formula <- value ~ pid*tid | id2
data <- dat2
#------------------------------------------------------------
# ----  3 family design, two indicator
#------------------------------------------------------------

## Koeffizienten und model fits passen alle!
## Es fehlen noch die Korrelationen (generalized and dyadic covariances)
## --> wie kommt man von den Kovarianzen zu den Korrelationen?

library(foreign)
dat <- read.spss("3person.sav", to.data.frame=TRUE)

# convert to long format
dat$id2 <- 1:nrow(dat)
dat2 <- melt(dat, id.vars="id2")
dat2$pid <- substr(dat2$variable, 1, 1)
dat2$tid <- substr(dat2$variable, 2, 2)
dat2$v <- substr(dat2$variable, 3, 6)
dat3 <- dcast(dat2, id2 + pid + tid ~ v, value.var="value")

formula <- anx1/anx2 ~ pid*tid | id2
data <- dat3
s1 <- RR.roles.latent(anx1/anx2 ~ pid*tid | id2, dat3, fe=FALSE)