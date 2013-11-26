## ======================================================================
## Load Cook 2000 data set; construct three and four member data sets
## ======================================================================


library(foreign)
library(reshape2)
dat0 <- read.spss("Cook2000.sav", to.data.frame=TRUE)

# create an indicator variable for the family
dat0$fam <- 1:nrow(dat0)

# convert to long format
dat <- melt(dat0, id.vars="fam")

# create indicator variables for actor, partner, and measure #
dat$actor <- substr(dat$variable, 1, 1)
dat$partner <- substr(dat$variable, 2, 2)
dat$v <- substr(dat$variable, 3, 6)

# bring both measures back into columns
dat.wide <- dcast(dat, fam + actor + partner ~ v, value.var="value")

## --> now we have the correct data format for the fSRM package:
## each row is one directed relationship with multiple measures in columns

# construct a three-person data set
dat3 <- dat.wide[dat.wide$actor %in% c("c", "f", "m") & dat.wide$partner %in% c("c", "f", "m"), ]

# four-person data set
dat4 <- dat.wide

str(dat4)
colnames(dat4) <- c("family.id", "perceiver.id", "target.id", "dep1", "dep2")
write.table(dat4, file="attdep.tab", row.names=FALSE, col.names=TRUE)