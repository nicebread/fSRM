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

## ======================================================================
## Testing sequence: All types of models:
## - single vs. multiple indicators
## - 3 vs. 4 members
## - calculate mean structure or not
## ======================================================================

## No mean structure, standard models
# 3 persons, 1 indicator
f3.1 <- fSRM(dep1 ~ actor*partner | fam, dat3)
f3.1

# Test: drop something else...
f3.1.d <- fSRM(dep1 ~ actor*partner | fam, dat3, drop="actor")
f3.1.d

f3.1.d <- fSRM(dep1 ~ actor*partner | fam, dat3, drop="partner")
f3.1.d

f3.1.d <- fSRM(dep1 ~ actor*partner | fam, dat3, drop="reciprocities")
f3.1.d


# 3 persons, 2 indicators
f3.2 <- fSRM(dep1/dep2 ~ actor*partner | fam, dat3)
f3.2

# Test: drop something else...
f3.2.d <- fSRM(dep1/dep2 ~ actor*partner | fam, dat3, drop="actor")
f3.2.d

f3.2.d <- fSRM(dep1/dep2 ~ actor*partner | fam, dat3, drop="partner")
f3.2.d

f3.2.d <- fSRM(dep1/dep2 ~ actor*partner | fam, dat3, drop="reciprocities")
f3.2.d



# 4 persons, 1 indicator
f4.1 <- fSRM(dep1 ~ actor*partner | fam, dat4)
f4.1

# Test: drop something in 4-members
f4.1.d <- fSRM(dep1 ~ actor*partner | fam, dat4, drop="actor")
f4.1.d


# 4 persons, 2 indicators
f4.2 <- fSRM(dep1/dep2 ~ actor*partner | fam, dat4)
f4.2


## Mean structure
# 3 persons, 1 indicator, mean structure: family effect is allowed but automatically constrained to zero
f3.1.m <- fSRM(dep1 ~ actor*partner | fam, dat3, means=TRUE)
f3.1.m

# Alternative approach: update the existing model with new parameters:
f3.1.m2 <- update(f3.1, means=TRUE)
f3.1.m2

# 3 persons, 2 indicators, mean structure
f3.2.m <- fSRM(dep1/dep2 ~ actor*partner | fam, dat3, means=TRUE)
f3.2.m

# 4 persons, 1 indicator, mean structure
f4.1.m <- fSRM(dep1 ~ actor*partner | fam, dat4, means=TRUE)
f4.1.m

# 4 persons, 2 indicators, mean structure
f4.2.m <- fSRM(dep1/dep2 ~ actor*partner | fam, dat4, means=TRUE)
f4.2.m


## ======================================================================
## other parameter tests
## ======================================================================

# ... add intragenerational similarity (now results are identical to Cook, 2000)
f4.ig <- fSRM(dep1/dep2 ~ actor*partner | fam, dat4, IGSIM=list(c("m", "f"), c("c", "y")))
f4.ig

# print modification indeces
mod(f3.1)

# predict new cases
predict(f4.1, dat4[dat4$fam==1, ])
predict(f4.1, dat4[dat4$fam %in% c(1, 2), ])


## ======================================================================
## Wald-test for equality of means
## ======================================================================

f4.1.m <- fSRM(dep1 ~ actor*partner | fam, dat4, means=TRUE)
f4.1.m
equalMeans(f4.1.m)

f3.2.m <- fSRM(dep1/dep2 ~ actor*partner | fam, dat3, means=TRUE)
f3.2.m
equalMeans(f3.2.m)


## ======================================================================
## deltamethod test
## ======================================================================

# split the data set into two groups
dat.4g <- dat4
dat.4g$group <- ifelse(dat.4g$fam <= 104, "A", "B")

f4.d <- fSRM(dep1 ~ actor*partner | fam, dat.4g, means=TRUE, group="group", delta=TRUE)
f4.d


dat.3g <- dat3
dat.3g$group <- ifelse(dat.3g$fam <= 104, "A", "B")

f3.d <- fSRM(dep1 ~ actor*partner | fam, dat.3g, means=TRUE, group="group", delta=TRUE, drop="family")
f3.d
