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
dat2 <- dcast(dat, fam + actor + partner ~ v, value.var="value")

## --> now we have the correct data format for the fSRM package:
## each row is one directed relationship with multiple measures in columns
head(dat2)

# run SRM with roles
s1 <- fSRM(dep1/dep2 ~ actor*partner | fam, dat2)

# show the results:
s1

# take a look at the lavaan output
summary(s1$res)

# show the model syntax:
cat(s1$model)

# ... add intragenerational similarity (now results are identical to Cook, 2000)
s2 <- fSRM(dep1/dep2 ~ actor*partner | fam, dat2, IGSIM=list(c("m", "f"), c("c", "y")))
s2

# reestimate the model: force non-significant (co)variances to be zero
s2b <- fSRM(dep1/dep2 ~ actor*partner | fam, dat2, IGSIM=list(c("m", "f"), c("c", "y")), reestimate=2)
s2b


# compare modelfits with and without IGSIM
anovaList(list(s1=s1, s2=s2))

# compare modelfits with and without reestimation
anovaList(list(s2=s2, s2b=s2b))


# change the method correlations to the style of Eichelsheim et al. 2009 (only correlate error terms of one measure *within one rater*)
s3 <- fSRM(dep1/dep2 ~ actor*partner | fam, dat2, IGSIM=list(c("m", "f"), c("c", "y")), err=2)
s3

anovaList(list(s1=s1, s2=s2, s3=s3))

# --> you get better CFI, TLI, Chi2, AIC and BIC. The less restricted model s3 is NOT significantly worse than s2, so s3 would be preferable.
