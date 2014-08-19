data(two.indicators)

# construct a 3-person data set
two.indicators3 <- two.indicators[two.indicators$actor.id != "y" & two.indicators$partner.id != "y", ]

## ======================================================================
## Testing sequence: All types of models:
## - single vs. multiple indicators
## - 3 vs. 4 members
## - calculate mean structure or not
## ======================================================================

## No mean structure, standard models
# 3 persons, 1 indicator
f3.1 <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators3)
f3.1
plot(f3.1)
plot(f3.1, bw=TRUE)

f3.1.b <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators3, se="boot")

# Test: drop something else...
f3.1.d <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators3, drop="actor")
f3.1.d

f3.1.d <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators3, drop="partner")
f3.1.d

f3.1.d <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators3, drop="GR")
f3.1.d


# 3 persons, 2 indicators
f3.2 <- fSRM(dep1/dep2 ~ actor.id*partner.id | family.id, two.indicators3)
f3.2
plot(f3.2)
plot(f3.1, onlyStable=TRUE)

# Test: drop something else...
f3.2.d <- fSRM(dep1/dep2 ~ actor.id*partner.id | family.id, two.indicators3, drop="actor")
f3.2.d

f3.2.d <- fSRM(dep1/dep2 ~ actor.id*partner.id | family.id, two.indicators3, drop="partner")
f3.2.d

f3.2.d <- fSRM(dep1/dep2 ~ actor.id*partner.id | family.id, two.indicators3, drop="GR")
f3.2.d



# 4 persons, 1 indicator
f4.1 <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators)
f4.1

f4.1.b <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators, se="boot")
f4.1.b


# Test: drop something in 4-members
f4.1.d <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators, drop="actor")
f4.1.d


# 4 persons, 2 indicators
f4.2 <- fSRM(dep1/dep2 ~ actor.id*partner.id | family.id, two.indicators)
f4.2


## Mean structure
# 3 persons, 1 indicator, mean structure: family effect is allowed but automatically constrained to zero
f3.1.m <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators3, means=TRUE)
f3.1.m

# Alternative approach: update the existing model with new parameters:
f3.1.m2 <- update(f3.1, means=TRUE)
f3.1.m2

# 3 persons, 2 indicators, mean structure
f3.2.m <- fSRM(dep1/dep2 ~ actor.id*partner.id | family.id, two.indicators3, means=TRUE)
f3.2.m

# 4 persons, 1 indicator, mean structure
f4.1.m <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators, means=TRUE)
f4.1.m

f4.1.m.pw <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators, means=TRUE, pairwise=TRUE)
f4.1.m.pw


# 4 persons, 2 indicators, mean structure
f4.2.m <- fSRM(dep1/dep2 ~ actor.id*partner.id | family.id, two.indicators, means=TRUE)
f4.2.m


## ======================================================================
## other parameter tests
## ======================================================================

# ... add intragenerational similarity (now results are identical to Cook, 2000)
f4.ig <- fSRM(dep1/dep2 ~ actor.id*partner.id | family.id, two.indicators, IGSIM=list(c("m", "f"), c("c", "y")))
f4.ig

# print modification indices
mod(f3.1)
mod(f3.1, minMI=1)

# predict new cases
predict(f4.1, two.indicators[two.indicators$family.id==1, ])
predict(f4.1, two.indicators[two.indicators$family.id %in% c(1, 2), ])


## ======================================================================
## Wald-test for equality of means
## ======================================================================

f4.1.m <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators, means=TRUE)
f4.1.m
equalMeans(f4.1.m)

f3.2.m <- fSRM(dep1/dep2 ~ actor.id*partner.id | family.id, two.indicators3, means=TRUE)
f3.2.m
equalMeans(f3.2.m)


## ======================================================================
## deltamethod test
## ======================================================================

# split the data set into two groups
two.indicators.g <- two.indicators
two.indicators.g$group <- ifelse(two.indicators.g$family.id <= 104, "A", "B")

f4.d <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators.g, group="group", means=TRUE)
f4.d

f4.d2 <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators.g, group="group", diff=TRUE)
f4.d2


f4.d3 <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators.g, group="group", means=TRUE, diff=TRUE)
f4.d3


f4.d2 <- update(f4.d, drop="family")

two.indicators3.g <- two.indicators
two.indicators3.g$group <- ifelse(two.indicators3.g$family.id <= 104, "A", "B")

f3.d <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators3.g, means=TRUE, group="group", diff=TRUE, drop="family")
f3.d


f3.d <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators3.g, means=TRUE, group="group", drop="family")
f3.d

f3.d <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators3.g, diff=TRUE, group="group", drop="actor")
f3.d

f3.d <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators3.g, drop="family", group="group")
f3.d


## ======================================================================
## Test missing values
## ======================================================================
data(two.indicators)

# construct a 3-person data set
two.indicators3 <- two.indicators[two.indicators$actor.id != "y" & two.indicators$partner.id != "y", ]

two.indicators3_MCAR <- two.indicators3
# add 50 missings
two.indicators3_MCAR$dep1[sample(1:nrow(two.indicators3_MCAR), 50)] <- NA

# default: you lose families with missing values ...
f3.1.m <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators3_MCAR)
f3.1.m

f3.1.m2 <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators3_MCAR, missing="listwise")
f3.1.m2



## ======================================================================
## test: do roles matter at all?
## ======================================================================

freeroles <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators, means=TRUE)
freeroles

equalroles <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators, means=TRUE, rolesEqual=TRUE)
equalroles

anova(freeroles$fit, equalroles$fit)


data(three.person)
freeroles <- fSRM(anx1 ~ actor.id*partner.id | family.id, data=three.person, drop="actor", means=TRUE)
freeroles

equalroles <- fSRM(anx1 ~ actor.id*partner.id | family.id, data=three.person, drop="actor", rolesEqual=TRUE, means=TRUE)
equalroles
