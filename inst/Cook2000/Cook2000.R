data(attdep)

# construct a 3-person data set
attdep3 <- attdep[attdep$perceiver.id != "y" & attdep$target.id != "y", ]

## ======================================================================
## Testing sequence: All types of models:
## - single vs. multiple indicators
## - 3 vs. 4 members
## - calculate mean structure or not
## ======================================================================

## No mean structure, standard models
# 3 persons, 1 indicator
f3.1 <- fSRM(dep1 ~ perceiver.id*target.id | family.id, attdep3)
f3.1

# Test: drop something else...
f3.1.d <- fSRM(dep1 ~ perceiver.id*target.id | family.id, attdep3, drop="actor")
f3.1.d

f3.1.d <- fSRM(dep1 ~ perceiver.id*target.id | family.id, attdep3, drop="partner")
f3.1.d

f3.1.d <- fSRM(dep1 ~ perceiver.id*target.id | family.id, attdep3, drop="GR")
f3.1.d


# 3 persons, 2 indicators
f3.2 <- fSRM(dep1/dep2 ~ perceiver.id*target.id | family.id, attdep3)
f3.2

# Test: drop something else...
f3.2.d <- fSRM(dep1/dep2 ~ perceiver.id*target.id | family.id, attdep3, drop="actor")
f3.2.d

f3.2.d <- fSRM(dep1/dep2 ~ perceiver.id*target.id | family.id, attdep3, drop="partner")
f3.2.d

f3.2.d <- fSRM(dep1/dep2 ~ perceiver.id*target.id | family.id, attdep3, drop="GR")
f3.2.d



# 4 persons, 1 indicator
f4.1 <- fSRM(dep1 ~ perceiver.id*target.id | family.id, attdep)
f4.1

# Test: drop something in 4-members
f4.1.d <- fSRM(dep1 ~ perceiver.id*target.id | family.id, attdep, drop="actor")
f4.1.d


# 4 persons, 2 indicators
f4.2 <- fSRM(dep1/dep2 ~ perceiver.id*target.id | family.id, attdep)
f4.2


## Mean structure
# 3 persons, 1 indicator, mean structure: family.idily effect is allowed but automatically constrained to zero
f3.1.m <- fSRM(dep1 ~ perceiver.id*target.id | family.id, attdep3, means=TRUE)
f3.1.m

# Alternative approach: update the existing model with new parameters:
f3.1.m2 <- update(f3.1, means=TRUE)
f3.1.m2

# 3 persons, 2 indicators, mean structure
f3.2.m <- fSRM(dep1/dep2 ~ perceiver.id*target.id | family.id, attdep3, means=TRUE)
f3.2.m

# 4 persons, 1 indicator, mean structure
f4.1.m <- fSRM(dep1 ~ perceiver.id*target.id | family.id, attdep, means=TRUE)
f4.1.m

# 4 persons, 2 indicators, mean structure
f4.2.m <- fSRM(dep1/dep2 ~ perceiver.id*target.id | family.id, attdep, means=TRUE)
f4.2.m


## ======================================================================
## other parameter tests
## ======================================================================

# ... add intragenerational similarity (now results are identical to Cook, 2000)
f4.ig <- fSRM(dep1/dep2 ~ perceiver.id*target.id | family.id, attdep, IGSIM=list(c("m", "f"), c("c", "y")))
f4.ig

# print modification indices
mod(f3.1)
mod(f3.1, minMI=1)

# predict new cases
predict(f4.1, attdep[attdep$family.id==1, ])
predict(f4.1, attdep[attdep$family.id %in% c(1, 2), ])


## ======================================================================
## Wald-test for equality of means
## ======================================================================

f4.1.m <- fSRM(dep1 ~ perceiver.id*target.id | family.id, attdep, means=TRUE)
f4.1.m
equalMeans(f4.1.m)

f3.2.m <- fSRM(dep1/dep2 ~ perceiver.id*target.id | family.id, attdep3, means=TRUE)
f3.2.m
equalMeans(f3.2.m)


## ======================================================================
## deltamethod test
## ======================================================================

# split the data set into two groups
attdep.g <- attdep
attdep.g$group <- ifelse(attdep.g$family.id <= 104, "A", "B")

f4.d <- fSRM(dep1 ~ perceiver.id*target.id | family.id, attdep.g, means=TRUE, group="group", diff=TRUE)
f4.d

f4.d <- fSRM(dep1 ~ perceiver.id*target.id | family.id, attdep.g, means=TRUE, group="group", diff=TRUE, setZero="negative")
f4.d


f4.d2 <- update(f4.d, drop="family")

attdep3.g <- attdep
attdep3.g$group <- ifelse(attdep3.g$family.id <= 104, "A", "B")

f3.d <- fSRM(dep1 ~ perceiver.id*target.id | family.id, attdep3.g, means=TRUE, group="group", diff=TRUE, drop="family")
f3.d
