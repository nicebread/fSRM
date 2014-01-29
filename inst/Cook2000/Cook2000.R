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

# Test: drop something in 4-members
f4.1.d <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators, drop="actor")
f4.1.d


# 4 persons, 2 indicators
f4.2 <- fSRM(dep1/dep2 ~ actor.id*partner.id | family.id, two.indicators)
f4.2


## Mean structure
# 3 persons, 1 indicator, mean structure: family.idily effect is allowed but automatically constrained to zero
f3.1.m <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators3, means=TRUE)
f3.1.m





s <- "
### lavaan syntax for family SRM
### ROLES:'c','f','m'
### VARID:'dep1'

# Family effect:
FE =~ 1*c_f_dep1 + 1*c_m_dep1 + 1*f_c_dep1 + 1*f_m_dep1 + 1*m_c_dep1 + 1*m_f_dep1

# Actor effects:
A.c =~ 1*c_f_dep1 + 1*c_m_dep1
A.f =~ 1*f_c_dep1 + 1*f_m_dep1
A.m =~ 1*m_c_dep1 + 1*m_f_dep1

# Partner effects:
P.c =~ 1*f_c_dep1 + 1*m_c_dep1
P.f =~ 1*c_f_dep1 + 1*m_f_dep1
P.m =~ 1*c_m_dep1 + 1*f_m_dep1

# Relationship effects:
R.c.f =~ 1* c_f_dep1
R.c.m =~ 1* c_m_dep1
R.f.c =~ 1* f_c_dep1
R.f.m =~ 1* f_m_dep1
R.m.c =~ 1* m_c_dep1
R.m.f =~ 1* m_f_dep1

# Generalized reciprocity:
A.c ~~ P.c
A.f ~~ P.f
A.m ~~ P.m

# Dyadic reciprocity:
R.c.f ~~ R.f.c
R.c.m ~~ R.m.c
R.f.m ~~ R.m.f

# Dropping factors:
##################
 FE ~~ 0*FE

c_f_dep1 ~~ 0*c_f_dep1
c_m_dep1 ~~ 0*c_m_dep1
f_c_dep1 ~~ 0*f_c_dep1
f_m_dep1 ~~ 0*f_m_dep1
m_c_dep1 ~~ 0*m_c_dep1
m_f_dep1 ~~ 0*m_f_dep1 

## Compute structured means
# Define labels for subsequent constraints
FE ~ .means.FE*1
A.c ~ .means.A.c*1
A.f ~ .means.A.f*1
A.m ~ .means.A.m*1
P.c ~ .means.P.c*1
P.f ~ .means.P.f*1
P.m ~ .means.P.m*1
R.c.f ~ .means.R.c.f*1
R.c.m ~ .means.R.c.m*1
R.f.c ~ .means.R.f.c*1
R.f.m ~ .means.R.f.m*1
R.m.c ~ .means.R.m.c*1
R.m.f ~ .means.R.m.f*1


# set means of observed variables to zero
c_f_dep1 ~ 0
c_m_dep1 ~ 0
f_c_dep1 ~ 0
f_m_dep1 ~ 0
m_c_dep1 ~ 0
m_f_dep1 ~ 0

# set constraints on means for identifiability
.means.A.c + .means.A.f + .means.A.m == 0
.means.P.c + .means.P.f + .means.P.m == 0
.means.R.c.f + .means.R.c.m == 0
.means.R.f.c + .means.R.f.m == 0
.means.R.m.c + .means.R.m.f == 0
.means.R.f.c + .means.R.m.c == 0
.means.R.c.f + .means.R.m.f == 0
.means.R.c.m + .means.R.f.m == 0
"

fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators3, syntax=s)









# Alternative approach: update the existing model with new parameters:
f3.1.m2 <- update(f3.1, means=TRUE)
f3.1.m2

# 3 persons, 2 indicators, mean structure
f3.2.m <- fSRM(dep1/dep2 ~ actor.id*partner.id | family.id, two.indicators3, means=TRUE)
f3.2.m

# 4 persons, 1 indicator, mean structure
f4.1.m <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators, means=TRUE)
f4.1.m

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

f4.d <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators.g, group="group", means=TRUE, diff=TRUE)
f4.d



s <- "
### lavaan syntax for family SRM
### ROLES:'c','f','m','y'
### VARID:'dep1'

# Family effect:
FE =~ 1*c_f_dep1 + 1*c_m_dep1 + 1*c_y_dep1 + 1*f_c_dep1 + 1*f_m_dep1 + 1*f_y_dep1 + 1*m_c_dep1 + 1*m_f_dep1 + 1*m_y_dep1 + 1*y_c_dep1 + 1*y_f_dep1 + 1*y_m_dep1

# Actor effects:
A.c =~ 1*c_f_dep1 + 1*c_m_dep1 + 1*c_y_dep1
A.f =~ 1*f_c_dep1 + 1*f_m_dep1 + 1*f_y_dep1
A.m =~ 1*m_c_dep1 + 1*m_f_dep1 + 1*m_y_dep1
A.y =~ 1*y_c_dep1 + 1*y_f_dep1 + 1*y_m_dep1

# Partner effects:
P.c =~ 1*f_c_dep1 + 1*m_c_dep1 + 1*y_c_dep1
P.f =~ 1*c_f_dep1 + 1*m_f_dep1 + 1*y_f_dep1
P.m =~ 1*c_m_dep1 + 1*f_m_dep1 + 1*y_m_dep1
P.y =~ 1*c_y_dep1 + 1*f_y_dep1 + 1*m_y_dep1

# Relationship effects:
R.c.f =~ 1* c_f_dep1
R.c.m =~ 1* c_m_dep1
R.c.y =~ 1* c_y_dep1
R.f.c =~ 1* f_c_dep1
R.f.m =~ 1* f_m_dep1
R.f.y =~ 1* f_y_dep1
R.m.c =~ 1* m_c_dep1
R.m.f =~ 1* m_f_dep1
R.m.y =~ 1* m_y_dep1
R.y.c =~ 1* y_c_dep1
R.y.f =~ 1* y_f_dep1
R.y.m =~ 1* y_m_dep1

# Generalized reciprocity:
A.c ~~ P.c
A.f ~~ P.f
A.m ~~ P.m
A.y ~~ P.y

# Dyadic reciprocity:
R.c.f ~~ R.f.c
R.c.m ~~ R.m.c
R.c.y ~~ R.y.c
R.f.m ~~ R.m.f
R.f.y ~~ R.y.f
R.m.y ~~ R.y.m


c_f_dep1 ~~ 0*c_f_dep1
c_m_dep1 ~~ 0*c_m_dep1
c_y_dep1 ~~ 0*c_y_dep1
f_c_dep1 ~~ 0*f_c_dep1
f_m_dep1 ~~ 0*f_m_dep1
f_y_dep1 ~~ 0*f_y_dep1
m_c_dep1 ~~ 0*m_c_dep1
m_f_dep1 ~~ 0*m_f_dep1
m_y_dep1 ~~ 0*m_y_dep1
y_c_dep1 ~~ 0*y_c_dep1
y_f_dep1 ~~ 0*y_f_dep1
y_m_dep1 ~~ 0*y_m_dep1 

## Compute structured means
# Define labels for subsequent constraints
 FE ~ .means.FE*1
A.c ~ .means.A.c*1
A.f ~ .means.A.f*1
A.m ~ .means.A.m*1
A.y ~ .means.A.y*1
P.c ~ .means.P.c*1
P.f ~ .means.P.f*1
P.m ~ .means.P.m*1
P.y ~ .means.P.y*1
R.c.f ~ .means.R.c.f*1
R.c.m ~ .means.R.c.m*1
R.c.y ~ .means.R.c.y*1
R.f.c ~ .means.R.f.c*1
R.f.m ~ .means.R.f.m*1
R.f.y ~ .means.R.f.y*1
R.m.c ~ .means.R.m.c*1
R.m.f ~ .means.R.m.f*1
R.m.y ~ .means.R.m.y*1
R.y.c ~ .means.R.y.c*1
R.y.f ~ .means.R.y.f*1
R.y.m ~ .means.R.y.m*1


# set means of observed variables to zero
c_f_dep1~ 0
c_m_dep1~ 0
c_y_dep1~ 0
f_c_dep1~ 0
f_m_dep1~ 0
f_y_dep1~ 0
m_c_dep1~ 0
m_f_dep1~ 0
m_y_dep1~ 0
y_c_dep1~ 0
y_f_dep1~ 0
y_m_dep1~ 0

# set constraints on means for identifiability
# .means.A.c + .means.A.f + .means.A.m + .means.A.y == 0
# .means.P.c + .means.P.f + .means.P.m + .means.P.y == 0
# .means.R.c.f + .means.R.c.m + .means.R.c.y == 0
# .means.R.f.c + .means.R.f.m + .means.R.f.y == 0
# .means.R.m.c + .means.R.m.f + .means.R.m.y == 0
# .means.R.y.c + .means.R.y.f + .means.R.y.m == 0
# .means.R.f.c + .means.R.m.c + .means.R.y.c == 0
# .means.R.c.f + .means.R.m.f + .means.R.y.f == 0
# .means.R.c.m + .means.R.f.m + .means.R.y.m == 0
# .means.R.c.y + .means.R.f.y + .means.R.m.y == 0
"

f4.d <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators.g, means=TRUE, syntax=s)


f4.d <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators.g, means=TRUE, group="group", diff=TRUE)
f4.d

# This does not work yet ...
f4.d <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators.g, means=TRUE, group="group", diff=TRUE, setZero="negative")
f4.d


f4.d2 <- update(f4.d, drop="family")

two.indicators3.g <- two.indicators
two.indicators3.g$group <- ifelse(two.indicators3.g$family.id <= 104, "A", "B")

f3.d <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators3.g, means=TRUE, group="group", diff=TRUE, drop="family")
f3.d
