\name{fSRM}
\alias{fSRM}
\title{Run a Social Relations Model with roles ("Family SRM")}
\usage{
fSRM(formula = NULL, data, drop = "default", add = "", means = FALSE,
  pairwise = FALSE, diff = FALSE, IGSIM = list(), add.variable = c(),
  syntax = "", group = NULL, setZero = "none", ...)
}
\arguments{
  \item{formula}{A formula that defines the variable names.
  Should be in one of following formats: (1) Single
  manifest dependent variable: DV ~ actor.id * partner.id |
  group.id, (2) Multiple indicators for dependent variable:
  DV1/DV2/DV3 ~ actor.id * parter.id | group.id.}

  \item{data}{A data frame with all variables defined by
  \code{formula}. Must be in long format where each row
  describes one directed dyadic relationship.}

  \item{drop}{In three-member families at least one
  component has to be dropped. \code{drop} defines which
  one: "none": drop nothing; "family" - drop family effect;
  "GR" - drop generalized reciprocities; "actor" - drop
  actor factors and actor-partner covariances; "partner" -
  drop partner effects and actor-partner covariances;
  "default": drop nothing in >= 4 members and drop family
  effect with 3 members. Although usually not necessary,
  the drop parameter can also be applied to >= 4 member
  families.}

  \item{add}{Additional lavaan syntax pasted at the end of
  the generated model. Can contain, for example, user
  specified error correlations.}

  \item{IGSIM}{Define intragenerational similarity
  correlations. Must be a list where the levels of actor.id
  and partner.id are combined, e.g.:
  \code{IGSIM=list(c("m", "f"), c("c", "y"))}. Here
  "m"other and "f"ather are defined as one generation, and
  "y"ounger and "o"lder as the other generation.}

  \item{syntax}{In that variable the user can directly
  provide a lavaan model syntax. Then no automatical model
  syntax is generated; it is important that the variable
  nakes in the formula}

  \item{add.variable}{Not yet fully implemented: Add
  external variables to the model syntax.}

  \item{...}{Additional arguments passed to the \code{sem}
  function of \code{lavaan}}

  \item{means}{Should the structured means of the SRM
  factors be calculated?}

  \item{pairwise}{Compute pairwise comparison of actor and
  partner means between all roles? Only works when
  \code{means} is also set to TRUE.}

  \item{group}{Variable name indicating group membership}

  \item{diff}{Compare groups with the delta method? You
  need to specify a group identifier in parameter
  \code{group}. If \code{diff = TRUE} and \code{means =
  FALSE}, only variances are compared between groups. If
  \code{diff = TRUE} and \code{means = TRUE}, variances and
  means are compared between groups.}

  \item{setZero}{Should misbehaving variances be set to
  zero? If "negative", all negative variances are
  constrained to zero. If "nonsig", all nonsignificant
  variances are constrained to zero. Please note: The
  purpose of this function is to reproduce published
  results; usually it is *not* recommended to set
  non-significant variances to zero!}
}
\description{
Run a Social Relations Model with roles ("Family SRM")
}
\details{
A model can be rerun with additional syntax using the
\code{add} function: \code{s1 <- fSRM(dep1/dep2 ~
actor*partner | fam, dat2)} \code{s2 <- add(s1, "Ac ~~
Pm")} A model can be run with new parameters using the
update function: \code{s1 <- fSRM(dep1/dep2 ~ actor*partner
| fam, dat2)} \code{s2 <- update(s1, diff=TRUE)}
}
\examples{
\dontrun{
# Example from Dyadic Data Analysis
data(two.indicators)
# 4 persons, 1 indicator
f4.1 <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators)
f4.1

# 4 persons, 2 indicators
f4.2 <- fSRM(dep1/dep2 ~ actor.id*partner.id | family.id, two.indicators)
f4.2

# 4 persons, 1 indicator, mean structure
f4.1.m <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators, means=TRUE)
f4.1.m

# 4 persons, 2 indicators, mean structure
f4.2.m <- fSRM(dep1/dep2 ~ actor.id*partner.id | family.id, two.indicators, means=TRUE)
f4.2.m

# ... add intragenerational similarity (now results are identical to Cook, 2000)
f4.ig <- fSRM(dep1/dep2 ~ actor.id*partner.id | family.id, two.indicators,
              IGSIM=list(c("m", "f"), c("c", "y")))
f4.ig

## ======================================================================
## Wald-test for equality of means
## ======================================================================

f4.1.m <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators, means=TRUE)
f4.1.m
equalMeans(f4.1.m)

f3.2.m <- fSRM(dep1/dep2 ~ actor.id*partner.id | family.id, two.indicators3, means=TRUE)
f3.2.m
equalMeans(f3.2.m)



# ---------------------------------------------------------------------
# Run analyses based on Eichelsheim, V. I., Buist, K. L., Dekovic, M.,
  Cook, W. L., Manders, W., Branje, S. J. T., Frijns, T., et al. (2011).
  Negativity in problematic and nonproblematic families:
  A multigroup social relations model analysis with structured means.
  Journal of Family Psychology, 25, 152-156. doi:10.1037/a0022450

# The data set is a simulated data set which has
# comparable properties as the original data set

data(two.groups)
str(two.groups)

E1 <- fSRM(neg1/neg2 ~ actor.id*partner.id | family.id, data=two.groups)
E1

# make group comparison:
# group = 1: non-problematic families, group = 2: problematic families
E2 <- fSRM(neg1/neg2 ~ actor.id*partner.id | family.id, data=two.groups, group="group")
E2

# Compare means and differences between groups. Beware: This model takes *really* long ...
E3 <- fSRM(neg1/neg2 ~ actor.id*partner.id | family.id,
           data=two.groups, group="group", means=TRUE, diff=TRUE)
E3

}
}
\references{
Kenny, D. A., & West, T. V. (2010). Similarity and
Agreement in Self-and Other Perception: A Meta-Analysis.
Personality and Social Psychology Review, 14(2), 196-213.
doi:10.1177/1088868309353414
}

