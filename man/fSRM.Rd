\name{fSRM}
\alias{fSRM}
\alias{predict,}
\alias{update}
\title{Run a Social Relations Model with roles ("Family SRM")}
\usage{
  fSRM(formula = NULL, data, drop = "default", add = "",
    means = FALSE, delta = FALSE, IGSIM = list(),
    add.variable = c(), selfmode = "cor", syntax = "",
    group = NULL, ...)
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
  "reciprocities" - drop individual reciprocities; "actor"
  - drop actor factors and actor-partner covariances;
  "partner" - drop partner effects and actor-partner
  covariances; "default": drop nothing in >= 4 members and
  drop family effect with 3 members. Although usually not
  necessary, the drop parameter can also be applied to >= 4
  member families.}

  \item{add}{Additional lavaan syntax pasted at the end of
  the generated model. Can contain, for example, user
  specified error correlations.}

  \item{IGSIM}{Define intragenerational similarity
  correlations. Must be a list where the levels of actor.id
  and partner.id are combined, e.g.:
  \code{IGSIM=list(c("m", "f"), c("c", "y"))}. Here
  "m"other and "f"ather are defined as one generation, and
  "y"ounger and "o"lder as the other generation.}

  \item{selfmode}{Defines the style how the selfratings are
  combined with the latent actor and partner effects. If
  \code{selfmode="cor"} they are correlated (as in
  REFERENCE), if \code{selfmode="kq"} the k and q paths are
  calculated (see Kenny & West, 2010)}

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

  \item{group}{Variable name indicating group membership}

  \item{delta}{Compare groups with the delta method?}
}
\description{
  Run a Social Relations Model with roles ("Family SRM")
}
\details{
  A model can be rerun with additional syntax using the
  \code{add} function: \code{s1 <- fSRM(dep1/dep2 ~
  actor*partner | fam, dat2)} \code{s2 <- add(s1, "Ac ~~
  Pm")} A model can be run with new parameters using the
  update function: \code{s1 <- fSRM(dep1/dep2 ~
  actor*partner | fam, dat2)} \code{s2 <- update(s1,
  delta=TRUE)}
}
\references{
  Kenny, D. A., & West, T. V. (2010). Similarity and
  Agreement in Self-and Other Perception: A Meta-Analysis.
  Personality and Social Psychology Review, 14(2), 196-213.
  doi:10.1177/1088868309353414
}

