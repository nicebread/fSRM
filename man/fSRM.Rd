\name{fSRM}
\alias{add}
\alias{fSRM}
\title{Run a Social Relations Model with roles ("Family SRM")}
\usage{
  fSRM(formula = NULL, data, fe = TRUE, add = "", err = 1,
    reestimate = 0, min.p = 0.05, IGSIM = list(),
    self = FALSE, add.variable = c(), selfmode = "cor",
    model = "", REESTIMATE = NULL, ...)
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

  \item{fe}{Should the family effect be included? Requires
  at least 4 members per group.}

  \item{add}{Additional lavaan syntax pasted at the end of
  the generated model. Can contain, for example, user
  specified error correlations.}

  \item{err}{Defines the type of correlations between error
  terms. err = 1: Correlate same items BETWEEN ALL RATERS
  (e.g., Dyadic Data Analysis, Kenny, Kashy, & Cook, 2000);
  err = 2: Correlate same items WITHIN RATERS (e.g., Branje
  et al., 2003, Eichelsheim)}

  \item{reestimate}{0 = no reestimation; 1 = negative
  variances are set to zero --> new estimation; 2 =
  negative and non-significant variances (with p > min.p)
  are set to zero --> new estimation}

  \item{min.p}{Minimum p value for reestimation: variances
  with a p value > min.p are set to zero (see also
  parameter \code{reestimate})}

  \item{IGSIM}{Define intragenerational similarity
  correlations. Must be a list where the levels of actor.id
  and partner.id are combined, e.g.:
  \code{IGSIM=list(c("m", "f"), c("c", "y"))}. Here
  "m"other and "f"ather are defined as one generation, and
  "y"ounger and "o"lder as the other generation.}

  \item{self}{Should self-ratings be included in the
  analysis (if present in the data set)?}

  \item{selfmode}{Defines the style how the selfratings are
  combined with the latent actor and partner effects. If
  \code{selfmode="cor"} they are correlated (as in
  REFERENCE), if \code{selfmode="kq"} the k and q paths are
  calculated (see Kenny & West, 2010)}

  \item{model}{In that variable the user can directly
  provide a lavaan model syntax. Then no automatical model
  syntax is generated; it is important that the variable
  nakes in the formula}

  \item{REESTIMATE}{Internal parameter, do not use.}

  \item{add.variable}{Not yet fully implemented: Add
  external variables to the model syntax.}

  \item{...}{Additional arguments passed to the \code{sem}
  function of \code{lavaan}}
}
\description{
  Run a Social Relations Model with roles ("Family SRM")
}
\details{
  A model can be rerun with additional syntax using the
  \code{add} function: \code{s1 <- fSRM(dep1/dep2 ~
  actor*partner | fam, dat2)} \code{s2 <- add(s1, "Ac ~~
  Pm")}
}
\references{
  Kenny, D. A., & West, T. V. (2010). Similarity and
  Agreement in Self-and Other Perception: A Meta-Analysis.
  Personality and Social Psychology Review, 14(2), 196–213.
  doi:10.1177/1088868309353414
}

