\name{buildSRMSyntaxLatent}
\alias{buildSRMSyntaxLatent}
\title{Build lavaan syntax for a Social Relations Model with roles ("Family SRM")}
\usage{
buildSRMSyntaxLatent(roles, var.id, self = FALSE, IGSIM = list(),
  drop = "default", err = "default", means = FALSE, diff = FALSE,
  pairwise = FALSE, groupnames = NULL, add.variable = c(),
  selfmode = "cor", ...)
}
\arguments{
  \item{roles}{A vector with all role labels.}

  \item{var.id}{A vector with the variable names of the DV
  indicators}

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

  \item{err}{Defines the type of correlations between error
  terms. err = "no": no error term correlations - this is
  the required mode for single indicators. err = "all": If
  multiple indicators are present, correlate same items
  BETWEEN raters (e.g., Dyadic Data Analysis, Kenny, Kashy,
  & Cook, 2000). err = "default": Set err to "no" for
  single indicators and to "all" for multiple indicators.}

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

  \item{add.variable}{Not yet fully implemented: Add
  external variables to the model syntax.}

  \item{...}{Additional arguments (not documented yet)}

  \item{means}{Should the structured means of the SRM
  factors be calculated?}

  \item{diff}{Compare groups with the delta method?}

  \item{pairwise}{Compute pairwise comparison of actor and
  partner means between all roles? Only works when
  \code{means} is also set to TRUE}

  \item{groupnames}{Vector with the names of the groups
  (i.e., the values of the group column in the data set)}
}
\description{
Build lavaan syntax for a Social Relations Model with roles
("Family SRM"). This function is called by the fSRM
function, but can be also
}
\details{
None.
}
\references{
Kenny, D. A., & West, T. V. (2010). Similarity and
Agreement in Self-and Other Perception: A Meta-Analysis.
Personality and Social Psychology Review, 14(2), 196-213.
doi:10.1177/1088868309353414
}

