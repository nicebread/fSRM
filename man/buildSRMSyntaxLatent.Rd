\name{buildSRMSyntaxLatent}
\alias{buildSRMSyntaxLatent}
\title{Build lavaan syntax for a Social Relations Model with roles ("Family SRM")}
\usage{
  buildSRMSyntaxLatent(roles, var.id, self = FALSE,
    IGSIM = list(), fe = TRUE, err = 2, add.variable = c(),
    selfmode = "cor", ...)
}
\arguments{
  \item{roles}{A vector with all role labels.}

  \item{var.id}{A vector with the variable names of the DV
  indicators}

  \item{fe}{Should the family effect be included? Requires
  at least 4 members per group.}

  \item{err}{Defines the type of correlations between error
  terms. err = 1: Correlate same items BETWEEN ALL RATERS
  (e.g., Dyadic Data Analysis, Kenny, Kashy, & Cook, 2000);
  err = 2: Correlate same items WITHIN RATERS (e.g., Branje
  et al., 2003, Eichelsheim)}

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
}
\description{
  Build lavaan syntax for a Social Relations Model with
  roles ("Family SRM"). This function is called by the fSRM
  function, but can be also
}
\details{
  None.
}
\references{
  Kenny, D. A., & West, T. V. (2010). Similarity and
  Agreement in Self-and Other Perception: A Meta-Analysis.
  Personality and Social Psychology Review, 14(2), 196–213.
  doi:10.1177/1088868309353414
}

