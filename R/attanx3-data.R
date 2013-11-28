#' Data set on attachment anxiety in 3-person families (based on Cook, 2000)
#' 
#' A dataset containing ... TODO.
#' Three roles are present: Mothers "m", fathers "f", and the younger child "y".
#'
#' The variables are as follows:
#' 
#' \itemize{
#'   \item family.id An indicator for the family.
#'   \item perceiver.id An indicator for the perceiver, either "m", "f", or "y"
#'   \item target.id An indicator for the target, either "m", "f", or "y"
#'   \item anx1 TODO: Describe anxiety measure.
#'   \item anx2 TODO: Describe anxiety measure.
#' }
#' 
#' @references Cook, W. L. (2000). Understanding attachment security in family context. \emph{Journal of Personality and Social Psychology, 78}, 285–294. doi:10.1037/0022-3514.78.2.285
#' @references http://davidakenny.net/kkc/c9/c9.htm
#' @docType data
#' @keywords datasets
#' @format A data frame with 1248 rows and 5 variables (208 families with 3 members each, round-robin design)
#' @name attanx3

\name{attanx3}
\alias{attanx3}
\docType{data}
\title{
Attachment anxiety in three person families
}
\description{
The classic Cook (2000) dataset consists of measurements on security of attachment within families. 
Only the variable measuring fear of rejection in family relationships is included in this dataset. 
Cook (2000) deduced this variable from the anxiety dimension of the adult attachment scale (Collins & Read, 1990).
The orignial data consisted of four person families (i.e. two parents and two children), but in order to obtain 
a three person family the oldest sibling is systematically deleted in accordance with Kenny, Kashy & Cook (2006). 
The data are presented in the long format.
}
\usage{data(attanx3)}
\format{
  A data frame with 1249 variables (comming from 208 different families) containing four different variables. 
  \describe{
    \item{\code{family.id}}{An indicator for the family.}
    \item{\code{perceiver.id}}{An indicator of the rater in the dyad, either "m", "f", or "y"}
    \item{\code{target.id}}{Identifier of the person being rated in the dyad, either "m", "f", or "y"}
    \item{\code{anx1}}{First indicator of relationship specific anxiety (i.e. average of first halve of the scale)}
    \item{\code{anx1}}{Second indicator of relationship specific anxiety (i.e. average of second halve of the scale)}
    }
  }
\source{
This dataset was retrieved from \url{http://davidakenny.net/kkc/c9/c9.htm} and converted to an R dataset.
}    
\references{
Cook, W. L. (2000). Understanding attachment security in family context. Journal of
Personality and Social Psychology, 78 (2), 285–294.

Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic Data Analysis. United States of America: The Guilford Press.
}

\examples{
head(attanx3)
}
