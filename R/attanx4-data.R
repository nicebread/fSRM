#' Data set on attachment anxiety in families (Cook, 2000)
#' 
#' A dataset containing ... TODO.
#' Four roles are present: Mothers "m", fathers "f", the older child "c", and the younger child "y".
#'
#' The variables are as follows:
#' 
#' \itemize{
#'   \item family.id An indicator for the family.
#'   \item perceiver.id An indicator for the perceiver, either "m", "f", "c", or "y"
#'   \item target.id An indicator for the target, either "m", "f", "c", or "y"
#'   \item anx TODO: Describe anxiety measure.
#' }
#' 
#' @references Cook, W. L. (2000). Understanding attachment security in family context. \emph{Journal of Personality and Social Psychology, 78}, 285–294. doi:10.1037/0022-3514.78.2.285
#' @references http://davidakenny.net/kkc/c9/c9.htm
#' @docType data
#' @keywords datasets
#' @format A data frame with 2496 rows and 4 variables (208 families with 4 members each, round-robin design)
#' @name attanx4

\name{attanx4}
\alias{attanx4}
\docType{data}
\title{
Attachment anxiety in four person families
}
\description{
The classic Cook (2000) dataset consists of measurements on security of attachment within families. 
Only the variable measuring fear of rejection in family relationships is included in this dataset. 
Cook (2000) deduced this variable from the anxiety dimension of the adult attachment scale (Collins & Read, 1990).
The original data consisted of four person families (i.e. two parents and two children): the mother is called "m", 
the father "f", the oldest child "c" and the youngest child "y".
}
\usage{data(attanx3)}
\format{
  A data frame with 2497 rows (coming from 208 different families) containing four different variables. 
  \describe{
    \item{\code{family.id}}{An indicator for the family.}
    \item{\code{perceiver.id}}{An indicator of the rater in the dyad, either "m", "f", "c" or "y"}
    \item{\code{target.id}}{Identifier of the person being rated in the dyad, either "m", "f", "c" or "y"}
    \item{\code{anx1}}{The obtained score on the attachment anxiety scale}
    }
  }
\source{
This dataset was retrieved from \url{http://davidakenny.net/kkc/c9/c9.htm} in wide format and converted to an R dataset in long format.
}    
\references{
Collins, N. L., & Read, S. J. (1990, April). Adult attachment, working models, and
relationship quality in dating couples. Journal of personality and social psychology,
58 (4), 644-663.

Cook, W. L. (2000). Understanding attachment security in family context. Journal of
Personality and Social Psychology, 78 (2), 285–294.
}
\examples{
head(attanx4)
}
