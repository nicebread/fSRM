\docType{data}
\name{two.groups}
\alias{two.groups}
\title{Data set on negative interactions
A simulated dataset, mimicking the study performed by Eichelsheim et al. (2011) who investigated whether there are
differences in patterns of negativity between families with and without an adolescent with externalizing problem
behavior. The problematic and nonproblematic group consist of 120 and 153 four-person families, respectively.
This dataset contains two measures of negativity for each of the 12 relationships.
Four roles are present: Mothers "MO", fathers "FA", the asolescent with externalizing problem behavior "C1", and the adolescent without problem behavior "C2".}
\format{A data frame with 3276 rows and 6 variables (273 families with 4 members each, round-robin design)}
\description{
The variables are as follows:
}
\details{
\itemize{ \item family.id An indicator for the family.
\item actor.id An indicator for the perceiver, either "MO",
"FA", "C1", or "C2". \item partner.id An indicator for the
target, either "MO", "FA", "C1", or "C2". \item group An
indicator for the group, 1 represents the problematic
families and 2 the nonproblematic families. \item neg1
First negativity measure. \item neg2 Second negativity
measure. }
}
\examples{
data(two.groups)
head(two.groups)
}
\references{
Eichelsheim, V. I., Buist, K. L., Dekovic, M., Cook, W. L.,
Manders, W., Branje, S. J. T., et al. (2011). Negativity in
problematic and nonproblematic families: A multigroup
social relations model analysis with structured means.
\emph{Journal of Family Psychology}, 25, 152-6. DOI:
10.1037/a0022450.
}
\keyword{datasets}

