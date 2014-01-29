\docType{data}
\name{two.groups}
\alias{two.groups}
\title{Data set on negative interactions}
\format{A data frame with 3276 rows and 6 variables (208 families with 4 members each, round-robin design)}
\description{
A simulated dataset, mimicking the study performed by Eichelsheim et al. (2011) who investigated whether there are 
differences in patterns of negativity between families with and without an adolescent with externalizing problem 
behavior. The problematic and nonproblematic group consist both of 120 and 153 four-person families, respectively. 
This dataset contains two measures of negativity for each of the 12 relationships.
Four roles are present: Mothers "m", fathers "f", the older child "c", and the younger child "y".
}
\details{
The variables are as follows:
\itemize{ \item family.id An indicator for the family.
\item actor.id An indicator for the perceiver, either "m",
"f", "c", or "y" \item partner.id An indicator for the
target, either "m", "f", "c", or "y" 
\item group An indicator for the group
\item neg1 First negativity measure 
.  \item neg2 Second negativity measure. }
}
\examples{
data(two.groups)
head(two.groups)
}
\references{
Eichelsheim, V. I., Buist, K. L., Dekovic, M., Cook, W. L., Manders, W., Branje, S. J. T.,
et al. (2011). Negativity in problematic and nonproblematic families: a multigroup social
relations model analysis with structured means. Journal of family psychology : JFP :
journal of the Division of Family Psychology of the American Psychological Association
(Division 43), 25 (1), 152–6. DOI: 10.1037/a0022450 .
}
\keyword{datasets}

