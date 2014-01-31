\name{import}
\alias{import}
\title{Easily inserting and transforming a dataset in fSRM}
\usage{
import()
}
\description{
This GUI can be used for inserting csv, txt and sav-files
in R by means of a pop-up window. Next, it it allows the
user to easily transform a dataset into the required format
(long format), since SRM data are typically organized in a
wide format. Finally, the user can specify the desired
labels that will be used in the output.
}
\details{
The upper section of the main window allows to insert csv,
txt and sav-files in R. It is required that the first row
in the datafile contains the variable names. A new pop-up
window appears after clicking on 'transform my data' in the
middle section. This new window allows to transform a
dataset from wide format (i.e. each column representing one
measured relationship, and one row for each family) to the
necessary long format. Specify - if present - in this new
window first the variable names which identifies the
different families and groups, respectively. Next, specify
the columns in the original dataset which contain the
dyadic measurements. Enumerated columns need to be
separated with a comma,consecutive columns are defined by
means of colon (e.g. 1 : 9 defines the first until the
second column in the original dataset). Finally, define the
position of the characters of the rater, the person being
rated and the indicator (if present) in the previously
selected dyadic measurements. At the bottom section of the
main window the user can agree with the default labels of
the SRM components by clicking on 'Confirm output format'
or specify alternative labels.
}
\examples{
# The result is saved in a variable called \\code{SRMData}, this is the dataset in long format. The original dataset is saved as \\code{MyData}, which is in wide format.
import()
}

