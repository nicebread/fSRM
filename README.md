# fSRM: An R package for Social Relations Model with Roles ("family SRM")

The package computes Social Relations Analysis with roles ("Family SRM"), using a structural equation modeling approach. Groups ranging from three members up to an unlimited number of members are supported and the mean structure can be computed. A delta method approach allows the comparison of means and variances between two groups of families, a Wald statistic tests the hypothesis that actor or partner means are equal across roles.

## Installation

The stable version can be installed from [CRAN](http://cran.r-project.org/web/packages/fSRM/index.html):

    install.packages("fSRM")

The current development version can be installed from this repository:

    install.packages(c("devtools", "lavaan", "plyr"), dependencies=TRUE)
    library(devtools)
    install_github("fSRM", username="nicebread", subdir="package")