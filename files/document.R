# check for non-ASCII characters
setwd("/Users/Felix/Documents/R/Funktionen/GitHub/fSRM/package")
library(tools)
for (nm in list.files("R/.", pattern="\\.[r|R]", recursive=FALSE)) {
	cat(nm,":")    
   showNonASCIIfile(paste0("R/",nm))
	cat("\n")
}

install.packages(c("devtools", "lavaan", "plyr", "reshape2", "foreign", "tcltk2"), type="source")
install.packages("roxygen2", type="source")
library(devtools)


devtools::document("package")
devtools::check("package")
devtools::check_win_release("package")
devtools::check_win_devel("package")
devtools::check_rhub("package")

# everything OK? Install locally
devtools::install("package")

# Finally: Release to CRAN, and pray
devtools::release("package")
