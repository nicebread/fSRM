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


setwd("/Users/Felix/Documents/R/Funktionen/GitHub/fSRM/package")
devtools::document()

setwd("/Users/Felix/Documents/R/Funktionen/GitHub/fSRM/package")
devtools::check()

setwd("/Users/Felix/Documents/R/Funktionen/GitHub/fSRM/package")
devtools::install()

setwd("/Users/Felix/Documents/R/Funktionen/GitHub/fSRM/package")
devtools::build_win()

setwd("/Users/Felix/Documents/R/Funktionen/GitHub/fSRM/package")
devtools::build()

#setwd("/Users/Felix/Documents/R/Funktionen/GitHub/fSRM/package")
#devtools::release()

