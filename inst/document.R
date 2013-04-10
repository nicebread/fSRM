# check for non-ASCII characters
setwd("/Users/Felix/Documents/R/Funktionen/GitHub/fSRM")
library(tools)
for (nm in list.files("R/.", pattern="\\.[r|R]", recursive=FALSE)) {
	cat(nm,":")    
   showNonASCIIfile(paste0("R/",nm))
	cat("\n")
}

library(devtools)

setwd("/Users/Felix/Documents/R/Funktionen/GitHub/fSRM")
devtools::document()

setwd("/Users/Felix/Documents/R/Funktionen/GitHub/fSRM")
devtools::check()

setwd("/Users/Felix/Documents/R/Funktionen/GitHub/fSRM")
devtools::install()

setwd("/Users/Felix/Documents/R/Funktionen/GitHub/fSRM")
devtools::build_win()

setwd("/Users/Felix/Documents/R/Funktionen/GitHub/fSRM")
devtools::release()

