# check for non-ASCII characters
setwd("/Users/Felix/Documents/R/Funktionen/GitHub/fSRM/package")
library(tools)
for (nm in list.files("R/.", pattern="\\.[r|R]", recursive=FALSE)) {
	cat(nm,":")    
   showNonASCIIfile(paste0("R/",nm))
	cat("\n")
}

library(devtools)

setwd("/Users/Felix/Documents/R/Funktionen/GitHub/fSRM/package")
devtools::load_code()

setwd("/Users/Felix/Documents/R/Funktionen/GitHub/fSRM/package")
devtools::document()

setwd("/Users/Felix/Documents/R/Funktionen/GitHub/fSRM/package")
devtools::check()

setwd("/Users/Felix/Documents/R/Funktionen/GitHub/fSRM/package")
devtools::install()

setwd("/Users/Felix/Documents/R/Funktionen/GitHub/fSRM/package")
devtools::build_win()

setwd("/Users/Felix/Documents/R/Funktionen/GitHub/fSRM/package")
devtools::release()

