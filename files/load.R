sourceDir <- function(path, trace = TRUE, ...) {
   for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
      if(trace) cat(nm,":")           
      source(file.path(path, nm), ...)
      if(trace) cat("\n")
   }
}

sourceDir("../R")

# alternatively: load from GitHub
library(devtools)
install_github("fSRM", "felice303")
library(fSRM)