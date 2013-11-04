sourceDir <- function(path, trace = TRUE, ...) {
   for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
      if(trace) cat(nm,":")           
      source(file.path(path, nm), ...)
      if(trace) cat("\n")
   }
}

sourceDir("../R")
library(lavaan)
library(plyr)
library(reshape2)
library(ggplot2)

# alternatively: load from GitHub
#library(devtools)
#install_github("fSRM", "felice303")
#library(fSRM)