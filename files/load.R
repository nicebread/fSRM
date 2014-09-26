sourceDir <- function(path, trace = TRUE, ...) {
   for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
      if(trace) cat(nm,":")           
      source(file.path(path, nm), ...)
      if(trace) cat("\n")
   }
}

sourceDir("../package/R")
library(lavaan)
library(plyr)
library(reshape2)
library(ggplot2)
library(shape)
library(grid)
library(scales)
library(gridExtra)

# alternatively: load from GitHub
#library(devtools)
#install_github("fSRM", "felice303")
#library(fSRM)