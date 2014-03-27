.onLoad <- function(libname = find.package("fSRM"), pkgname = "fSRM") {	
	
}

# Define skinning variables in a new environment
#------------------------------------
style <- new.env(parent=globalenv())
style$actor <- "A"
style$partner <- "P"
style$relationship <- "R"
style$familyeffect <- "FE"
style$self <- "S"