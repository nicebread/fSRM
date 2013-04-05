mod <- function(x, minMI = 10) {
	MI <- modindices(x$fit, standardized=TRUE)
	MI <- MI[order(MI$mi, decreasing=TRUE), ]
	
	# Joereskog: MI > 5 before consideration of respecification
	# Rosseel: MI > 10 before consideration of respecification
	MI2 <- MI[!is.na(MI$mi) & MI$mi>minMI, ]
	if (nrow(MI2)>0) {
		return(MI2)
	} else {
		return(NULL)
	}
}