mod <- function(x) {
	MI <- modindices(x$res, standardized=TRUE)
	MI <- MI[order(MI$mi, decreasing=TRUE), ]
	
	# Joereskog: MI > 5 before consideration of respecification
	MI2 <- MI[MI$mi>5, ]
	MI2[order(MI2$lhs), ]
}