mod <- function(x) {
	MI <- modindices(x$res, standardized=TRUE)
	MI <- MI[order(MI$mi, decreasing=TRUE), ]
	
	# Jöreskog: MI > 5 before consideration of respecification
	MI[MI$mi>5, ]
}

mod2 <- function(x) {
	MI <- modindices(x$res, standardized=TRUE)
	MI <- MI[order(MI$mi, decreasing=TRUE), ]
	
	# Jöreskog: MI > 5 before consideration of respecification
	MI2 <- MI[MI$mi>5, ]
	MI2[order(MI2$lhs), ]
}