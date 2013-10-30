# get correlations between latent factors
getCor <- function(x, ops="~~", g="") {
	eff <- parameterEstimates(x$fit)
	SS <- x$SS
	#sel <- SS$op %in% ops & !is.na(SS$est.std) & SS$est.std != 0 & SS$est.std < .99999 & !grepl(paste(x$var.id, collapse="|"), SS$lhs)
	sel <- SS$op %in% ops & !is.na(SS$est) & !grepl(paste(x$var.id, collapse="|"), SS$rhs)
	if (g != "") {
		sel <- SS$op %in% ops & !is.na(SS$est) & !grepl(paste(x$var.id, collapse="|"), SS$rhs) & (grepl(g, SS$lhs) | grepl(g, SS$rhs))
	}
	SS2 <- cbind(eff[sel, ], r=SS[sel, "est.std"])
	if (is.null(SS2$label)) {
		SS2 <- cbind(SS2[1:3], label="", SS2[, 4:10])
	}
	
	N <- apply(SS2[, 1:3], 1, paste, collapse=" ", sep=" ")	# formula names
	SS3 <- data.frame(component=N, label=SS2$label, round(SS2[, -c(1:4)], 3))
	SS3$component <- as.character(SS3$component)
	colnames(SS3) <- c("component", "label", "estimate", "se", "z", "p.value", "ci.lower", "ci.upper", "r")
	return(SS3[, c(1, 2, 9, 6, 3:5, 7:8)])
}


# retrieve model syntax from fSRM object and copy it directly to the clipboard
# TODO: pbcopy for Windows?
model <- function(x){
	cat(x$syntax)
	clipboard <- pipe("pbcopy", open="w")
	write(x$syntax, clipboard)
	close(clipboard)
}



# Transform correlation to Fisher's Z
r2Z <- function(r) {return(0.5 * log((1 + r)/(1 - r)))}

# Recode  Fisher's Z to correlation
Z2r <- function(Z) {return((exp(2*Z)-1)/(exp(2*Z)+1))}

# calculate average correlation for all elemts of x which are within [-1;1].
# I.e., out-of-bound estimates are excluded.
meanNA <- function(x) {
	x[is.na(x)] <- NA
	x[x>1] <- NA
	x[x<(-1)] <- NA
	return(Z2r(mean(r2Z(x), na.rm=TRUE)))
}



