# get correlations between latent factors
getCor <- function(x, ops="~~", g="", label="", group=1) {
	suppressWarnings(
		eff <- parameterEstimates(x$fit, standardized=TRUE)
	)
	
	# adjustements for multigroup case: add a group variable with only one group
	if (is.null(eff$group)) eff$group <- 1
	
	eff <- eff[eff$group==group, ]
	
	if (label=="") {
		sel <- eff$op %in% ops & !is.na(eff$est) & !grepl(paste(x$var.id, collapse="|"), eff$rhs)
		if (g != "") {
			sel <- eff$op %in% ops & !is.na(eff$est) & !grepl(paste(x$var.id, collapse="|"), eff$rhs) & (grepl(g, eff$lhs) | grepl(g, eff$rhs))
		}
	} else {
		sel <- grepl(label, eff$label, fixed=TRUE)
		#TODO: include (g != "")? What does it mean?
	}
	SS2 <- eff[sel, ]
	
	# insert label column if missing
	if (is.null(SS2$label)) {
		SS2 <- cbind(SS2[1:3], label="", SS2[, 4:10])
	}
	
	N <- apply(SS2[, 1:3], 1, paste, collapse=" ", sep=" ")	# formula names
	SS3 <- data.frame(component=N, label=SS2$label, round(SS2[, c("est", "se", "z", "pvalue", "ci.lower", "ci.upper", "std.lv")], 3))
	SS3$component <- as.character(SS3$component)
	colnames(SS3) <- c("component", "label", "estimate", "se", "z", "p.value", "ci.lower", "ci.upper", "r")
	
	SS3$r[SS3$r > 1 | SS3$r < -1] <- NA
	return(SS3[, c(1, 2, 3:8, 9)])
}


# retrieve model syntax from fSRM object and copy it directly to the clipboard
# TODO: pbcopy for Windows?
syntax <- function(x){
	cat(x$syntax)
	clipboard <- pipe("pbcopy", open="w")
	write(x$syntax, clipboard)
	close(clipboard)
	invisible(x$syntax)
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



