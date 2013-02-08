getCor <- function(x, ops="~~", g="") {
	eff <- parameterEstimates(x$res)
	SS <- standardizedSolution(x$res, type="std.all")
	#sel <- SS$op %in% ops & !is.na(SS$est.std) & SS$est.std != 0 & SS$est.std < .99999 & !grepl(paste(x$var.id, collapse="|"), SS$lhs)
	sel <- SS$op %in% ops & !is.na(SS$est) & SS$est.std != 0 & !grepl(paste(x$var.id, collapse="|"), SS$rhs)
	if (g != "") {
		sel <- SS$op %in% ops & !is.na(SS$est) & SS$est.std != 0 & !grepl(paste(x$var.id, collapse="|"), SS$rhs) & (grepl(g, SS$lhs) | grepl(g, SS$rhs))
	}
	SS2 <- cbind(eff[sel, ], COR=SS[sel, "est.std"])
	
	N <- apply(SS2[, 1:3], 1, paste, collapse=" ", sep=" ")	# formula names
	SS3 <- data.frame(f=N, round(SS2[, -c(1:4)], 3), label=SS2[, 4])
	SS3$f <- as.character(SS3$f)
	return(SS3)
}

model <- function(x){
	cat(x$model)
	clipboard <- pipe("pbcopy", open="w")
	write(x$model, clipboard)
	close(clipboard)
}