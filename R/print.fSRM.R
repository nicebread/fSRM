# Helper: Transform correlation to Fisher's Z
r2Z <- function(r) {
	return(0.5 * log((1 + r)/(1 - r)))
}

# Helper: REcode  Fisher's to correlation
Z2r <- function(Z) {
	return((exp(2*Z)-1)/(exp(2*Z)+1))
}

meanNA <- function(x) {
	x[is.na(x)] <- NA
	x[x>1] <- NA
	x[x<(-1)] <- NA
	return(Z2r(mean(r2Z(x), na.rm=TRUE)))
}

print.fSRM <-
function(x, digits=3, ...) {
	library(plyr)
	## Das Modell muss bei 4 Personen: 31 freie parameter und 47 df haben!
	cat("----------------\n")
	cat(paste("SRM with roles (latent) (Roles: ", paste(x$roles, collapse=", "), sep=""), "; DVs = ", x$var.id, ") :\n----------------\n")
	cat("\nModel summary:\n----------------\n")
	show(x$res)
	cat("\nModel Fit:\n----------------\n")
	FIT <- fitmeasures(x$res)
	cat(paste("Chi2 (df=", FIT["df"], ") = ", round(FIT["chisq"], digits), ", p = ", round(FIT["pvalue"], digits), "\n", sep=""))
	cat(paste("CFI = ", round(FIT["cfi"], digits), "\n", sep=""))
	cat(paste("TLI / NNFI = ", round(FIT["tli"], digits), "\n", sep=""))
	cat(paste("RMSEA = ", round(FIT["rmsea"], digits), " [", round(FIT["rmsea.ci.lower"], digits), ";", round(FIT["rmsea.ci.upper"], digits), "]", "; Test of close fit: p(data | true value == .05) = ", round(FIT["rmsea.pvalue"], digits), "\n", sep=""))
	
	
	eff <- as.data.frame(parameterEstimates(x$res))
	eff$f <- paste(eff$lhs, eff$op, eff$rhs)
	
	# SS = standardized solution: get correlation for that
	SS <- getCor(x, ops=c("~~", "~"))

	cat("\n\nVariance decomposition:\n----------------\n")
	T <- varComp(x)
	T[, -1] <- round(T[, -1], digits)
	print(T)
	
	cat("\n\nRelative variance decomposition:\n----------------\n")
	print(round(percTable.latent(x)$stand))
	

	#cat("\n\nGeneralized reciprocity (actor-partner covariances):\n----------------\n")
	GR <- getGR(x)
	cat(paste("\n\nGeneralized reciprocity (actor-partner covariances): Mean r =", round(meanNA(GR$COR), digits),"(out of bounds estimates set to NA)\n----------------\n"))
	print(GR, row.names=TRUE)
	
	
	
	#cat("\n\nDyadic reciprocity (relationship covariances): Mean r =", round(meanNA(GR$COR), digits),"(out of bounds estimates set to NA)\n----------------\n")
	DR <- getDR(x)
	cat("\n\nDyadic reciprocity (relationship covariances): Mean r =", round(meanNA(DR$COR), digits),"(out of bounds estimates set to NA)\n----------------\n")
	print(DR, row.names=TRUE)
	
	if (length(x$IGSIM) > 0) {
		cat("\n\nIntragenerational similarity:\n----------------\n")
		igsim <- SS[grepl("IGSIM", SS$label), ]
		
		print(igsim)
	}
	
	if (x$self == TRUE) {
		AS <- data.frame()
		for (t in x$roles) {
			
			if (x$selfmode == "cor") {F <- paste("A", t, " ~~ ", "S", t, sep="")}
			if (x$selfmode == "kq") {F <- paste("S", t, " ~ ", "A", t, sep="")}
						
			AS0 <- SS[SS$f == F, ]
			AS0$comment <- ""

			# get Variance of components --> if that is < min.p, correlation is not reliable!
			SD1 <- SS[SS$f == paste("P", t, " ~~ ", "P", t, sep=""), ]
			SD2 <- SS[SS$f == paste("S", t, " ~~ ", "S", t, sep=""), ]
			if (is.na(SD1$pvalue)) SD1$pvalue <- 1
			if (is.na(SD2$pvalue)) SD2$pvalue <- 1

			if (SD1["pvalue"] > x$min.p | SD2["pvalue"] > x$min.p) {
				AS0$COR <- NA_real_
				AS0$comment <- paste("One of the variance components has p <", x$min.p)
			}
			
			if (AS0$pvalue > x$min.p) {
				AS0$COR <- NA_real_
				AS0$comment <- paste("Covariance estimate has p <", x$min.p)
			}

			AS <- rbind(AS, AS0)
		}
		cat("\n\nAssumed similarity: Mean r =", round(meanNA(AS$COR), digits),"(out of bound estimates set to zero)\n----------------\n")
		print(AS,row.names=TRUE)
		
		SO <- data.frame()
		for (t in x$roles) {
			if (x$selfmode == "cor") {F <- paste("P", t, " ~~ ", "S", t, sep="")}
			if (x$selfmode == "kq") {F <- paste("S", t, " ~ ", "P", t, sep="")}
						
			SO0 <- SS[SS$f == F, ]
			SO0$comment <- ""

			# get Variance of components --> if that is < min.p, correlation is not reliable!
			SD1 <- SS[SS$f == paste("P", t, " ~~ ", "P", t, sep=""), ]
			SD2 <- SS[SS$f == paste("S", t, " ~~ ", "S", t, sep=""), ]
			if (is.na(SD1$pvalue)) SD1$pvalue <- 1
			if (is.na(SD2$pvalue)) SD2$pvalue <- 1

			if (SD1["pvalue"] > x$min.p | SD2["pvalue"] > x$min.p) {
				SO0$COR <- NA_real_
				SO0$comment <- paste("One of the variance components has p <", x$min.p)
			}
			
			if (SO0$pvalue > x$min.p) {
				SO0$COR <- NA_real_
				SO0$comment <- paste("Covariance estimate has p <", x$min.p)
			}

			SO <- rbind(SO, SO0)
		}
		SO$COR <- as.numeric(SO$COR)
		cat("\n\nSelf-Other agreement: Mean r =", round(meanNA(SO$COR), digits),"(out of bound estimates set to NA)\n----------------\n")
		print(SO, row.names=TRUE)
	}
}
