#' @S3method print fSRM

print.fSRM <-
function(x, digits=3, ...) {
	
	# Print model summary for all groups
	
	## The model for 4 members must have 31 free parameters and 47 df!
	cat("----------------\n")
	cat(paste("SRM with roles (latent) (Roles: ", paste(x$roles, collapse=", "), sep=""), "; DVs = ", x$var.id, ") :\n----------------\n")
	cat("\nModel summary:\n----------------\n")
	show(x$fit)
	cat("\nModel Fit:\n----------------\n")
	FIT <- fitmeasures(x$fit)
	cat(paste("Chi2 (df=", FIT["df"], ") = ", round(FIT["chisq"], digits), ", p = ", round(FIT["pvalue"], digits), "\n", sep=""))
	cat(paste("CFI = ", round(FIT["cfi"], digits), "\n", sep=""))
	cat(paste("TLI / NNFI = ", round(FIT["tli"], digits), "\n", sep=""))
	cat(paste("RMSEA = ", round(FIT["rmsea"], digits), " [", round(FIT["rmsea.ci.lower"], digits), ";", round(FIT["rmsea.ci.upper"], digits), "]", "; Test of close fit: p(data | true value == .05) = ", round(FIT["rmsea.pvalue"], digits), "\n", sep=""))
	
	
	if (is.null(x$group)) {
		print.singlegroup(x, group=1, digits=digits)
	} else {
		# print stats for each group
		for (g in 1:length(x$groupnames)) {
			cat("\n\n#####################################\n")
			cat(paste0("Statistics for group", g, "\n"))
			cat("#####################################\n")
			print.singlegroup(x, group=g, digits=digits)
		}
		cat("\n\n#####################################\n")
		cat(paste0("Difference of means between groups (", x$groupnames[1], "-", x$groupnames[2], ")\n"))
		cat("#####################################\n")
		
		MD <- getCor(x, label=".meanDiff.", group=0)
		colnames(MD)[5] <- "diff"
		print(MD[, c(2, 5, 4, 6:9)])
		
		cat("\n\n#####################################\n")
		cat(paste0("Difference of variances between groups (", x$groupnames[1], "-", x$groupnames[2], ")\n"))
		cat("#####################################\n")
		
		MD <- getCor(x, label=".varDiff.", group=0)
		colnames(MD)[5] <- "diff"
		print(MD[, c(2, 5, 4, 6:9)])
	}
}






print.singlegroup <-
function(x, group=1, digits=3) {
	eff <- as.data.frame(parameterEstimates(x$fit))
	eff$f <- paste(eff$lhs, eff$op, eff$rhs)
	
	# SS = standardized solution: get correlation for that
	SS <- getCor(x, ops=c("~~", "~"), group=group)

	cat("\n\nVariance decomposition:\n----------------\n")
	T <- varComp(x, group=group)
	T[, -1] <- round(T[, -1], digits)
	print(T)
	
	cat("\n\nRelative variance decomposition:\n----------------\n")
	print(round(percTable(x, group=group)$stand))
	

	if (!x$drop %in% c("actor", "partner", "reciprocities")) {
		cat("\n\nGeneralized reciprocity (actor-partner covariances):\n----------------\n")
		GR <- getGR(x, group=group)
		print(GR, row.names=TRUE)
	}
	
	#cat("\n\nDyadic reciprocity (relationship covariances): Mean r =", round(meanNA(GR$COR), digits),"(out of bounds estimates set to NA)\n----------------\n")
	DR <- getDR(x, group=group)
	cat("\n\nDyadic reciprocity (relationship covariances): Mean r =", round(meanNA(DR$r), digits),"(out of bounds estimates set to NA)\n----------------\n")
	print(DR, row.names=TRUE)
	
	if (length(x$IGSIM) > 0) {
		cat("\n\nIntragenerational similarity:\n----------------\n")
		igsim <- SS[grepl("IGSIM", SS$label), ][, -2]
		print(igsim)
	}
	
	if (x$self == TRUE) {
		AS <- data.frame()
		for (t in x$roles) {
			
			if (x$selfmode == "cor") {F <- paste(style$actor, ".", t, " ~~ ", style$self, ".", t, sep="")}
			if (x$selfmode == "kq") {F <- paste(style$self, ".", t, " ~ ", style$actor, ".", t, sep="")}
						
			AS0 <- SS[SS$f == F, ]
			AS0$comment <- ""

			# get Variance of components --> if that is < min.p, correlation is not reliable!
			SD1 <- SS[SS$f == paste(style$partner, ".", t, " ~~ ", style$partner, ".", t, sep=""), ]
			SD2 <- SS[SS$f == paste(style$self, ".", t, " ~~ ", style$self, ".", t, sep=""), ]
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
			if (x$selfmode == "cor") {F <- paste(style$partner, ".", t, " ~~ ", style$self, ".", t, sep="")}
			if (x$selfmode == "kq") {F <- paste(style$self, ".", t, " ~ ", style$partner, ".", t, sep="")}
						
			SO0 <- SS[SS$f == F, ]
			SO0$comment <- ""

			# get Variance of components --> if that is < min.p, correlation is not reliable!
			SD1 <- SS[SS$f == paste(style$partner, ".", t, " ~~ ", style$partner, ".", t, sep=""), ]
			SD2 <- SS[SS$f == paste(style$self, ".", t, " ~~ ", style$self, ".", t, sep=""), ]
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
	
	if (x$means == TRUE) {
		cat("\n\nMean structure\n----------------\n")
		if (is.null(x$group)) {
			MS <- eff[grepl(".means.", eff$label, fixed=TRUE), c(1, 5:10)]
		} else {
			MS <- eff[grepl(paste0(".means", x$groupnames[group], "."), eff$label, fixed=TRUE), c(1, 6:11)]
		}
		colnames(MS) <- c("factor", "estimate", "se", "z", "p.value", "ci.lower", "ci.upper")
		MS[, -1] <- round(MS[, -1], digits)
		rownames(MS) <- NULL
		print(MS)
	}
	
}
