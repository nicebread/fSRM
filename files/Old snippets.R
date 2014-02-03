# compute factor scores. FS = wide format, FS3 = long format
FS <- data.frame(predict(m))
FS[, group.id] <- included
	
# TODO: seems buggy
FS2 <- list()
for (r in roles) {
	FS2[[r]] <- FS[, grepl(r, colnames(FS))]
	colnames(FS2[[r]]) <- substr(colnames(FS2[[r]]), 1, 1)
	FS2[[r]][, group.id] <- FS[, group.id]
}
FS3 <- ldply(FS2, rbind)
colnames(FS3)[1] <- "role"


## ======================================================================
## REESTIMATION
## ======================================================================

#' @param reestimate 0 = no reestimation; 1 = negative variances are set to zero --> new estimation; 2 = negative and non-significant variances (with p > min.p) are set to zero --> new estimation
#' @param min.p Minimum p value for reestimation: variances with a p value > min.p are set to zero (see also parameter \code{reestimate})



## Do a reestimation of negative or non-significant variance components (VCs)
if (reestimate > 0) {
	VC <- varComp(res)[, 1:7]
	REP <- rbind(getGR(res)[, 1:7], getDR(res)[, 1:7])
	ALL <- rbind(VC, REP)
	if (reestimate == 1 & any(VC[, "est"] <= 0)) {
			
		VC.sel <- VC[which(VC$est <= 0), ]
			
		print(paste("Following variances are < 0 ... reestimating model with these variances set to zero:", paste(VC.sel$f, collapse=", ")))
			
		RE <- list(VC.sel$f, gsub(" ~~ ", " ~~ 0*", VC.sel$f))
		res2 <- RE.fSRM(res, RE)
		return(res2)
	}
	if (reestimate == 2 & (any(ALL$pvalue > min.p)  | any(VC$est <= 0))) {
			
		VC.sel <- VC[which(VC$est <= 0 | VC$pvalue > min.p), ]
		REP.sel <- REP[which(REP$pvalue > min.p), ]
		ALL.sel <- c(VC.sel$f, REP.sel$f)
			
		print(paste("Following variances are < 0 or non-significant, or non-significant covariances... reestimating model with these (co)variances set to zero:", paste(VC.sel$f, collapse=", "), paste(REP.sel$f, collapse=", ")))
		RE <- list(ALL.sel, gsub(" ~~ ", " ~~ 0*", ALL.sel))
		res2 <- RE.fSRM(res, RE)
		return(res2)
	}
}


## ======================================================================
## 
## ======================================================================



