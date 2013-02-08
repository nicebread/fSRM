te2 <- function(x, ...) {
	#print("te2")
	#print(x)
	args <- list(...)
	#print(args[["y"]])
	#print(args[["z"]])
}
te2(1)

te <- function(x, y=2, ...) {
	M <- match.call()
	#print(str(M))
	#do.call(te2, as.list(M))
	M[[1]] <- as.name("te2")
	eval(M)
}
te(1, 3, z=4)

#' @param fe Should the family effect be included?
#' @param model_add Additional lavaan syntax pasted at the end of the generated model
#' @param err Defines the type of correlations between error terms. err = 1: Correlate same items BETWEEN ALL RATERS (e.g., Dyadic Data Analysis, Kenny, Kashy, & Cook, 2000); err = 2: Correlate same items WITHIN RATERS (e.g., Branje et al., 2003, Eichelsheim)
#' @param reestimate 0 = no reestimation; 1 = negative variances are set to zero --> new estimation; 2 = negative and non-significant variances (with p > min.p) are set to zero --> new estimation
#' @param min.p Minimum p value for reestimation: variances with a p value > min.p are set to zero (see also parameter \code{reestimate})
#' @param self Should self-ratings be included in the analysis (if present in the data set)?

fSRM <-
function(formula=NULL, data, fe=TRUE, model_add="", err=1, reestimate=0, min.p=.05, IGSIM=list(), self=FALSE, add.variable=c(), selfmode="cor", model="", REESTIMATE=NULL, ...) {
	library(lavaan)
	
	dots <- list(...)
	
	# save the function call for use in refitting; but only if no refititng is done!
	#if (is.null(dots[["REESTIMATE"]])) {
	call <- match.call(expand.dots = TRUE)
	
	lhs <- strsplit(gsub(" ","",as.character(formula)[2], fixed=TRUE), "/", fixed=TRUE)[[1]]
	rhs <- strsplit(gsub(" ","",as.character(formula)[3], fixed=TRUE),"\\*|\\|", perl=TRUE)[[1]]
	
	var.id <- lhs
	actor.id <- rhs[1]
	partner.id <- rhs[2]
	group.id <- NULL
	if (length(rhs)>=3) {
		group.id <- rhs[3]
	} else {
		stop("For SRM with roles a group id has to be defined in the formula (after the | operator).")
	}
	
	fam0 <- list()
	for (v in c(var.id, add.variable)) {
		fam0[[v]] <- dcast(data[, c(var.id, actor.id, partner.id, group.id, add.variable)], formula(paste(group.id, "~", actor.id, "+", partner.id)), value.var=v)
		colnames(fam0[[v]])[-1] <- paste(colnames(fam0[[v]])[-1], v, sep="_")
	}
	
	fam <- merge.rec(fam0, by=group.id)
	
	# remove all-NA columns
	NAcol <- which(apply(fam, 2, function(x) sum(is.na(x))) == nrow(fam))
	if(length(NAcol)>0)	{fam <- fam[, -NAcol]}
	fam <- na.omit(fam)
	included <- fam[, group.id]
	
	roles <- sort(unique(data[, actor.id]))
	
	# if no model is directly provided:
	if (model == "") {
		model <- buildSRMSyntaxLatent(roles, var.id, fe=fe,err=err, IGSIM=IGSIM, self=self, add.variable=add.variable, selfmode=selfmode)
	
		model2 <- paste(model, model_add, sep="\n")
	} else {
		print("Model syntax is directly specified; skipping buildfSRMSyntax")
		model2 <- model
	}
	
	# reestimation: exchange or add new paths
	if (!is.null(REESTIMATE)) {
		model2 <- paste(model2, "\n## Reestimation adjustments:\n")
		for (r in 1:length(REESTIMATE[[1]])) {
			if (grepl(REESTIMATE[[1]][r], model2)) {
				model2 <- gsub(REESTIMATE[[1]][r], REESTIMATE[[2]][r], model2)
			} else {
				model2 <- paste(model2, REESTIMATE[[2]][r], "\n")
			}
		}
	}
	
	
	#m <- sem(model=model2, data=fam, std.ov=FALSE, orthogonal=TRUE, fixed.x=FALSE, ...)
	m <- lavaan(model=model2, data=fam, std.ov=FALSE, orthogonal=TRUE, fixed.x=FALSE, int.ov.free = TRUE, int.lv.free = FALSE, auto.fix.first = FALSE, auto.fix.single = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE, auto.cov.y = TRUE, ...)
	
	# compute factor scores. FS = wide format, FS3 = long format
	FS <- data.frame(predict(m))
	FS[, group.id] <- included
	
	FS2 <- list()
	for (r in roles) {
		FS2[[r]] <- FS[, grepl(r, colnames(FS))]
		colnames(FS2[[r]]) <- substr(colnames(FS2[[r]]), 1, 1)
		FS2[[r]][, group.id] <- FS[, group.id]
	}
	FS3 <- ldply(FS2, rbind)
	colnames(FS3)[1] <- "role"
	
	
	res <- list(res=m, model=model2, roles=roles, var.id=var.id, fe=fe, IGSIM=IGSIM, self=self, selfmode=selfmode, scores.wide=FS, scores.long=FS3, call=call, min.p=min.p, data=fam)
	
	attr(res, "class") <- "fSRM"
	# --> now the basic model is estimated


	## Do a reestimation of negative or non-significant variance components (VCs)
	if (reestimate > 0) {
		VC <- varComp(x)[, 1:7]
		REP <- rbind(getGR(x)[, 1:7], getDR(x)[, 1:7])
		ALL <- rbind(VC, REP)
		if (reestimate == 1 & any(VC[, "est"] <= 0)) {
			
			VC.sel <- VC[which(T$est <= 0), ]
			
			print(paste("Following variances are < 0 ... reestimating model with these variances set to zero:", paste(VC.sel$f, collapse=", ")))
			
			RE <- list(VC.sel$f, gsub(" ~~ ", " ~~ 0*", VC.sel$f))
			res2 <- add.fSRM(res, RE)
			return(res2)
		}
		if (reestimate == 2 & (any(ALL$pvalue > min.p)  | any(VC$est <= 0))) {
			
			VC.sel <- VC[which(VC$est <= 0 | VC$pvalue > min.p), ]
			REP.sel <- REP[which(REP$pvalue > min.p), ]
			ALL.sel <- c(VC.sel$f, REP.sel$f)
			
			print(paste("Following variances are < 0 or non-significant, or non-significant covariances... reestimating model with these (co)variances set to zero:", paste(VC.sel$f, collapse=", "), paste(REP.sel$f, collapse=", ")))
			RE <- list(ALL.sel, gsub(" ~~ ", " ~~ 0*", ALL.sel))
			res2 <- add.fSRM(res, RE)
			return(res2)
		}
	}
		
	# no reestimation: directly return the result
	return(res)	
}


# rerun a fSRM model with a prespecified model syntax
update.fSRM <- function(x, model, ..., evaluate=TRUE) {
	call <- x$call
    if(is.null(call)) stop("need an fRSM object with call slot")

    extras <- match.call(expand.dots = FALSE)$...

    if(!missing(model)) {
        call$model <- model
		call$REESTIMATE <- NULL
	}

    if(length(extras) > 0) {
        existing <- !is.na(match(names(extras), names(call)))
        for(a in names(extras)[existing]) call[[a]] <- extras[[a]]
        if(any(!existing)) {
            call <- c(as.list(call), extras[!existing])
            call <- as.call(call)
        }
    }
    if (evaluate) {
        eval(call, parent.frame())
    }
    else call
}

# rerun a fSRM model with additional syntax
add.fSRM <- function(x, RE, ..., evaluate=TRUE) {
	call <- x$call
    if(is.null(call)) stop("need an fRSM object with call slot")

    extras <- match.call(expand.dots = FALSE)$...

    if(!missing(RE)) {
        call$REESTIMATE <- RE
		call$reestimate <- 0	# stop reestimation loop
	}

    if(length(extras) > 0) {
        existing <- !is.na(match(names(extras), names(call)))
        for(a in names(extras)[existing]) call[[a]] <- extras[[a]]
        if(any(!existing)) {
            call <- c(as.list(call), extras[!existing])
            call <- as.call(call)
        }
    }
    if (evaluate) {
        eval(call, parent.frame())
    }
    else call
}
