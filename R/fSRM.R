#' @title Run a Social Relations Model with roles ("Family SRM")
#' @aliases predict, update
#'
#' @description
#' Run a Social Relations Model with roles ("Family SRM")
#'
#' @details
#' A model can be rerun with additional syntax using the \code{add} function:
#' \code{s1 <- fSRM(dep1/dep2 ~ actor*partner | fam, dat2)}
#' \code{s2 <- add(s1, "Ac ~~ Pm")}
#'
#' @export
#' @param formula A formula that defines the variable names. Should be in one of following formats: (1) Single manifest dependent variable: DV ~ actor.id * partner.id | group.id, (2) Multiple indicators for dependent variable: DV1/DV2/DV3 ~ actor.id * parter.id | group.id.
#' @param data A data frame with all variables defined by \code{formula}. Must be in long format where each row describes one directed dyadic relationship.
#' @param fe Should the family effect be included? Requires at least 4 members per group.
#' @param add Additional lavaan syntax pasted at the end of the generated model. Can contain, for example, user specified error correlations.
#' @param err Defines the type of correlations between error terms. err = 1: Correlate same items BETWEEN ALL RATERS (e.g., Dyadic Data Analysis, Kenny, Kashy, & Cook, 2000); err = 2: Correlate same items WITHIN RATERS (e.g., Branje et al., 2003, Eichelsheim)
#' @param IGSIM Define intragenerational similarity correlations. Must be a list where the levels of actor.id and partner.id are combined, e.g.: \code{IGSIM=list(c("m", "f"), c("c", "y"))}. Here "m"other and "f"ather are defined as one generation, and "y"ounger and "o"lder as the other generation.
#' @param self Should self-ratings be included in the analysis (if present in the data set)?
#' @param selfmode Defines the style how the selfratings are combined with the latent actor and partner effects. If \code{selfmode="cor"} they are correlated (as in REFERENCE), if \code{selfmode="kq"} the k and q paths are calculated (see Kenny & West, 2010)
#' @param syntax In that variable the user can directly provide a lavaan model syntax. Then no automatical model syntax is generated; it is important that the variable nakes in the formula
#' @param add.variable Not yet fully implemented: Add external variables to the model syntax.
#' @param ... Additional arguments passed to the \code{sem} function of \code{lavaan}
#' @param means Should the structured means of the SRM factors be calculated?

#' @references
#' Kenny, D. A., & West, T. V. (2010). Similarity and Agreement in Self-and Other Perception: A Meta-Analysis. Personality and Social Psychology Review, 14(2), 196–213. doi:10.1177/1088868309353414

fSRM <-
function(formula=NULL, data, fe=TRUE, add="", err="default", means=FALSE, IGSIM=list(), self=FALSE, add.variable=c(), selfmode="cor", syntax="", ...) {
	
	library(lavaan)
	library(reshape2)
	library(plyr)
	
	dots <- list(...)
	
	# save the function call for use in refitting
	call <- match.call(expand.dots = TRUE)
	
	# Parse the formula
	lhs <- strsplit(gsub(" ","",as.character(formula)[2], fixed=TRUE), "/", fixed=TRUE)[[1]]
	rhs <- strsplit(gsub(" ","",as.character(formula)[3], fixed=TRUE),"\\*|\\|", perl=TRUE)[[1]]
	
	# Retrieve the variable names from the formula
	var.id <- lhs
	actor.id <- rhs[1]
	partner.id <- rhs[2]
	group.id <- NULL
	if (length(rhs)>=3) {
		group.id <- rhs[3]
	} else {
		stop("For SRM with roles a group id has to be defined in the formula (after the | operator).")
	}
	
	# Restructure data format from long to wide
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
	
	# Do some sanity checks
	if (length(roles) == 3 & fe ==TRUE) {warning("Data set with 3-member-groups detected - model probably is not identified. Maybe you should remove the family effect (fe = FALSE)?")}
	if (!identical(sort(unique(data[, actor.id])), sort(unique(data[, partner.id])))) {
		warning("Actor.id and Partner.id have different factor levels; results might be wrong!")
	}
	
	
	# if no syntax is directly provided:
	if (syntax == "") {
		syntax0 <- buildSRMSyntaxLatent(roles, var.id, fe=fe, err=err, IGSIM=IGSIM, means=means, self=self, add.variable=add.variable, selfmode=selfmode)
	
		syntax <- paste(syntax0, add, sep="\n")
	} else {
		print("Model syntax is directly specified; skipping buildfSRMSyntax")
	}
	
	m <- lavaan(
			model		= syntax, 
			data		= fam,
			std.ov		= FALSE,
			orthogonal	= TRUE,
			fixed.x 	= FALSE,
			int.ov.free	= TRUE,
			int.lv.free = FALSE,
			auto.fix.first = FALSE,
			auto.fix.single = TRUE,
			auto.var 	= TRUE,
			auto.cov.lv.x = TRUE,
			auto.cov.y 	= TRUE, ...)
		
	res <- list(
		fit		= m,
		syntax	= syntax,
		roles	= roles,
		actor.id 	= actor.id,
		partner.id 	= partner.id,
		group.id 	= group.id,
		var.id	= var.id,
		fe		= fe,
		IGSIM	= IGSIM,
		self	= self,
		selfmode	= selfmode,
		call	= call,
		min.p	= min.p,
		data	= fam)
	
	attr(res, "class") <- "fSRM"
	return(res)	
}


# rerun a fSRM model with new parameters specified
update.fSRM <- function(x, ..., evaluate=TRUE) {
	call <- x$call
    if(is.null(call)) stop("need an fRSM object with call slot")

    extras <- match.call(expand.dots = FALSE)$...

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



# predicts new cases
# TODO: @Yves: Allow single row data frames, omit unnecessary sanity checks
predict.fSRM <- function(x, newdata, ...) {
	
	# TODO: This snippet is also in fSRM function --> refactor
	# Restructure data format from long to wide
	fam0 <- list()
	for (v in c(x$var.id, x$add.variable)) {
		fam0[[v]] <- dcast(newdata[, c(x$var.id, x$actor.id, x$partner.id, x$group.id, x$add.variable)], formula(paste(x$group.id, "~", x$actor.id, "+", x$partner.id)), value.var=v)
		colnames(fam0[[v]])[-1] <- paste(colnames(fam0[[v]])[-1], v, sep="_")
	}

	fam <- merge.rec(fam0, by=x$group.id)
	
	print(str(x$data))
	print(str(fam))
	predict(x$fit, newdata=fam)
}