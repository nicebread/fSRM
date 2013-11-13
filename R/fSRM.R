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
#' A model can be run with new parameters using the update function:
#' \code{s1 <- fSRM(dep1/dep2 ~ actor*partner | fam, dat2)}
#' \code{s2 <- update(s1, delta=TRUE)}
#'
#' @export
#' @param formula A formula that defines the variable names. Should be in one of following formats: (1) Single manifest dependent variable: DV ~ actor.id * partner.id | group.id, (2) Multiple indicators for dependent variable: DV1/DV2/DV3 ~ actor.id * parter.id | group.id.
#' @param data A data frame with all variables defined by \code{formula}. Must be in long format where each row describes one directed dyadic relationship.
#' @param drop In three-member families at least one component has to be dropped. \code{drop} defines which one: "none": drop nothing; "family" - drop family effect; "reciprocities" - drop individual reciprocities; "actor" - drop actor factors and actor-partner covariances; "partner" - drop partner effects and actor-partner covariances; "default": drop nothing in >= 4 members and drop family effect with 3 members. Although usually not necessary, the drop parameter can also be applied to >= 4 member families.
#' @param add Additional lavaan syntax pasted at the end of the generated model. Can contain, for example, user specified error correlations.
#' @param IGSIM Define intragenerational similarity correlations. Must be a list where the levels of actor.id and partner.id are combined, e.g.: \code{IGSIM=list(c("m", "f"), c("c", "y"))}. Here "m"other and "f"ather are defined as one generation, and "y"ounger and "o"lder as the other generation.
#' @param selfmode Defines the style how the selfratings are combined with the latent actor and partner effects. If \code{selfmode="cor"} they are correlated (as in REFERENCE), if \code{selfmode="kq"} the k and q paths are calculated (see Kenny & West, 2010)
#' @param syntax In that variable the user can directly provide a lavaan model syntax. Then no automatical model syntax is generated; it is important that the variable nakes in the formula
#' @param add.variable Not yet fully implemented: Add external variables to the model syntax.
#' @param ... Additional arguments passed to the \code{sem} function of \code{lavaan}
#' @param means Should the structured means of the SRM factors be calculated?
#' @param group Variable name indicating group membership
#' @param delta Compare groups with the delta method?

## OLD PARAMETERS, NOT CURRENTLY USED
# @param err Defines the type of correlations between error terms. err = 1: Correlate same items BETWEEN ALL RATERS (e.g., Dyadic Data Analysis, Kenny, Kashy, & Cook, 2000); err = 2: Correlate same items WITHIN RATERS (e.g., Branje et al., 2003, Eichelsheim)
# @param self Should self-ratings be included in the analysis (if present in the data set)?


#' @references
#' Kenny, D. A., & West, T. V. (2010). Similarity and Agreement in Self-and Other Perception: A Meta-Analysis. Personality and Social Psychology Review, 14(2), 196-213. doi:10.1177/1088868309353414

fSRM <-
function(formula=NULL, data, drop="default", add="", means=FALSE, delta=FALSE, IGSIM=list(), add.variable=c(), selfmode="cor", syntax="", group=NULL, ...) {
	
	dots <- list(...)
	
	# TODO: Re-introduce self-ratings? Preliminarily, fix it to FALSE
	self <- FALSE
	
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
	
	# add group variable (for group comparison)
	if (!is.null(group)) {
		g2 <- ddply(data, group.id, function(x) x[1, group])
		colnames(g2) <- c(group.id, group)
		fam <- merge(fam, g2, by=group.id)
	}
	
	# remove all-NA columns
	NAcol <- which(apply(fam, 2, function(x) sum(is.na(x))) == nrow(fam))
	if(length(NAcol)>0)	{fam <- fam[, -NAcol]}
	fam <- na.omit(fam)
	included <- fam[, group.id]
	
	roles <- sort(unique(data[, actor.id]))
	
	# define defaults for drop
	drop <- match.arg(drop, c("nothing", "family", "reciprocities", "actor", "partner", "default"))
	if (drop == "default" & length(roles) == 3 & syntax=="") {
		message("Three-member families: Dropping family factor per default.")
		drop <- "family"
	}
	if (drop == "default" & length(roles) > 3 & syntax=="") {drop <- "nothing"}
	
	# Do some sanity checks
	if (length(roles) == 3 & drop == "nothing" & means == FALSE) {warning('Data set with 3-member-groups detected - model is not identified. Maybe you should remove the family effect (drop = "family") or one of the reciprocities?')}
	if (!identical(sort(unique(data[, actor.id])), sort(unique(data[, partner.id])))) {
		warning("Actor.id and Partner.id have different factor levels; results might be wrong!")
	}
	if (delta==TRUE & is.null(group)) stop("For comparing groups with the delta method you have to provide a `group`variable.")
	
	
	# if no syntax is directly provided:
	if (syntax == "") {
		
		if (!is.null(group)) {
			groupnames <- as.character(unique(fam$group))
		} else {
			groupnames <- NULL
		}
		
		syntax0 <- buildSRMSyntaxLatent(roles, var.id, drop=drop, err="default", IGSIM=IGSIM, means=means, delta=delta, groupnames=groupnames, self=self, add.variable=add.variable, selfmode=selfmode)
	
		syntax <- paste(syntax0, add, sep="\n")
	} else {
		print("Model syntax is directly specified; skipping buildfSRMSyntax")
	}
	
	cat(syntax)

	
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
			auto.cov.y 	= TRUE, 
			group		= group, ...)
		
	suppressWarnings(
		SS <- standardizedSolution(m, type="std.all")
	)
	
	res <- list(
		fit		= m,
		SS		= SS,
		syntax	= syntax,
		roles	= roles,
		actor.id 	= actor.id,
		partner.id 	= partner.id,
		group.id 	= group.id,
		var.id	= var.id,
		drop	= drop,
		means	= means,
		delta	= delta,
		group	= group,
		groupnames = groupnames,
		IGSIM	= IGSIM,
		self	= self,
		selfmode	= selfmode,
		call	= call,
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
predict.fSRM <- function(x, newdata, ...) {
	
	# TODO: This snippet is also in fSRM function --> refactor
	# Restructure data format from long to wide
	fam0 <- list()
	for (v in c(x$var.id, x$add.variable)) {
		fam0[[v]] <- dcast(newdata[, c(x$var.id, x$actor.id, x$partner.id, x$group.id, x$add.variable)], formula(paste(x$group.id, "~", x$actor.id, "+", x$partner.id)), value.var=v)
		colnames(fam0[[v]])[-1] <- paste(colnames(fam0[[v]])[-1], v, sep="_")
	}

	fam <- merge.rec(fam0, by=x$group.id)
	
	#print(str(x$data))
	#print(str(fam))
	predict(x$fit, newdata=fam)
}