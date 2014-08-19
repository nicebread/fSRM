#' @title Rerun a fSRM model with new parameters
#' @description
#' Rerun a fSRM model with new parameters
#'
#' @method update fSRM
#' @S3method update fSRM

#' @param object A fSRM object.
#' @param evaluate Set to TRUE.
#' @param ... Other parameters (currently not used)
update.fSRM <- function(object, evaluate=TRUE, ...) {
	call <- object$call
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



#' @title Predict new cases based on a fitted fSRM model
#' @description
#' Predict new cases based on a fitted fSRM model
#'
#' @method predict fSRM
#' @S3method predict fSRM

#' @param object A fSRM object.
#' @param newdata A data frame with exactly the same structure as the data frame on which the original fSRM object is based on.
#' @param ... Other parameters (currently not used)
predict.fSRM <- function(object, newdata, ...) {
	
	# TODO: This snippet is also in fSRM function --> refactor
	# Restructure data format from long to wide
	fam0 <- list()
	for (v in c(object$var.id, object$add.variable)) {
		fam0[[v]] <- dcast(newdata[, c(object$var.id, object$actor.id, object$partner.id, object$group.id, object$add.variable)], formula(paste(object$group.id, "~", object$actor.id, "+", object$partner.id)), value.var=v)
		colnames(fam0[[v]])[-1] <- paste(colnames(fam0[[v]])[-1], v, sep="_")
	}

	fam <- merge.rec(fam0, by=object$group.id)
	
	#print(str(object$data))
	#print(str(fam))
	predict(object$fit, newdata=fam)
}


#' @title Plot the relative variances of an fSRM-object
#' @description
#' Plot the relative variances of an fSRM-object
#'
#' @method plot fSRM
#' @S3method plot fSRM
#' @importFrom scales percent
#' @import ggplot2
#' @import shape

#' @param x A fSRM object.
#' @param ... Other parameters (currently not used)
#' @param mean If FALSE, the relative variances are plotted. If TRUE, the mean structure is plotted.
#' @param bw Black/white plotting?
#' @param onlyStable In case of variance plots: Should only the partitioning of the \emph{stable} variance (without error) be plotted?
#' @examples
#' data(two.indicators)

#' # 4 persons, 1 indicator
#' f4.1 <- fSRM(dep1 ~ actor.id*partner.id | family.id, two.indicators)
#' f4.1
#' plot(f4.1)
#' plot(f4.1, bw=TRUE)
#' 
#' # 4 persons, 2 indicators
#' f4.2 <- fSRM(dep1/dep2 ~ actor.id*partner.id | family.id, two.indicators)
#' f4.2
#' plot(f4.2)
#' plot(f4.2, bw=TRUE)
plot.fSRM <- function(x, ..., mean=FALSE, bw=FALSE, onlyStable=FALSE) {
	
	# plot relative percentages
	if (mean == FALSE) {
		relvar <- percTable(x)$stand
		relvar <- relvar[1:(nrow(relvar)-1), c("Family", "Actor", "Partner", "Relationship", "Error")]
		relvar$dyad <- rownames(relvar)
		
		if (onlyStable == TRUE) {
			relvar$Sum <- 1 - relvar$Error
			relvar$Error <- 0
			for (i in 1:4) relvar[, i] <- relvar[, i]/relvar$Sum
			relvar$Sum <- NULL
		}
	
		relvar.long <- melt(relvar, id.vars="dyad")
		relvar.long$variable <- factor(relvar.long$variable, levels=rev(c("Family", "Actor", "Partner", "Relationship", "Error")))
		
		if (bw==FALSE) {
			colors <- c("#DDDDDD", "#7fc97f", "#beaed4", "#fdc086", "#386cb0")
		} else {
			colors <- gray(c(0.9, 0.8, 0.6, 0.4, 0.2))
		}
		
		names(colors) <- rev(c("Family", "Actor", "Partner", "Relationship", "Error"))
		
		# remove non-present components
		lablist <- c("Family", "Actor", "Partner", "Relationship", "Error")
		for (i in c("Family", "Actor", "Partner", "Error")) {
			if (all(relvar[, i] == 0)) {
				lablist <- lablist[lablist != i] 
				relvar.long <- relvar.long[relvar.long$variable != i, ]
				relvar.long$variable <- factor(relvar.long$variable, levels=rev(lablist))
				colors <- colors[which(names(colors) != i)]
			}
		}
	
		p1 <- ggplot(relvar.long, aes_string(x="dyad", y="value", fill="variable", order = -as.numeric("variable"))) + geom_bar(stat="identity") + xlab("Dyad") + scale_y_continuous(labels=percent) + scale_fill_manual("Component", values=colors)
	
		if (bw==TRUE) p1 <- p1 + theme_bw()
		if (onlyStable==FALSE) {
			p1 <- p1 + ylab("Percentage variance")
		} else {
			p1 <- p1 + ylab("Percentage stable variance")
		}
		
		return(p1)
	}
	
	
	# plot mean structure
	if (mean == TRUE) {
		
	}
}