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