# plot relative percentages
plot_relvar <- function(x, bw=FALSE, onlyStable=FALSE, group=1, ...) {
	relvar <- percTable(x, group=group)$stand
	
	if (any(relvar[, 1:4] < 0) == TRUE) {
		warning(paste("In group", group, "some variances are negative. Plot is not well-defined, please consdier setting `noNegVar = TRUE`."), call.=FALSE)
	}
	
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
	relvar.long$dyad <- gsub("_", "->\n", relvar.long$dyad, fixed=TRUE)
	
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

	relvar.long$ord <- -as.numeric(relvar.long$variable)
	p1 <- ggplot(relvar.long, aes_string(x="dyad", y="value", fill="variable", order = "ord")) + geom_bar(stat="identity") + xlab("Dyad") + scale_y_continuous(labels=percent) + scale_fill_manual("Component", values=colors) + theme_bw()

	if (onlyStable==FALSE) {
		p1 <- p1 + ylab("Percentage variance")
	} else {
		p1 <- p1 + ylab("Percentage stable variance")
	}
	
	return(p1)
}


plot_meanstruc <- function(x, ...) {
	if (x$means == FALSE) stop("You have to provide a fSRM with mean structure; use: fSRM(..., means=TRUE)")
    eff <- parameterEstimates(x$fit)
    est_act <- eff[grepl(paste0(".means.", style$actor, "."), eff$label, fixed=TRUE), c(1, 5)]
	est_par <- eff[grepl(paste0(".means.", style$partner, "."), eff$label, fixed=TRUE), c(1, 5)]
	est_rel <- eff[grepl(paste0(".means.", style$relationship, "."), eff$label, fixed=TRUE), c(1, 5)]
  

  # Create axis & labels
	aM <- attr(regexpr(paste0(style$actor, "."), est_act$lhs, fixed=TRUE), "match.length")
	est_act$role <- substr(est_act$lhs, aM+1, nchar(est_act$lhs))	# actor labels

	aP <- attr(regexpr(paste0(style$partner, "."), est_par$lhs, fixed=TRUE), "match.length")
	est_par$role <- substr(est_par$lhs, aP+1, nchar(est_par$lhs))	# actor labels

	aR <- attr(regexpr(paste0(style$relationship, "."), est_rel$lhs, fixed=TRUE), "match.length")
	est_rel$role <- substr(est_rel$lhs, aR+1, nchar(est_rel$lhs))	# actor labels

	res <- data.frame()
	FE <- eff[grepl(".means.FE", eff$label, fixed=TRUE), c(1, 5)]$est
	for (a in x$roles) {
		for (p in x$roles) {
			if (a != p) {
			
				# start of arrow
				res <- rbind(res, data.frame(
					pos="start", 
					actor = a, 
					partner = p, 
					dyad = paste0(a, "->\n", p),
					a.effect = FE,
					p.effect = FE + est_act[est_act$role==a, "est"],
					r.effect = FE + est_act[est_act$role==a, "est"] + est_par[est_par$role==p, "est"]
				))
			
				# end of arrow
				res <- rbind(res, data.frame(
					pos="end", 
					actor = a, 
					partner = p, 
					dyad = paste0(a, "->\n", p),
					a.effect = FE + est_act[est_act$role==a, "est"],
					p.effect = FE + est_act[est_act$role==a, "est"] + est_par[est_par$role==p, "est"],
					r.effect = FE + est_act[est_act$role==a, "est"] + est_par[est_par$role==p, "est"] + est_rel[est_rel$role==paste0(a, ".", p), "est"]
				))
			}
		}
	}

	res2 <- melt(res, id.vars=c("actor", "partner", "dyad", "pos"))
	res3 <- dcast(res2, variable + actor + partner + dyad ~ pos)
	res3$variable <- factor(res3$variable, levels=c("a.effect", "p.effect", "r.effect"), labels=c("Actor effect", "Partner effect", "Relationship effect"))
	res3$group <- res3$variable:res3$dyad
	res3$x2 <- as.numeric(res3$dyad) + (as.numeric(res3$variable)-2)*.2  # manual dodging of arrows
	
	p1 <- ggplot(res3, aes_string(x="x2", xend="x2", y="start", yend="end", group="group", linetype="variable")) + geom_segment(arrow=arrow(length = unit(0.07, "inches"), angle=20, type="closed"), linend="square") + geom_hline(yintercept=FE, linetype="dotted") + theme_bw() + scale_linetype_discrete("Component") + xlab("Dyad") + ylab(paste0("Estimated ", x$var.id)) + scale_x_discrete(labels=res3$dyad, limits=1:length(unique(res3$dyad)))
	
	return(p1)
}