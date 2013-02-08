anova.fSRM <- function(object, ...) {
	mcall <- match.call(expand.dots = TRUE)
	print(mcall)
    dots <- list(...)
    modp <- if(length(dots)) sapply(dots, is, "fSRM") else logical(0)
	if(!any(modp)) stop("Need at least two models to compare")
	
	mods <- list(object$res)
	for (d in dots[modp]) {mods <- c(mods, d$res)}
    names(mods) <- sapply(as.list(mcall)[c(FALSE, TRUE, modp)], as.character)

	#S0 <- list(s1=S1, s2=S2)
	do.call("anova", as.list(mods))
}