#' @title Test actor and partner means for equality
#'
#' @description
#' Test actor and partner means for equality, using a Wald test.
#'
#' @export
#' @param x A fSRM object-
#' @param digits Digits to which the printed results are rounded

equalMeans <- function(x, digits=3) {
	if (x$means == FALSE) stop("Please provide an fSRM object with mean structure (set `means` to TRUE)")
	eff <- parameterEstimates(x$fit, standardized=TRUE)
	vc <- vcov(x$fit)
	r <- x$roles[1]

	A <- eff[grepl(".means.A", eff$label) & eff$lhs != paste0(style$actor, ".", r), ]
	A.vc <- vc[A$label, A$label]
	Wald.A <- A$est %*% solve(A.vc) %*% A$est

	P <- eff[grepl(".means.P", eff$label) & eff$lhs != paste0(style$partner, ".", r), ]
	P.vc <- vc[P$label, P$label]
	Wald.P <- P$est %*% solve(P.vc) %*% P$est
	
	res <- data.frame(Wald=c(Wald.A, Wald.P), df = length(x$roles)-1, p.value=c(1-pchisq(Wald.A, length(x$roles)-1), 1-pchisq(Wald.P, length(x$roles)-1)))
	rownames(res) <- c("H0: Equal actor means", "H0: Equal partner means")
	
	print(round(res, digits))
	invisible(res)
}