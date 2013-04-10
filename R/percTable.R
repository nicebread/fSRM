percTable <-
function(x) {
	library(plyr)
	eff <- parameterEstimates(x$fit)
	SS <- x$SS
	
	eff$f <- paste(eff$lhs, eff$op, eff$rhs)
	res <- matrix(NA, ncol=6, nrow=length(x$roles)*(length(x$roles)-1))
	
	colnames(res) <- c("Family", "Actor Effect", "Partner Effect", "Relationship Effect", "Error", "SUM")
	rownames(res) <- rep("", nrow(res))
	count <- 1
	for (p in 1:length(x$roles)) {
		for (t in 1:length(x$roles)) {
			if (x$roles[p] != x$roles[t]) {
				#print(paste(p, t))
				res[count, 1:4] <- c(
					ifelse(x$fe == TRUE, eff[eff$f == "FE ~~ FE", "est"], 0), 
					eff[eff$f == paste(style$actor, ".", x$roles[p], " ~~ ", style$actor, ".", x$roles[p], sep=""), "est"],
					eff[eff$f == paste(style$partner, ".", x$roles[t], " ~~ ", style$partner, ".", x$roles[t], sep=""), "est"],
					eff[eff$f == paste(paste(style$relationship, ".", x$roles[p], ".", x$roles[t], sep=""), "~~", paste(style$relationship, ".", x$roles[p], ".", x$roles[t], sep=""), sep=" "), "est"])
					
					# error in all indicators
					err <- c()
					for (v in x$var.id) {
						err <- c(err, eff[eff$f == paste(paste(x$roles[p], x$roles[t], v, sep="_"), "~~", paste(x$roles[p], x$roles[t], v, sep="_")), "est"])
					}
					res[count, 5] <- mean(err)

				res[count, 6] <- sum(res[count, 1:5])
							
					
				rownames(res)[count] <- paste(x$roles[p], x$roles[t], sep="_")
				count <- count + 1
			}
		}
	}
	
	res2 <- round((res[, 1:5] / res[, 6])*100)
	res2 <- data.frame(addmargins(res2, margin=1, FUN=mean))
	res2$explained <- 100-res2$Error
	return(list(raw=res, stand=res2))
}
