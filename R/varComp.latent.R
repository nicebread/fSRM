# Gives a structured table of all (co)variances

varComp <-
function(x) {
	eff <- as.data.frame(parameterEstimates(x$res))
	eff$f <- paste(eff$lhs, eff$op, eff$rhs)
	res <- matrix(NA, ncol=6, nrow=ifelse(x$fe==TRUE, 1, 0) + ifelse(x$self==TRUE, length(x$roles), 0) + length(x$roles)*2 + length(x$roles)*(length(x$roles)-1))
	
	colnames(res) <- c("est","se","z","pvalue","ci.lower","ci.upper")
	rownames(res) <- rep("", nrow(res))

	count <- 1
	if (x$fe == TRUE) {
		rownames(res)[1] <- "FE ~~ FE"
		res[1, ] <- as.matrix(eff[eff$f == "FE ~~ FE", c("est","se","z","pvalue","ci.lower","ci.upper")])
		count <- count + 1
	}

	for (p in 1:length(x$roles)) {
		res[count, ]  <- as.matrix(eff[eff$f == paste("A", x$roles[p], " ~~ ", "A", x$roles[p], sep=""), c("est","se","z","pvalue","ci.lower","ci.upper")])
		rownames(res)[count] <- paste("A", x$roles[p], " ~~ ", "A", x$roles[p], sep="")
		count <- count + 1
	}
	for (p in 1:length(x$roles)) {
		res[count, ]  <- as.matrix(eff[eff$f == paste("P", x$roles[p], " ~~ ", "P", x$roles[p], sep=""), c("est","se","z","pvalue","ci.lower","ci.upper")])
		rownames(res)[count] <-  paste("P", x$roles[p], " ~~ ", "P", x$roles[p], sep="")
		count <- count + 1
	}
	for (p in 1:length(x$roles)) {
		for (t in 1:length(x$roles)) {
			if (p != t) {
			res[count, ]  <- as.matrix(eff[eff$f == paste(paste("R", substr(x$roles[p], 1, 1), substr(x$roles[t], 1, 1), sep=""), "~~", paste("R", substr(x$roles[p], 1, 1), substr(x$roles[t], 1, 1), sep="")), c("est","se","z","pvalue","ci.lower","ci.upper")])
			rownames(res)[count] <- paste(paste("R", substr(x$roles[p], 1, 1), substr(x$roles[t], 1, 1), sep=""), "~~", paste("R", substr(x$roles[p], 1, 1), substr(x$roles[t], 1, 1), sep=""))
			count <- count + 1
		}
		}
	}
	
	if (x$self == TRUE) {
		for (p in 1:length(x$roles)) {
			res[count, ]  <- as.matrix(eff[eff$f == paste("S", x$roles[p], " ~~ ", "S", x$roles[p], sep=""), c("est","se","z","pvalue","ci.lower","ci.upper")])
			rownames(res)[count] <-  paste("S", x$roles[p], " ~~ ", "S", x$roles[p], sep="")
			count <- count + 1
		}
	}
	
	res2 <- data.frame(f=rownames(res), as.data.frame(res))
	rownames(res2) <- NULL
	res2$f <- as.character(res2$f)
	return(res2)
}

getGR <- function(x) {
	# SS = standardized solution: get correlation for that
	SS <- getCor(x, ops=c("~~", "~"))
	GR <- data.frame()
	for (t in x$roles) {
		GR0 <- SS[SS$f == paste("A", t, " ~~ ", "P", t, sep=""), ]
		
		SD1 <- T[paste("A", t, " ~~ ", "A", t, sep=""), ]
		SD2 <- T[paste("P", t, " ~~ ", "P", t, sep=""), ]
		if (is.na(SD1$pvalue)) SD1$pvalue <- 1
		if (is.na(SD2$pvalue)) SD2$pvalue <- 1
		
		#if (SD1["pvalue"] <= .05 & SD2["pvalue"] <= .05) {
		#	GR0$COR <- GR0$est / (sqrt(SD1$est)*sqrt(SD2$est))
		#} else {
		#	GR0$COR <- NA_real_
		#	GR0$est <- NA_real_
		#}
		
		GR <- rbind(GR, GR0)
	}
	GR$COR <- as.numeric(GR$COR)
	GR
}

getDR <- function(x) {
	# SS = standardized solution: get correlation for that
	SS <- getCor(x, ops=c("~~", "~"))
	DR <- data.frame()
	for (p in 1:length(x$roles)) {
		for (t in 1:length(x$roles)) {
			if ((p < t) & (x$roles[p] != x$roles[t])) {
				
				DR0 <- SS[SS$f == paste("R", substr(x$roles[p], 1, 1), substr(x$roles[t], 1, 1), " ~~ ", "R", substr(x$roles[t], 1, 1), substr(x$roles[p], 1, 1), sep=""), ]
				
				SD1 <- T[paste("R", substr(x$roles[p], 1, 1), substr(x$roles[t], 1, 1), " ~~ ", "R", substr(x$roles[p], 1, 1), substr(x$roles[t], 1, 1), sep=""), ]

				SD2 <- T[paste("R", substr(x$roles[t], 1, 1), substr(x$roles[p], 1, 1), " ~~ ", "R", substr(x$roles[t], 1, 1), substr(x$roles[p], 1, 1), sep=""), ]

				if (is.na(SD1$pvalue)) SD1$pvalue <- 1
				if (is.na(SD2$pvalue)) SD2$pvalue <- 1
				
				# if (SD1["pvalue"] <= .05 & SD2["pvalue"] <= .05 & !is.na(DR0$est)) {
				# 	DR0$COR <- DR0$est / (sqrt(SD1$est)*sqrt(SD2$est))
				# } else {
				# 	DR0$COR <- NA_real_
				# 	DR0$est <- NA_real_
				# }
				DR <- rbind(DR, DR0)
			}
		}
	}
	DR$COR <- as.numeric(DR$COR)
	DR
}