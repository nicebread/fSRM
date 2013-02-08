# err = 1: Correlate same items BETWEEN ALL RATERS (e.g., DDA)
# err = 2: Correlate for same items WITHIN RATERS (e.g., Branje et al., 2003, Eichelsheim)

# c.p, c.t: constraints. Equal characters have equal weight

buildSRMSyntaxLatent <-
function(roles, var.id, self=FALSE, IGSIM = list(), fe=TRUE, err=2, add.variable=c(), selfmode="cor", ...) {

	dots <- list(...)

	# helper function: paste two vectors together, but only if elements are not identical
	pasteNS <- function(x, y, v="", sep="_", self=FALSE) {
		if (v[1] == "") {
			res <- c()
			for (i in x) {
				for (j in y) {if (i != j | self==TRUE) {res <- c(res, paste(i,j, sep=sep))}}
			}
			return(res)
		} else {
			res <- c()
			for (k in v) {
				for (i in x) {
					for (j in y) {if (i != j | self==TRUE) {res <- c(res, paste(i,j,k, sep=sep))}}
				}
			}
			return(res)
		}
	}

	# Family effect
	FE <- paste("# Family effect:\nFE =~ ", paste("1*", pasteNS(roles, roles, var.id), sep="", collapse=" + "), sep="")
	if (self == TRUE) {
		# self factors (not indicators!) should load on family effect, but with free loadings (D. Kenny, Email from June 15, 2012)
		for (p in roles) {FE <- paste(FE, " + ", paste("S", p, sep="", collapse=" + "), sep="")}
	}
	# for (a in add.variable) {
	# 		for (r in roles) {FE <- paste(FE, " + ", r, "_", r, "_", a, sep="")}
	# 	}
	
	
	
	# Actor effects
	AE <- "\n# Actor effects:\n"
	for (p in roles) {AE <- paste(AE, "A", p, " =~ ", paste("1*", pasteNS(p, roles, var.id), sep="", collapse=" + "), "\n", sep="")}

	# Partner effects
	PE <- "# Partner effects:\n"
	for (t in roles) {PE <- paste(PE, "P", t, " =~ ", paste("1*", pasteNS(roles, t, var.id), sep="", collapse=" + "), "\n", sep="")}

	# Relationship effects
	RE <- "# Relationship effects:\n"
	for (p in roles) {
		for (t in roles) {
			if (p != t) {RE <- paste(RE, "R", substr(p, 1, 1), substr(t, 1, 1), " =~ ", paste("1*", pasteNS(p, t, var.id), collapse=" + "), "\n", sep="")}
		}
	}

	# generalized reciprocity
	GR <- "# Generalized reciprocity:\n"
	for (p in roles) {GR <- paste(GR, "A", p, " ~~ P", p, "\n", sep="")}

	# dyadic reciprocity
	DR <- "# Dyadic reciprocity:\n"
	for (p in 1:length(roles)) {
		for (t in 1:length(roles)) {
			if ((p < t) & (roles[p] != roles[t])) {
				DR <- paste(DR, "R", substr(roles[p], 1, 1), substr(roles[t], 1, 1), " ~~ ", "R", substr(roles[t], 1, 1), substr(roles[p], 1, 1), "\n", sep="")
			}
		}
	}
	

	
	# method correlations: The same items are allowed to correlate
	# ERR1: Correlate same items BETWEEN ALL RATERS (e.g., DDA)
	ERR1 <- "# Method covariance: Correlations among error terms:\n"
	count <- 1
	for (v in var.id) {
		M <- pasteNS(roles, roles, v, self=self)
		for (m1 in 1:length(M)) {
			for (m2 in 1:length(M)) {
				if ((m1 < m2) & (M[m1] != M[m2])) {
					ERR1 <- paste(ERR1, M[m1], " ~~ MF", count, "*", M[m2], "\n", sep="")
					count <- count + 1
				}
			}
		}
	}
	
	# ERR2: Correlate for same items WITHIN RATERS (e.g., Branje et al., 2003, Eichelsheim)
	# define correlations between error terms
	ERR2 <- "# Method covariance: Correlations among error terms:\n"
	count <- 1
	
	
	for (v in 1:length(var.id)) {
		for (p in 1:length(roles)) {
			for (t1 in 1:length(roles)) {
				for (t2 in 1:length(roles)) {
					if (self == FALSE) {
						if (p != t1 & p != t2 & t1 < t2) {
							ERR2 <- paste(ERR2, pasteNS(roles[p], roles[t1], var.id[v]), " ~~ ERR", count, "*", pasteNS(roles[p], roles[t2], var.id[v]), "\n", sep="")
							count <- count + 1
						}
					} else {
						if (t1 < t2) {
							ERR2 <- paste(ERR2, paste(roles[p], roles[t1], var.id[v], sep="_"), " ~~ ERR", count, "*", paste(roles[p], roles[t2], var.id[v], sep="_"), "\n", sep="")
							count <- count + 1
						}
					}
				}
			}
		}
	}
	

	# equality constraints
	count <- 1
	EQ <- "# Equality constraints\n"
	if (!is.null(dots[["c.a"]])) {
		for (p in roles) {
			EQ <- paste(EQ, "A", p, " ~~ ", dots[["c.a"]][count], "*A", p, "\n", sep="")
			count <- count + 1
		}
	}
	if (!is.null(dots[["c.p"]])) {
		count <- 1
		for (t in roles) {
			EQ <- paste(EQ, "P", t, " ~~ ", dots[["c.p"]][count], "*P", t, "\n", sep="")
			count <- count + 1
		}
	}


	# intergenerational similarity
	if (length(IGSIM) > 0) {
		igsim <- "# intergenerational similarity:\n"
		for (i in 1:length(IGSIM)) {
			igsim <- paste(igsim, "A", IGSIM[[i]][1], " ~~ IGSIMA", i,"*A", IGSIM[[i]][2], "\n", sep="")
			igsim <- paste(igsim, "P", IGSIM[[i]][1], " ~~ IGSIMP", i,"*P", IGSIM[[i]][2], "\n", sep="")
		}
	}
	
	
	# self-ratings: assumed similarity and self-other agreement
	if (self == TRUE) {
		SELF <- "# Build self-rating factors:\n"
		for (p in roles) {
				# TODO: hier mit 1* oder ohne? paste("1*", paste(p, p, var.id, sep="_")
				SELF <- paste(SELF, "S", p, " =~ ", paste("", paste(p, p, var.id, sep="_"), sep="", collapse=" + "), "\n", sep="")
		}
		
		if (selfmode=="cor") {
			SELF <- paste(SELF, "\n\n# Assumed similarity:\n")
			for (p in roles) {SELF <- paste(SELF, "S", p, " ~~ A", p, "\n", sep="")}
		
			SELF <- paste(SELF, "\n\n# Self-other-agreement:\n")
			for (p in roles) {SELF <- paste(SELF, "S", p, " ~~ P", p, "\n", sep="")}
		}
		if (selfmode=="kq") {
			SELF <- paste(SELF, "\n\n# Assumed similarity:\n")
			for (p in roles) {SELF <- paste(SELF, "S", p, " ~ A", p, "\n", sep="")}
		
			SELF <- paste(SELF, "\n\n# Self-other-agreement:\n")
			for (p in roles) {SELF <- paste(SELF, "S", p, " ~ P", p, "\n", sep="")}
		}
	}


	# any other variables?
	addv <- ""
	# for (a in add.variable) {
	# 	addv <- "\n\n# Add extra variables to model:\n"
	# 	for (r in roles) {addv <- paste(addv, a, r, " =~ ", r, "_", r, "_", a, "\n", sep="")}
	# 	addv <- paste(addv, "# Actor effect ~~", a, ":\n")
	# 	for (r in roles) {addv <- paste(addv, a, r, " ~~ A", r, "\n", sep="")}
	# 	addv <- paste(addv, "# Partner effect ~~", a, ":\n")
	# 	for (r in roles) {addv <- paste(addv, a, r, " ~~ P", r, "\n", sep="")}
	# 	
	# 	if (self == TRUE) {
	# 		addv <- paste(addv, "# Self rating ~~", a, ":\n")
	# 		for (r in roles) {addv <- paste(addv, a, r, " ~~ S", r, "\n", sep="")}
	# 	}
	# 	# for (p in roles) {
	# 	# 	for (t in roles) {
	# 	# 		if (p != t) {addv <- paste(addv, a, p, " ~~ R", substr(t, 1, 1), substr(p, 1, 1), "\n", sep="")}
	# 	# 	}
	# 	# }
	# 	
	# }
	
	
	## Add internal information about variable names etc.
	SRM <- "### lavaan syntax for family SRM\n"
	SRM <- paste(SRM, "### ROLES:'", paste(roles, collapse="','"), "'\n", sep="")
	SRM <- paste(SRM, "### VARID:'", paste(var.id, collapse="','"), "'\n", sep="")
	
	if (fe==TRUE) SRM <- paste(SRM, FE)
	SRM <- paste(SRM, AE, PE, RE, ifelse(err==1, ERR1, ERR2), GR, DR, EQ, sep="\n")
	if (length(IGSIM) > 0) {SRM <- paste(SRM, igsim, sep="\n")}
	if (self == TRUE) {SRM <- paste(SRM, SELF, sep="\n")}
	if (addv!="") SRM <- paste(SRM, addv)
	return(SRM)
}
