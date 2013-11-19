#' @title Build lavaan syntax for a Social Relations Model with roles ("Family SRM")
#'
#' @description
#' Build lavaan syntax for a Social Relations Model with roles ("Family SRM"). This function is called by the fSRM function, but can be also 
#'
#' @details
#' None.
#'
#' @export
#' @param roles A vector with all role labels.
#' @param var.id A vector with the variable names of the DV indicators
#' @param drop In three-member families at least one component has to be dropped. \code{drop} defines which one: "none": drop nothing; "family" - drop family effect; "GR" - drop generalized reciprocities; "actor" - drop actor factors and actor-partner covariances; "partner" - drop partner effects and actor-partner covariances; "default": drop nothing in >= 4 members and drop family effect with 3 members. Although usually not necessary, the drop parameter can also be applied to >= 4 member families.
#' @param err Defines the type of correlations between error terms. err = "no": no error term correlations - this is the required mode for single indicators. err = "all": If multiple indicators are present, correlate same items BETWEEN raters (e.g., Dyadic Data Analysis, Kenny, Kashy, & Cook, 2000). err = "default": Set err to "no" for single indicators and to "all" for multiple indicators.
#' @param IGSIM Define intragenerational similarity correlations. Must be a list where the levels of actor.id and partner.id are combined, e.g.: \code{IGSIM=list(c("m", "f"), c("c", "y"))}. Here "m"other and "f"ather are defined as one generation, and "y"ounger and "o"lder as the other generation.
#' @param self Should self-ratings be included in the analysis (if present in the data set)?
#' @param selfmode Defines the style how the selfratings are combined with the latent actor and partner effects. If \code{selfmode="cor"} they are correlated (as in REFERENCE), if \code{selfmode="kq"} the k and q paths are calculated (see Kenny & West, 2010)
#' @param add.variable Not yet fully implemented: Add external variables to the model syntax.
#' @param ... Additional arguments (not documented yet)
#' @param means Should the structured means of the SRM factors be calculated?
#' @param diff Compare groups with the delta method?
#' @param groupnames Vector with the names of the groups (i.e., the values of the group column in the data set)

#' @references
#' Kenny, D. A., & West, T. V. (2010). Similarity and Agreement in Self-and Other Perception: A Meta-Analysis. Personality and Social Psychology Review, 14(2), 196-213. doi:10.1177/1088868309353414


buildSRMSyntaxLatent <-
function(roles, var.id, self=FALSE, IGSIM = list(), drop="default", err="default", means=FALSE, diff=FALSE, groupnames=NULL,  add.variable=c(), selfmode="cor", ...) {
	
	# define defaults for parameters
	err <- match.arg(err, c("no", "all", "default"))
	if (err == "default" & length(var.id) == 1) {err <- "no"}
	if (err == "default" & length(var.id) > 1) {err <- "all"}
		
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
	FE <- paste("# Family effect:\n", style$familyeffect," =~ ", paste("1*", pasteNS(roles, roles, var.id), sep="", collapse=" + "), sep="")
	if (self == TRUE) {
		# self factors (not indicators!) should load on family effect, but with free loadings (D. Kenny, Email from June 15, 2012)
		for (p in roles) {FE <- paste(FE, " + ", paste(style$self, ".", p, sep="", collapse=" + "), sep="")}
	}
	# for (a in add.variable) {
	# 		for (r in roles) {FE <- paste(FE, " + ", r, "_", r, "_", a, sep="")}
	# 	}
	
	
	
	# Actor effects
	AE <- "\n# Actor effects:\n"
	for (p in roles) {AE <- paste(AE, style$actor, ".", p, " =~ ", paste("1*", pasteNS(p, roles, var.id), sep="", collapse=" + "), "\n", sep="")}

	# Partner effects
	PE <- "# Partner effects:\n"
	for (t in roles) {PE <- paste(PE, style$partner, ".", t, " =~ ", paste("1*", pasteNS(roles, t, var.id), sep="", collapse=" + "), "\n", sep="")}

	# Relationship effects
	RE <- "# Relationship effects:\n"
	for (p in roles) {
		for (t in roles) {
			if (p != t) {RE <- paste(RE, style$relationship, ".", p, ".", t, " =~ ", paste("1*", pasteNS(p, t, var.id), collapse=" + "), "\n", sep="")}
		}
	}

	# generalized reciprocity
	GR <- "# Generalized reciprocity:\n"
	for (p in roles) {GR <- paste(GR, style$actor, ".", p, " ~~ ", style$partner, ".", p, "\n", sep="")}

	# dyadic reciprocity
	DR <- "# Dyadic reciprocity:\n"
	for (p in 1:length(roles)) {
		for (t in 1:length(roles)) {
			if ((p < t) & (roles[p] != roles[t])) {
				DR <- paste(DR, style$relationship, ".", roles[p], ".", roles[t], " ~~ ", style$relationship, ".", roles[t], ".", roles[p], "\n", sep="")
			}
		}
	}
	

	## ======================================================================
	## Define method covariance (correlations between errors)
	## ======================================================================
	
	
	ERR <- ""
	# single indicator: set error variance to zero
	if (length(var.id) == 1 | err == "no") {
		if (err != "no") warning("For one indicator error variances cannot correlate; setting err to 'no'")
			M <- pasteNS(roles, roles, var.id, self=self)
			ERR <- paste(paste0(M, " ~~ 0*", M), collapse="\n")
	} else {
		ERR <- ""
	}
	
	
	if (length(var.id) > 1 & err == "all") {
		# method correlations: The same items are allowed to correlate
		# ERR1: Correlate same items BETWEEN ALL RATERS (e.g., DDA)
		ERR <- "# Method covariance: Correlations among all error terms of the same items:\n"
		count <- 1
		for (v in var.id) {
			M <- pasteNS(roles, roles, v, self=self)
			for (m1 in 1:length(M)) {
				for (m2 in 1:length(M)) {
					if ((m1 < m2) & (M[m1] != M[m2])) {
						ERR <- paste(ERR, M[m1], " ~~ MF", count, "*", M[m2], "\n", sep="")
						count <- count + 1
					}
				}
			}
		}
	}
	
	# TODO: The old within-error correlations - I think we could delete that
	# if (length(var.id) > 1 & err == "within") {
# 		# ERR2: Correlate for same items WITHIN RATERS (e.g., Branje et al., 2003, Eichelsheim)
# 		# define correlations between error terms
# 		ERR <- "# Method covariance: Correlations among error terms of one item within actors:\n"
# 		count <- 1
# 	
# 	
# 		for (v in 1:length(var.id)) {
# 			for (p in 1:length(roles)) {
# 				for (t1 in 1:length(roles)) {
# 					for (t2 in 1:length(roles)) {
# 						if (self == FALSE) {
# 							if (p != t1 & p != t2 & t1 < t2) {
# 								ERR <- paste(ERR, pasteNS(roles[p], roles[t1], var.id[v]), " ~~ ERR", count, "*", pasteNS(roles[p], roles[t2], var.id[v]), "\n", sep="")
# 								count <- count + 1
# 							}
# 						} else {
# 							if (t1 < t2) {
# 								ERR <- paste(ERR, paste(roles[p], roles[t1], var.id[v], sep="_"), " ~~ ERR", count, "*", paste(roles[p], roles[t2], var.id[v], sep="_"), "\n", sep="")
# 								count <- count + 1
# 							}
# 						}
# 					}
# 				}
# 			}
# 		}
# 	}





	# intragenerational similarity
	if (length(IGSIM) > 0) {
		igsim <- "# intragenerational similarity:\n"
		for (i in 1:length(IGSIM)) {
			igsim <- paste(igsim, style$actor, ".", IGSIM[[i]][1], " ~~ IGSIMA", i,"*", style$actor, ".", IGSIM[[i]][2], "\n", sep="")
			igsim <- paste(igsim, style$partner, ".", IGSIM[[i]][1], " ~~ IGSIMP", i,"*", style$partner, ".", IGSIM[[i]][2], "\n", sep="")
		}
	}
	
	
	# self-ratings: assumed similarity and self-other agreement
	if (self == TRUE) {
		SELF <- "# Build self-rating factors:\n"
		for (p in roles) {
				# TODO: hier mit 1* oder ohne? paste("1*", paste(p, p, var.id, sep="_")
				SELF <- paste(SELF, style$self, ".", p, " =~ ", paste("", paste(p, p, var.id, sep="_"), sep="", collapse=" + "), "\n", sep="")
		}
		
		if (selfmode=="cor") {
			SELF <- paste(SELF, "\n\n# Assumed similarity:\n")
			for (p in roles) {SELF <- paste(SELF, style$self, ".", p, " ~~ ", style$actor, ".", p, "\n", sep="")}
		
			SELF <- paste(SELF, "\n\n# Self-other-agreement:\n")
			for (p in roles) {SELF <- paste(SELF, style$self, ".", p, " ~~ ", style$partner, ".", p, "\n", sep="")}
		}
		if (selfmode=="kq") {
			SELF <- paste(SELF, "\n\n# Assumed similarity:\n")
			for (p in roles) {SELF <- paste(SELF, style$self, ".", p, " ~ ", style$actor, ".", p, "\n", sep="")}
		
			SELF <- paste(SELF, "\n\n# Self-other-agreement:\n")
			for (p in roles) {SELF <- paste(SELF, style$self, ".", p, " ~ ", style$partner, ".", p, "\n", sep="")}
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
	
## ======================================================================
## add structured means
# TODO: Check: should self ratings be included somehow?
## ======================================================================

SM <- ""
if (means==TRUE & diff == FALSE) {
	SM.prefix <- ".means."
	SM <- ""
	SM <- "\n## Compute structured means\n# Define labels for subsequent constraints\n"
	
	SM <- paste(SM, paste(style$familyeffect, " ~ ", SM.prefix, style$familyeffect, "*1\n", sep=""))

	for (p in roles) {SM <- paste(SM, style$actor, ".", p, " ~ ", SM.prefix, style$actor, ".", p, "*1\n", sep="")}
	for (p in roles) {SM <- paste(SM, style$partner, ".", p, " ~ ", SM.prefix, style$partner, ".", p, "*1\n", sep="")}

	for (p in roles) {
		for (t in roles) {
			if (p != t) {SM <- paste(SM, style$relationship, ".", p, ".", t, " ~ ", SM.prefix, style$relationship, ".", p, ".", t, "*1\n", sep="")}
		}
	}
	
	# if (drop == "family") {
	# 	SM <- paste(SM, "\n\n# For three person families: set variance of family effect to zero\n")
	# 	SM <- paste(SM, style$familyeffect, "~~ 0*", style$familyeffect)
	# }
	
	SM <- paste0(SM, "\n\n# set means of observed variables to zero\n")
	SM <- paste0(SM, paste0(pasteNS(roles, roles, var.id), "~ 0", collapse="\n"))
	
	SM <- paste0(SM, "\n\n# set constraints on means for identifiability\n")
	
	 SM <- paste0(SM, paste(paste(SM.prefix, style$actor, ".", roles, sep="", collapse=" + "), "== 0\n"))
	 SM <- paste0(SM, paste(paste(SM.prefix, style$partner, ".", roles, sep="", collapse=" + "), "== 0\n"))
	 
	 for (p in roles) {
	 	SM <- paste0(SM, paste(paste(SM.prefix, style$relationship, ".", p, ".", roles[roles != p], sep="", collapse=" + "), "== 0\n"))
	 }
	 for (p in roles) {
	 	SM <- paste0(SM, paste(paste(SM.prefix, style$relationship, ".", roles[roles != p], ".", p, sep="", collapse=" + "), "== 0\n"))
	 }
	
}



## ======================================================================
## Delta method for group comparisons
## ======================================================================


DM <- ""
if (diff==TRUE) {
	if (is.null(groupnames)) stop("You must provide the names of the groups in `groupnames`.")
	DM.prefix <- ".means"
	DM.var <- ".var"
	DM <- ""
	DM <- "\n\n## deltamethod for comparing two groups\n"

	# define new labels for the means
	DM <- paste(DM, paste(style$familyeffect, " ~ c(", paste0(DM.prefix, groupnames, ".", style$familyeffect, collapse=","), ")*1\n", sep=""))

	for (p in roles) {DM <- paste(DM, style$actor, ".", p, " ~ c(", paste0(DM.prefix, groupnames, ".", style$actor, ".", p, collapse=","), ")*1\n", sep="")}
	for (p in roles) {DM <- paste(DM, style$partner, ".", p, " ~ c(", paste0(DM.prefix, groupnames, ".", style$partner, ".", p, collapse=","), ")*1\n", sep="")}

	for (p in roles) {
		for (t in roles) {
			if (p != t) {DM <- paste(DM, style$relationship, ".", p, ".", t, " ~ c(", paste0(DM.prefix, groupnames, ".", style$relationship, ".", p, ".", t, collapse=","), ")*1\n", sep="")}
		}
	}
	
	
	# new labels for the variances
	DM <- paste(DM, paste("\n# Variances\n"))
	
	DM <- paste(DM, paste(style$familyeffect, " ~~ c(", paste0(DM.var, groupnames, ".", style$familyeffect, collapse=","), ")*", style$familyeffect, ifelse(drop=="family", paste0(" + c(0, 0)*", style$familyeffect), ""),"\n", sep=""))
	
	for (p in roles) {DM <- paste(DM, style$actor, ".", p, " ~~ c(", paste0(DM.var, groupnames, ".", style$actor, ".", p, collapse=","), ")*", style$actor, ".", p,  ifelse(drop=="actor", paste0(" + c(0, 0)*", style$actor, ".", p), ""),"\n", sep="")}
	for (p in roles) {DM <- paste(DM, style$partner, ".", p, " ~~ c(", paste0(DM.var, groupnames, ".", style$partner, ".", p, collapse=","), ")*", style$partner, ".", p, ifelse(drop=="partner", paste0(" + c(0, 0)*", style$partner, ".", p), ""),"\n", sep="")}

	for (p in roles) {
		for (t in roles) {
			if (p != t) {DM <- paste(DM, style$relationship, ".", p, ".", t, " ~~ c(", paste0(DM.var, groupnames, ".", style$relationship, ".", p, ".", t, collapse=","), ")*", style$relationship, ".", p, ".", t, "\n", sep="")}
		}
	}
	
	DM <- paste(DM, "\n\n# set means of observed variables to zero\n")
	DM <- paste(DM, paste(pasteNS(roles, roles, var.id), "~ 0", collapse="\n"))
	
	# ---------------------------------------------------------------------
	# Constraints for identifiability
	DM <- paste(DM, "\n\n# set constraints on means for identifiability\n")
	
	 DM <- paste(DM, paste(paste(DM.prefix, groupnames[1], ".", style$actor, ".", roles, sep="", collapse=" + "), "== 0\n"))
	 DM <- paste(DM, paste(paste(DM.prefix, groupnames[1], ".", style$actor, ".", roles, sep="", collapse=" + "), "== 0\n"))
	 
	 for (p in roles) {
	 	DM <- paste(DM, paste(paste(DM.prefix, groupnames[1], ".", style$relationship, ".", p, ".", roles[roles != p], sep="", collapse=" + "), "== 0\n"))
	 }
	 for (p in roles) {
	 	DM <- paste(DM, paste(paste(DM.prefix, groupnames[1], ".", style$relationship, ".", roles[roles != p], ".", p, sep="", collapse=" + "), "== 0\n"))
	 }

	 DM <- paste(DM, paste(paste(DM.prefix, groupnames[2], ".", style$actor, ".", roles, sep="", collapse=" + "), "== 0\n"))
	 DM <- paste(DM, paste(paste(DM.prefix, groupnames[2], ".", style$actor, ".", roles, sep="", collapse=" + "), "== 0\n"))
	 
	 for (p in roles) {
	 	DM <- paste(DM, paste(paste(DM.prefix, groupnames[2], ".", style$relationship, ".", p, ".", roles[roles != p], sep="", collapse=" + "), "== 0\n"))
	 }
	 for (p in roles) {
	 	DM <- paste(DM, paste(paste(DM.prefix, groupnames[2], ".", style$relationship, ".", roles[roles != p], ".", p, sep="", collapse=" + "), "== 0\n"))
	 }
	 
	 # ---------------------------------------------------------------------
	 # Defined parameters
	 
	 DM <- paste(DM, "\n\n# Defined parameters for group comparison (means)\n")

	 DM <- paste(DM, ".meanDiff.", style$familyeffect, " := ", DM.prefix, groupnames[1], ".", style$familyeffect, " - ", DM.prefix, groupnames[2], ".", style$familyeffect, "\n", sep="")
	 
		for (p in roles) {DM <- paste(DM, ".meanDiff.", style$actor, ".", p, " := ", DM.prefix, groupnames[1], ".", style$actor, ".", p, " - ", DM.prefix, groupnames[2], ".", style$actor, ".", p, "\n", sep="")}
		for (p in roles) {DM <- paste(DM, ".meanDiff.", style$partner, ".", p, " := ", DM.prefix, groupnames[1], ".", style$partner, ".", p, " - ", DM.prefix, groupnames[2], ".", style$partner, ".", p, "\n", sep="")}		
		
 	for (p in roles) {
 		for (t in roles) {
 			if (p != t) {DM <- paste(DM, ".meanDiff.", style$relationship, ".", p, ".", t, " := ", DM.prefix, groupnames[1], ".", style$relationship, ".", p, ".", t, " - ", DM.prefix, groupnames[2], ".", style$relationship, ".", p, ".", t, "\n", sep="")}
 		}
 	}


	 DM <- paste(DM, "\n\n# Defined parameters for group comparison (variances)\n")

	 if (drop != "family") {
		 DM <- paste(DM, ".varDiff.", style$familyeffect, " := ", DM.var, groupnames[1], ".", style$familyeffect, " - ", DM.var, groupnames[2], ".", style$familyeffect, "\n", sep="")
	 }
 
 	if (drop != "actor") {
		for (p in roles) {DM <- paste(DM, ".varDiff.", style$actor, ".", p, " := ", DM.var, groupnames[1], ".", style$actor, ".", p, " - ", DM.var, groupnames[2], ".", style$actor, ".", p, "\n", sep="")}
	}
	if (drop != "partner") {
		for (p in roles) {DM <- paste(DM, ".varDiff.", style$partner, ".", p, " := ", DM.var, groupnames[1], ".", style$partner, ".", p, " - ", DM.var, groupnames[2], ".", style$partner, ".", p, "\n", sep="")}		
	}
	
		for (p in roles) {
			for (t in roles) {
				if (p != t) {DM <- paste(DM, ".varDiff.", style$relationship, ".", p, ".", t, " := ", DM.var, groupnames[1], ".", style$relationship, ".", p, ".", t, " - ", DM.var, groupnames[2], ".", style$relationship, ".", p, ".", t, "\n", sep="")}
			}
		}
	
}





## ======================================================================
## Label variances for easy retrieval
## ======================================================================

LAB <- "# Define labels for variances\n"
VAR.prefix <- ".VAR."
LAB <- paste(LAB, paste(style$familyeffect, " ~ ", VAR.prefix, style$familyeffect, "*", style$familyeffect, "\n", sep=""))

for (p in roles) {LAB <- paste(LAB, style$actor, ".", p, " ~ ", VAR.prefix, style$actor, ".", p, "*", style$actor, ".", p, "\n", sep="")}
for (p in roles) {LAB <- paste(LAB, style$partner, ".", p, " ~ ", VAR.prefix, style$partner, ".", p, "*", style$partner, ".", p, "\n", sep="")}

for (p in roles) {
	for (t in roles) {
		if (p != t) {LAB <- paste(LAB, style$relationship, ".", p, ".", t, " ~ ", VAR.prefix, style$relationship, ".", p, ".", t, "*", style$relationship, ".", p, ".", t, "\n", sep="")}
	}
}


## ======================================================================
## Set variance of dropped factors to zero
## Dropping has to be different when diff = TRUE
## ======================================================================

DROP <- ""

if (diff == FALSE) {
	if (!drop %in% c("nothing", "GR")) DROP <- "# Dropping factors:\n##################\n"
	if (drop == "family") {
		DROP <- paste(DROP, paste0(style$familyeffect, " ~~ 0*", style$familyeffect, "\n"))
	}
	if (drop == "actor") {
		for (p in roles) {
			DROP <- paste(DROP, style$actor, ".", p, " ~~ 0*", style$actor, ".", p, "\n", sep="")
		}
		for (p in roles) {
			DROP <- paste(DROP, style$actor, ".", p, " ~~ 0*", style$partner, ".", p, "\n", sep="")
		}
	}
	if (drop == "partner") {
		for (p in roles) {
			DROP <- paste(DROP, style$partner, ".", p, " ~~ 0*", style$partner, ".", p, "\n", sep="")
		}
		for (p in roles) {
			DROP <- paste(DROP, style$actor, ".", p, " ~~ 0*", style$partner, ".", p, "\n", sep="")
		}
	}
}

## ======================================================================
## Put everything together
## ======================================================================
	
	## Add internal information about variable names etc.
	SRM <- "### lavaan syntax for family SRM\n"
	SRM <- paste(SRM, "### ROLES:'", paste(roles, collapse="','"), "'\n", sep="")
	SRM <- paste(SRM, "### VARID:'", paste(var.id, collapse="','"), "'\n", sep="")
	
	SRM <- paste(SRM, FE, sep="\n")
	SRM <- paste(SRM, AE, sep="\n")
	SRM <- paste(SRM, PE, sep="\n")
	SRM <- paste(SRM, RE, sep="\n")
	
	if (!drop %in% c("actor", "partner", "GR"))
		SRM <- paste(SRM, GR, sep="\n")
	
	SRM <- paste(SRM, DR, sep="\n")
	if (length(IGSIM) > 0) {SRM <- paste(SRM, igsim, sep="\n")}
	SRM <- paste(SRM, DROP, sep="\n")
	SRM <- paste(SRM, ERR, sep="\n")
			
	if (self == TRUE) {SRM <- paste(SRM, SELF, sep="\n")}
	if (addv!="") SRM <- paste(SRM, addv)
	SRM <- paste(SRM, SM)
	SRM <- paste(SRM, DM)
	return(SRM)
}


# cat(buildSRMSyntaxLatent(c("m", "f", "o", "y"), c("dep1", "dep2")))
# cat(buildSRMSyntaxLatent(c("m", "f", "o", "y"), c("dep1", "dep2"), means=TRUE, err="no"))
# cat(buildSRMSyntaxLatent(c("m", "f", "c"), c("dep1", "dep2"), means=TRUE, fe=FALSE))
# cat(buildSRMSyntaxLatent(c("m", "f", "c"), c("dep1", "dep2"), means=TRUE, fe=FALSE))

#cat(buildSRMSyntaxLatent(c("m", "f", "c"), c("dep1"), fe=FALSE, self=TRUE))
