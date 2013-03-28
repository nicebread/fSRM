## residual check

# returns a data frame of two variables which correlate
# If desired, one of both variables can be fixed to an existing variable by specifying x
getBiCop <- function(n, rho, mar.fun=rnorm, x = NULL, side=NULL, ...) {
     if (!is.null(x)) {X1 <- x} else {X1 <- mar.fun(n, ...)}
     if (!is.null(x) & length(x) != n) warning("Variable x does not have the same length as n!")

     C <- matrix(rho, nrow = 2, ncol = 2)
     diag(C) <- 1

     C <- chol(C)

	if (is.null(side)) {X2 <- mar.fun(n)} 
	else {X2 <- mar.fun(n, side=ifelse(side=="opposite", -1, 1))}
	
     X <- cbind(X1,X2)

     # induce correlation (does not change X1)
     df <- X %*% C

     ## if desired: check results
     #all.equal(X1,X[,1])
     #cor(X)

     return(df)
}


df <- getBiCop(1000, .5)
df2 <- data.frame(df)
colnames(df2) <- c("x", "y")
df3 <- getBiCop(1000, .71, x=df2$y)
df2$z <- df3[, 2]


# sample variances
var(df2)
sum(diag(var(df2)))

library(psych)
(FA <- fa(df2, 1))

library(lavaan)

s1 <- sem(model="F =~ x + y + z", data=df2, fixed.x=FALSE)
summary(s1)
standardizedSolution(s1, type="std.all")

#Perhaps you mean the variance contributed by each factor to the observed variable variances. If so, you sum the squared factor loadings from an orthogonal (factors uncorrelated) solution and divide by the number of observed factor indicators. 
(.563^2 + .96^2 + .748^2) / 3

FA



#------------------------------------------------------------
# ----  Branje Reanalysis --------------------------------------
#------------------------------------------------------------

B <- read.delim("BranjeE.tab", header=TRUE)
rownames(B) <- colnames(B)
B <- as.matrix(B)

roles <- c("v", "m", "o", "j")
var.id <- c("ex1", "ex2")
model2 <- buildSRMSyntaxLatent(roles, var.id=var.id, err=1)

m <- sem(model2, sample.cov=B, sample.nobs=268, orthogonal=TRUE)

fe <- TRUE
IGSIM <- list()
res <- list(model=m, syntax=model2, roles=roles, var.id=var.id, fe=fe, IGSIM=IGSIM)
attr(res, "class") <- "fSRM"

percTable.latent(res)

# Branje's estimate of F-->M unexplained variance: .64
0.48283 + 0.16074	# from the theta matrix of the output: sum of residual variances of both indicators
# But it should be the average!!