# ---------------------------------------------------------------------
# Run analyses based on Eichelsheim, V. I., Buist, K. L., Deković, M., Cook, W. L., Manders, W., Branje, S. J. T., Frijns, T., et al. (2011). Negativity in problematic and nonproblematic families: A multigroup social relations model analysis with structured means. Journal of Family Psychology, 25, 152-156. doi:10.1037/a0022450

# The data set is a simulated data set which has comparable properties as the original data set

data(two.groups)
str(two.groups)

E1 <- fSRM(neg1/neg2 ~ actor.id*partner.id | family.id, data=two.groups)
E1
plot(E1)

# make group comparison: group = 1: non-problematic families, group = 2: problematic families
E2 <- fSRM(neg1/neg2 ~ actor.id*partner.id | family.id, data=two.groups, group="group")
E2
plot(E2)

E2b <- fSRM(neg1/neg2 ~ actor.id*partner.id | family.id, data=two.groups, group="group", noNegVar=TRUE)
E2b


data(two.groups)
E2 <- fSRM(neg1 ~ actor.id*partner.id | family.id, data=two.groups, group="group", diff=TRUE, noNegVar=TRUE)
E2

E2 <- fSRM(neg1 ~ actor.id*partner.id | family.id, data=two.groups, group="group", noNegVar=TRUE)
E2


# Compare means and differences between groups. Beware: This model takes quite long ...
E3 <- fSRM(neg1/neg2 ~ actor.id*partner.id | family.id, data=two.groups, group="group", means=TRUE, diff=TRUE)
E3



data(two.groups)
clinical <- subset(two.groups, group==1)
clinical[,7] <- rowMeans(clinical[,c(5,6)])
colnames(clinical) <- c("family.id","actor.id","partner.id","group","neg1","neg2","neg")
clinical <- clinical[,-c(4:6)]

fSRM.4.1 <- fSRM(neg ~ actor.id*partner.id | family.id, data = clinical)
syntax(fSRM.4.1)