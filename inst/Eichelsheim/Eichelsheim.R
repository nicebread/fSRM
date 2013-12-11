# ---------------------------------------------------------------------
# Run analyses based on Eichelsheim, V. I., Buist, K. L., Deković, M., Cook, W. L., Manders, W., Branje, S. J. T., Frijns, T., et al. (2011). Negativity in problematic and nonproblematic families: A multigroup social relations model analysis with structured means. Journal of Family Psychology, 25, 152–156. doi:10.1037/a0022450

# The data set 

data(negInt)
str(negInt)

E1 <- fSRM(neg1/neg2 ~ actor.id*partner.id | family.id, data=negInt)
E1

E2 <- fSRM(neg1/neg2 ~ actor.id*partner.id | family.id, data=negInt, group="group")
E2

# Beware: This model takes *really* long ...
E3 <- fSRM(neg1/neg2 ~ actor.id*partner.id | family.id, data=negInt, group="group", means=TRUE, diff=TRUE)
E3
