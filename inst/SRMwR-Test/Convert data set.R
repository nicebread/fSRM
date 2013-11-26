library(foreign)
dat <- read.spss("4person.sav", to.data.frame=TRUE)

# convert to long format
dat2 <- melt(dat, id.vars="id2")
dat2$pid <- substr(dat2$variable, 1, 1)
dat2$tid <- substr(dat2$variable, 2, 2)
dat2$v <- substr(dat2$variable, 3, 6)

dat2 <- dat2[, c(1, 4, 5, 3)]
str(dat2)
colnames(dat2) <- c("family.id", "perceiver.id", "target.id", "anx")
dat2 <- dat2[order(dat2$family, dat2$perceiver.id, dat2$target.id), ]
rownames(dat2) <- NULL
write.table(dat2, file="attanx4.tab", row.names=FALSE, col.names=TRUE)



library(foreign)
library(reshape2)
dat <- read.spss("3person.sav", to.data.frame=TRUE)

# convert to long format
dat$id2 <- 1:nrow(dat)
dat2 <- melt(dat, id.vars="id2")
dat2$pid <- substr(dat2$variable, 1, 1)
dat2$tid <- substr(dat2$variable, 2, 2)
dat2$v <- substr(dat2$variable, 3, 6)
dat3 <- dcast(dat2, id2 + pid + tid ~ v, value.var="value")


colnames(dat3) <- c("family.id", "perceiver.id", "target.id", "anx1", "anx2")
dat3 <- dat3[order(dat3$family.id, dat3$perceiver.id, dat3$target.id), ]
rownames(dat3) <- NULL
write.table(dat3, file="attanx3.tab", row.names=FALSE, col.names=TRUE)
