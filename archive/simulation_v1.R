# Generate dataset with specific correlations
install.packages("MASS")
library(MASS)

#using mvrnorm to create columns (transformed) which have a specific correlations with one another
set.seed(197)

cor_ab<-0.6
cor_ac<-0.3
cor_bc<--0.1

sdev_a<-1
sdev_b<-1
sdev_c<-1

cov_ab<-cor_ab*sdev_a*sdev_b
cov_ac<-cor_ac*sdev_a*sdev_c
cov_bc<-cor_bc*sdev_b*sdev_c

mean_a<-0
mean_b<-0
mean_c<-0

covm<-cbind(c(sdev_a^2,cov_ab,cov_ac),c(cov_ab,sdev_b^2,cov_bc),c(cov_ac,cov_bc,sdev_c^2))
covm


out <- as.data.frame(mvrnorm(10000, mu = c(mean_a,mean_b,mean_c), 
                             Sigma =covm, 
                             empirical = TRUE))

out$V1.s <- (out$V1 - min(out$V1))*1000+10
out$V2.s <- out$V2 
out$V3.s <- out$V3

mean(out$V2.s)
min(out$V2.s)
max(out$V2.s)
hist(out$V2.s)

mean(out$V3.s)
min(out$V3.s)
max(out$V3.s)
hist(out$V3.s)

cor(out$V1.s, out$V2.s)
cor(out$V1.s, out$V3.s)
cor(out$V2.s, out$V3.s)
