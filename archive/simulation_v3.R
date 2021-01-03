
library(GenOrd)
set.seed(1)
# Sets the marginals.
# The values are cumulative so for the first variable the first marginal will be .1, the second is .2, the third is .3, and the fourth is .4
marginal <- list(c(0.1,0.3,0.6),c(0.4,0.7,0.9))
# Checks the lower and upper bounds of the correlation coefficients.
corrcheck(marginal)
# Sets the correlation coefficients
R <- matrix(c(1,-0.6,-0.6,1),2,2) # Correlation matrix
n <- 100
##Selects an ordinal sample with given correlation R and given marginals.
m <- ordsample(n, marginal, R)

##compare it with the pre-defined R
cor(m)
table(m[,1],m[,2])
chisq.test(m)
gbar <- tapply(m[,1], list(m[,1], m[,2]), length)
par(mfrow=c(1,1))
barplot(gbar, beside=T, col=cm.colors(4), main="Example Bar Chart of Counts by Group",xlab="Group",ylab="Frequency")



# https://stackoverflow.com/questions/27361710/generating-correlated-ordinal-data

install.packages("miscor")
library(miscor)

install.packages("GenOrd")
library(GenOrd)

R<-matrix(c(1,0.5,0.5,1),2,2)
Marginal<-list(c(0.2,0.5,0.7,0.9),c(0.1,0.3,0.4,0.5))

set.seed(1234)
n <- 100
correlations <- replicate(n = 1000, expr = cor(ordsample(n, Marginal, R))[1,2])
mean(correlations)