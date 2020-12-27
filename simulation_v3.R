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