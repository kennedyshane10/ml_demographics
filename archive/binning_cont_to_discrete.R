##### creating bins #####
# https://www.r-bloggers.com/2020/09/how-to-convert-continuous-variables-into-categorical-by-creating-bins/

library(dplyr)
# Generate 1000 observations from the Norm distribution 

df<-data.frame(MyContinuous = runif(1000, 0, 1))
# get the histogtam
hist(df$MyContinuous)

# two bins e.g. for two categories
df<-df%>%mutate(MySpecificBins = cut(MyContinuous, breaks = c(0,0.6,1)))
head(df,10)

df%>%group_by(MySpecificBins)%>%count()

# four bins, for four categories
df<-df%>%mutate(MySpecificBins = cut(MyContinuous, breaks = c(0,0.2,0.65,0.9,1))) # cumulative probabilities for each bin
head(df,10)

df%>%group_by(MySpecificBins)%>%count()

# quantiles
numbers_of_bins = 4
df<-df%>%mutate(MyQuantileBins = cut(MyContinuous, 
                                     breaks = unique(quantile(MyContinuous,probs=seq.int(0,1, by=1/numbers_of_bins))), 
                                     include.lowest=TRUE))
head(df,10)


df%>%group_by(MyQuantileBins)%>%count()

##### correlations #####

library(MASS)
set.seed(100)

m <- 3
n <- 2000
sigma <- matrix(c(1, 0.4, 0.2,
                  0.4, 1, -0.8,
                  0.2, -0.8, 1), 
                nrow=3)
z <- mvrnorm(n,mu=rep(0, m),Sigma=sigma,empirical=T)

library(psych)
cor(z,method='spearman')
pairs.panels(z)

u <- pnorm(z)
pairs.panels(u)

library(rgl)
plot3d(u[,1],u[,2],u[,3],pch=20,col='navyblue')

x1 <- qgamma(u[,1],shape=2,scale=1)
x2 <- qbeta(u[,2],2,2)
x3 <- u[,3]
plot3d(x1,x2,x3,pch=20,col='blue')


df <- cbind(x1,x2,x3)
pairs.panels(df)
cor(df,meth='spearman')

head(df)
df1<-as.data.frame(df)
df1$gender<- ifelse(df1$x3 <=0.6,0,1)
head(df1)
cor(df1)
pairs(df1)
