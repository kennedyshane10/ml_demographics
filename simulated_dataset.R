##### Group Disability portfolio details #####

# Disability definitions may be own or any occupation, multiple of salary or fixed sum assured, 
# may include coverage for dependents, some schemes optional some compulsory

# Separate rate tables for above (e.g. own/any occ) but one ML model can be fitted for both

# 1 year annually renewable

# Variables include scheme optional or compulsory, age, gender, occupation scale 1 - blue or white collar, salary, sum assured, 
# years since joining scheme, years since joining company, work location, legal entity, previous claim y or n, 
# smoker status, dependents y or n, marital status, underwriting performed (health q), 
# underwriting loading applied

#Categorical:
# Optional or compulsory, gender, work location - two locations, legal entity, previous claim, smoker status,
# dependents, marital status, u/w performed
#Ordinal:
# occupation scale 1-4
#Numerical:
#age, salary, sum assured, years since joining scheme, years since joining company, u/w loading applied

# Number of observations around 100k lives, around 5 years claims history

##### Simulate dataset using copula package #####
library(copula)
set.seed(100)
myCop <- normalCopula(param=c(0.6,0.0,0.3,0.7,0.9,0.7,-0.2, #age
                              -0.4,0.1,0.2,0.3,0.7,-0.8, #sa
                              0.1,0.1,0.0,-0.9,0.9, #gender
                              0.05,0.35,-0.9,-0.2, #martial
                              0.2,-0.8,0.95, #claim
                              0.05,-0.9, # yrs since joining scheme
                              -0.9), #occ class
                      dim = 8, dispstr = "un")
myMvd <- mvdc(copula=myCop, margins=c("gamma", "lnorm", "binom", "binom","binom","lnorm","binom","binom"),
              paramMargins=list(list(shape=50, scale=0.8),
                                list(meanlog=11,sdlog=0.7), 
                                list(size = 1, prob = 0.6), # % male
                                list(size = 1, prob = 0.2), # % married
                                list(size = 1, prob = 0.15), # % claims
                                list(meanlog=1,sdlog=0.4),
                                list(size = 1, prob = 0.3), #% white collar
                                list(size = 1, prob = 0.7))) # % in location 1 (distribution center)
Z2 <- rMvdc(10000, myMvd)
colnames(Z2) <- c("age", "sum.assured", "gender","martial.status","claim.lastyr",
                  "yrs.IF","occ.class","location")
mcor<-round(cor(Z2),2)
# Hide upper triangle
upper<-mcor
upper[upper.tri(mcor)]<-""
upper<-as.data.frame(upper)
upper

pairs.panels(Z2)

#occupation classes - for now, not specifying any correlation with other variables
# can investigate doing this in future (e.g. using copula approach above, using normal distn, then binning into 4
# categories)
#set.seed(293)
#sampleDist = function(n) { 
#  sample(x = c(1,2,3,4), n, replace = T, prob = c(0.1, 0.4, 0.2, 0.3)) 
#}

#occupation.classes<-sampleDist(10000)

#df<-cbind(Z2,occupation.classes)
#pairs.panels(df)











##### Simulate dataset using other method #####
library(MASS)

set.seed(645)
# http://www.econometricsbysimulation.com/2014/02/easily-generate-correlated-variables.html?m=1
# We will use the command mvrnorm to draw a matrix of variables

# Let's keep it simple, 


#this is the covariance matrix. can work back to get the correlation using 
# following formula: 
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

mu <- cbind(mean_a,mean_b,mean_c)

Sigma<-cbind(c(sdev_a^2,cov_ab,cov_ac),c(cov_ab,sdev_b^2,cov_bc),c(cov_ac,cov_bc,sdev_c^2))

# Sigma <- matrix(.6, nrow=2, ncol=2) + diag(2)*.4
# Sigma <- as.matrix(cbind(c(1,0.5,0.3),c(0.5,1,0.4),c(0.3,0.4,1)))

rawvars <- mvrnorm(n=10000, mu=mu, Sigma=Sigma)

cov(rawvars); 
cor(rawvars)
# We can see our normal sample produces results very similar to our 
# specified covariance levels.

# Now lets transform some variables
pvars <- pnorm(rawvars)

# Through this process we already have 
cov(pvars)
cor(pvars)
# We can see that while the covariances have dropped significantly, 
# the correlations are largely the same.
#  Let's see what happens when we invert different CDFs.

# Binomial distribution 

binomvars <- qbinom(pvars, 1, .25) 
# Note, I did 1-p because p is defined differently for the qpois for some 
#reason
cor(binomvars)



male_prop <- 0.6
smok_prop<- 0.10
maritial_prop<-0.3

install.packages("Rlab")
library(Rlab)
gender <- qbern(pvars[,1], male_prop) 
smoker <- qbern(pvars[,2], smok_prop) 
married <- qbern(pvars[,3], maritial_prop) 
combined<-cbind(gender,smoker,married)
cor(combined)

# To make things a little more interesting, let's now transform our probabilities
# into gamma distribution to model age and lognormal to model sumassumred
# notice that pvars[,1] is used for age, pvars[,2] for sumassured. They will therefore
# have correlations close to that defined by Sigma at the start i.e. cor(pvars)
# we can include more continuous distributions by expanding the matrix at the stgart defined by Sigma
age <- qgamma(pvars[,1],shape=50,scale=0.8)
sumassured <- qlnorm(pvars[,2],meanlog=11,sdlog=0.7)

# Finally in order to demonstrate what we can do let's combine our variables into
# a single matrix.

combvar <- data.frame(gender, smoker, married, age, sumassured)

mcor<-round(cor(combvar),2)

# Hide upper triangle
upper<-mcor
upper[upper.tri(mcor)]<-""
upper<-as.data.frame(upper)
upper
