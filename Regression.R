rm(list=ls())

##### Using Regularized Regression for Life Insurance experience analysis studies #####

# Scenario:
# Scheme is currently insured for Death only benefit = 2 x Salary
# Historic claims experience available up to and including 2020
# Membership details and claims history is provided
# We wish to perform some experience analysis utilizing ML techniques
# Results can be used to supplement pricing and reserving mortality assumptions
# This script creates a synthetic data set

# https://www.r-bloggers.com/2020/09/how-to-convert-continuous-variables-into-categorical-by-creating-bins/
# http://www.econometricsbysimulation.com/2014/02/easily-generate-correlated-variables.html?m=1


# Fitting regularized regression models to Life Insurance mortality data
# What regression model? 
#Logistic for classification (claim, no claim)
# Why regularization?
#To reduce overfitting
#To increase interpretability (LASSO)
#To address collinearity

##### Simulate data set #####

start_year<-2000 #start year - will simulate from this exposure year up until and including 2020
no.records<-1000 #on start year
annual_membership_growth_perc<-15 #percentage annual membership growth per year
claim_rate<- 50 #per thousand

source("C:/Users/Admin/Documents/R_projects/ml_demographics/Simulate_data.R")

#load package
library(glmnet)

# Read in Data
life_insurance_data<-data.table:: fread("C:/Users/Admin/Documents/R_projects/ml_demographics/Dataset/Life_Insurance_data.csv")
life_insurance_data<-as_tibble(life_insurance_data)#%>%
  #mutate(claim_indi=ifelse(claim=="N",0,1))

# Lets look at the data
glimpse(life_insurance_data)
str(life_insurance_data)
summary(life_insurance_data)

# Remove some unnecessary columns
life_insurance_data<-life_insurance_data[,-c(1,6)]

# Replace characters with numeric for Gender and Claim
# Exclude location and occ.desc for now

data_adjusted<-life_insurance_data[,-c(6,12)]%>%
  mutate(gender=ifelse(life_insurance_data$gender=="M",0,1))%>%
  mutate(claim=ifelse(life_insurance_data$claim=="N",0,1))
  
# Look at correlations 
mcor<-round(cor(data_adjusted),2)

lower<-mcor
lower[lower.tri(mcor)]<-""
lower<-as.data.frame(lower)
lower

x=model.matrix(claim ~.,life_insurance_data )
# model.matrix automatically transforms qualitative variables into dummy variables
y=life_insurance_data$claim

##### Split data into training and test sets #####
set.seed(297)
train<-sample (1: nrow(x), floor(nrow(x)*0.8))
test<-(-train)
y_test<-y[test]

##### Perform LASSO #####

# Use cross validation to choose lambda

set.seed(1)
cv_lasso<-cv.glmnet(x[train,],y[train],alpha=1,family="binomial")
plot(cv_lasso)
bestlam_lasso<-cv_lasso$lambda.min
bestlam_lasso
one_se_lasso<-cv_lasso$lambda.1se

# Fit LASSO regression model on training data set using lambdas chosen by cv and examine coefficients

#min lambda
model_lasso_min<-glmnet(x[train,],y[train],alpha=1,lambda=bestlam_lasso,family="binomial")
# Examine coefficients - min lambda
predict(model_lasso,type="coefficients",s=bestlam_lasso)

# as we can see, some of the coefficients are zero as LASSO performs variable selection
