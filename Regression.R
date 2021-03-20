rm(list=ls())

# Fitting regularized regression models to Life Insurance mortality data
# What regression model? 
    #Logistic for classification (claim, no claim)
# Why regularization?
    #To reduce overfitting
    #To increase interpretability (LASSO)
    #To address collinearity


#load package
library(glmnet)

##### Read in Data #####
life_insurance_data<-data.table:: fread("C:/Users/Admin/Documents/R_projects/ml_demographics/Dataset/Life_Insurance_data.csv")
life_insurance_data<-as_tibble(life_insurance_data)#%>%
  #mutate(claim_indi=ifelse(claim=="N",0,1))

# Lets look at the data
glimpse(life_insurance_data)
str(life_insurance_data)
summary(life_insurance_data)

# Remove some unnecessary columns
life_insurance_data<-life_insurance_data[,-c(1,6)]


x=model.matrix(claim ~.,life_insurance_data )
# model.matrix automatically transforms qualitative variables into dummy variables
y=life_insurance_data$claim

##### Split data into training and test sets #####
set.seed(297)
train<-sample (1: nrow(x), floor(nrow(x)*0.8))
test<-(-train)
y_test<-y[test]

##### Perform Ridge Regression #####

# Use cross validation to choose lambda
# glmnet package defaults to 10 fold cv,but this can be altered using nfolds parameter
# by default, glmnet standardises the variables so that they are on the same scale

set.seed(1)
cv_ridge<-cv.glmnet(x[train,],y[train],alpha=0,family="binomial")
plot(cv_ridge)
bestlam_ridge =cv_ridge$lambda.min
bestlam_ridge

# Fit ridge regression model on training data set using lambda chosen by cv and examine coefficients
model_ridge<-glmnet(x[train,],y[train],alpha=0,lambda=bestlam_ridge,family="binomial")
# Examine coefficients
predict(model_ridge,type="coefficients",s=bestlam_ridge)

# as we can see, none of the coefficients are zero since Ridge regression does not perform variable selection

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
