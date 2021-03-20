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
life_insurance_data<-as_tibble(life_insurance_data)%>%
  mutate(claim_indi=ifelse(claim=="N",0,1))
life_insurance_data<-life_insurance_data[,-1]

head(life_insurance_data)

x=model.matrix(claim_indi ~.,life_insurance_data )
# model.matrix automatically transforms qualitative variables into dummy variables
y=life_insurance_data$claim_indi

##### Split data into training and test sets #####
set.seed(297)
train=sample (1: nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

##### Perform Ridge Regression #####

# Use cross validation to choose lambda
# glmnet package defaults to 10 fold cv,but this can be altered using nfolds parameter
# by default, glmnet standardises the variables so that they are on the same scale
set.seed(1)
cv.out=cv.glmnet(x[train ,],y[ train],alpha=0,family="binomial")
plot(cv.out)
bestlam =cv.out$lambda.min
bestlam

# Fit ridge regression model on training data set using lambda chosen by cv and examine coefficients
out=glmnet(x[train ,],y[ train],alpha=0,family="binomial")
# Examine coefficients
predict(out,type="coefficients",s= bestlam) [1:20,]

# as we can see, none of the coefficients are zero since Ridge regression does not perform variable selection

##### Perform LASSO #####

set.seed(1)
cv.out=cv.glmnet(x[train ,],y[ train],alpha=1,family="binomial")
plot(cv.out)
bestlam =cv.out$lambda.min
bestlam


