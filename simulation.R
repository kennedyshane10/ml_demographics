##### Group Risk Scheme #####

# Scenario:
# Scheme is currently insured for Death only benefit = 2 x Salary
# Just one year's worth of claims experience available
# Last years membership details and claims (anonymised) is provided
# Data is limited to Age, Sum Assured, Salary, Gender, Occupation and Work Location - check what we can have under GDPR
# We wish to perform some experience analysis utilizing ML techniques
# Results can be used to supplement pricing and reserving mortality rates

#Steps:
# 1. Simulate Dataset
# 2. What is it that we want to achieve? Prediction, Inference, ...?
# 3. Perform Exploratory Analysis - understand the dataset
# 4. 

##### Simulate dataset #####

# https://www.r-bloggers.com/2020/09/how-to-convert-continuous-variables-into-categorical-by-creating-bins/
# http://www.econometricsbysimulation.com/2014/02/easily-generate-correlated-variables.html?m=1

library(MASS)
library(matrixcalc)
library(dplyr)
library(corpcor)

rm(list=ls())
set.seed(180)

# a = age
# b = sum.assured
# c = gender
# d = occupation
# e = claim last year
# f = work location

no.records<-10000

adj=1

cor_ab<-0.6/adj   # age & sum.assured
cor_ac<--0.3/adj  # age & gender
cor_ad<-0.1/adj   # age & occ
cor_ae<-0.8/adj   # age & claim
cor_af<--0.8/adj  # age & location

cor_bc<--0.1/adj  # sum.assured & gender 
cor_bd<--0.6/adj  # sum.assured & occ
cor_be<--0.3/adj  # sum.assured & claim
cor_bf<--0.8/adj  # sum.assured & location

cor_cd<-0.4/adj   # gender & occ
cor_ce<--0.65/adj # gender & claim
cor_cf<--0.8/adj  # gender & location

cor_de<-0.46/adj  # occ & claim
cor_df<-0.8/adj   # occ & location

cor_ef<-0.9/adj  # claim & location

sdev_a<-1
sdev_b<-1
sdev_c<-1
sdev_d<-1
sdev_e<-1
sdev_f<-1

cov_ab<-cor_ab*sdev_a*sdev_b
cov_ac<-cor_ac*sdev_a*sdev_c
cov_ad<-cor_ad*sdev_a*sdev_d
cov_ae<-cor_ae*sdev_a*sdev_e
cov_af<-cor_af*sdev_a*sdev_f

cov_bc<-cor_bc*sdev_b*sdev_c
cov_bd<-cor_bd*sdev_b*sdev_d
cov_be<-cor_be*sdev_b*sdev_e
cov_bf<-cor_bf*sdev_b*sdev_f

cov_cd<-cor_cd*sdev_c*sdev_d
cov_ce<-cor_ce*sdev_c*sdev_e
cov_cf<-cor_cf*sdev_c*sdev_f

cov_de<-cor_de*sdev_d*sdev_e
cov_df<-cor_df*sdev_d*sdev_f

cov_ef<-cor_ef*sdev_e*sdev_f

mean_a<-0
mean_b<-0
mean_c<-0
mean_d<-0
mean_e<-0
mean_f<-0

covm<-cbind(c(sdev_a^2,cov_ab,cov_ac,cov_ad,cov_ae,cov_af),
            c(cov_ab,sdev_b^2,cov_bc,cov_bd,cov_be,cov_bf),
            c(cov_ac,cov_bc,sdev_c^2,cov_cd,cov_ce,cov_cf),
            c(cov_ad,cov_bd,cov_cd,sdev_d^2,cov_de,cov_df),
            c(cov_ae,cov_be,cov_ce,cov_de,sdev_e^2,cov_ef),
            c(cov_af,cov_bf,cov_cf,cov_df,cov_ef,sdev_f^2))

if (is.positive.definite(covm)==TRUE) {
  covm_use=covm
} else {
  covm_use=make.positive.definite(covm)
}

# 1. Draw any number of variables from a joint normal distribution.
z <- as.data.frame(mvrnorm(no.records, mu = c(mean_a,mean_b,mean_c,mean_d,mean_e,mean_f), 
                           Sigma=covm_use, 
                           empirical = TRUE))

library(psych)
round(cor(z,method='spearman'),2)

# 2. Apply the univariate normal CDF of variables to derive probabilities for each variable.
pvars.V1 <- pnorm(z$V1)
pvars.V2 <- pnorm(z$V2)
pvars.V3 <- pnorm(z$V3)
pvars.V4 <- pnorm(z$V4)
pvars.V5 <- pnorm(z$V5)
pvars.V6 <- pnorm(z$V6)

# 3. Finally apply the inverse CDF of any distribution to simulate draws from that distribution.
x1<-pvars.V1
age <- qgamma(pvars.V1,shape=50,scale=0.8)

x2<-pvars.V2
sum.assured <- round(qlnorm(pvars.V2,meanlog=11,sdlog=0.7),-3)
salary <- sum.assured/2 # sum assured is a multiple of salary 

#binning normal distribution into categorical variables so not applying inverse to these
x3 <- pvars.V3
x4 <- pvars.V4
x5 <- pvars.V5
x6 <- pvars.V6
df <- as.data.frame(cbind(x1,x2,x3,x4,x5,x6))

df<-df%>%mutate(gender=ifelse(df$x3 <=0.6,0,1)) #just two bins, male is 0 / female is 1

# cumulative probabilities for each bin - 12 bins here
df<-df%>%mutate(occ=cut(df$x4, breaks = c(0,0.2,0.25,0.3,0.35,0.4,0.5,0.6,0.65,0.72,0.75,0.77,1)))
df<-df%>%mutate(occ=ifelse(df$occ =='(0,0.2]',1,
                           ifelse(df$occ =='(0.2,0.25]',2,
                                  ifelse(df$occ =='(0.25,0.3]',3,
                                         ifelse(df$occ =='(0.3,0.35]',4,
                                                ifelse(df$occ =='(0.35,0.4]',5,
                                                       ifelse(df$occ =='(0.4,0.5]',6,
                                                              ifelse(df$occ =='(0.5,0.6]',7,
                                                                     ifelse(df$occ =='(0.6,0.65]',8,
                                                                            ifelse(df$occ =='(0.65,0.72]',9,
                                                                                   ifelse(df$occ =='(0.72,0.75]',10,
                                                                                          ifelse(df$occ =='(0.75,0.77]',11,
                            
                              
                                  12))))))))))))

df<-df%>%mutate(claim=ifelse(df$x5 <=(1-0.002),0,1)) #just two bins, no claim last year is 0 / yes is 1

# cumulative probabilities for each bin - three bins here
df<-df%>%mutate(location=cut(df$x6, breaks = c(0,0.15,0.65,1)))
df<-df%>%mutate(location=ifelse(df$location =='(0,0.15]',1,ifelse(df$location =='(0.15,0.65]',2,3)))

df2<-as.data.frame(cbind(df$x1,age,df$x2,sum.assured,salary,df$x3,df$gender,df$x4,df$occ,df$x5,df$claim,df$x6,df$location))%>%
  rename(x1=V1,
         x2=V3,
         x3=V6,
         gender=V7,
         x4=V8,
         occ=V9,
         x5=V10,
         claim=V11,
         x6=V12,
         location=V13)

mcor<-round(cor(df2),2)

lower<-mcor
lower[lower.tri(mcor)]<-""
lower<-as.data.frame(lower)
lower

# we see that the correlations work better when we have more categories since 
# this becomes more like a continuous distribution
# gender is based on x3 but it only has a correlation of 0.85 with x3 since it has two categories
# occ class is based on x4 and has a correlation of 0.92 with x4 since it has three categories

df3<-select(df2,age,sum.assured,salary,gender,occ,claim,location)

mcor<-round(cor(df3),2)

lower<-mcor
lower[lower.tri(mcor)]<-""
lower<-as.data.frame(lower)
lower

# changing 1/0 indicators to more meaningful indicators
df4<-df3%>%mutate(gender=ifelse(df3$gender ==0,"M","F"),
                  location=ifelse(df3$location ==1,"site1",ifelse(df3$location ==2,"site2","site3")))

df5<-df4%>%mutate(
  occ.desc=case_when(
    occ==1~"job1",
    occ==2~"job2",
    occ==3~"job3",
    occ==4~"job4",
    occ==5~"job5",
    occ==6~"job6",
    occ==7~"job7",
    occ==8~"job8",
    occ==9~"job9",
    occ==10~"job10",
    occ==11~"job11",
    occ==12~"job12"
  )
)

df6<-df5%>%mutate(
  claim=case_when(
    claim==0~"N",
    claim==1~"Y"
    )
)

#additional info to include having no correlation with other data
#smoker status, marital status, employee identification number
#

#data.to.write<-select(df6,age,sum.assured,salary,gender,occ.desc,location,claim)

write.csv(df6,"C:/Users/Admin/Documents/R_projects/ml_demographics/Dataset/data.csv")
