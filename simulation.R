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

##### Simulate dataset using other method - allows categorical variables #####

# https://www.r-bloggers.com/2020/09/how-to-convert-continuous-variables-into-categorical-by-creating-bins/
# http://www.econometricsbysimulation.com/2014/02/easily-generate-correlated-variables.html?m=1

library(MASS)
set.seed(100)

cor_ab<-0.6
cor_ac<-0.3
cor_ad<-0.1
cor_ae<-0.8
cor_af<-0.41
cor_bc<--0.1
cor_bd<--0.2
cor_be<-0.3
cor_bf<-0.13
cor_cd<-0.4
cor_ce<-0.65
cor_cf<-0.19
cor_de<-0.46
cor_df<-0.24
cor_ef<-0.17

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
covm

# 1. Draw any number of variables from a joint normal distribution.
z <- as.data.frame(mvrnorm(10000, mu = c(mean_a,mean_b,mean_c,mean_d,mean_e,mean_f), 
                           Sigma =covm, 
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
sum.assured <- qlnorm(pvars.V2,meanlog=11,sdlog=0.7)

#binning normal distribution into categorical variables so not applying inverse to these
x3 <- pvars.V3
x4 <- pvars.V4
x5 <- pvars.V5
x6 <- pvars.V6
df <- as.data.frame(cbind(x1,x2,x3,x4,x5,x6))

df<-df%>%mutate(gender=ifelse(df$x3 <=0.6,0,1)) #just two bins

# cumulative probabilities for each bin - three bins here
df<-df%>%mutate(occ.class=cut(df$x4, breaks = c(0,0.2,0.65,1)))
df<-df%>%mutate(occ.class=ifelse(df$occ.class =='(0,0.2]',1,ifelse(df$occ.class =='(0.2,0.65]',2,3)))

df2<-as.data.frame(cbind(df$x1,age,df$x2,sum.assured,df$x3,df$gender,df$x4,df$occ.class,df$x5,df$x6))%>%
  rename(x1=V1,
         x2=V3,
         x3=V5,
         gender=V6,
         x4=V7,
         occ.class=V8,
         x5=V9,
         x6=V10)

mcor<-round(cor(df2),2)

lower<-mcor
lower[lower.tri(mcor)]<-""
lower<-as.data.frame(lower)
lower

# we see that the correlations work better when we have more categories since 
# this becomes more like a continuous distribution
# gender is based on x3 but it only has a correlation of 0.85 with x3 since it has two categories
# occ class is based on x4 and has a correlation of 0.92 with x4 since it has three categories

df3<-select(df2,age,sum.assured,gender,occ.class)

mcor<-round(cor(df3),2)

lower<-mcor
lower[lower.tri(mcor)]<-""
lower<-as.data.frame(lower)
lower

pairs.panels(df3)

