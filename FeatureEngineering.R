library(standardize)
# https://cran.r-project.org/web/packages/standardize/vignettes/using-standardize.html

# Standardize age, sum assured, salary
# what standardization is appropriate(z standardize, between 0 and 1?)
#scheme.data.formatted$age.scaled <- scale(scheme.data.formatted$age)[, 1]
#scheme.data.formatted$sum.assured.scaled <- scale(scheme.data.formatted$sum.assured)[, 1]
#scheme.data.formatted$salary.scaled <- scale(scheme.data.formatted$salary)[, 1]

# What to do with other variables occupation, location etc.?

scheme.data.formatted2<-select(scheme.data.formatted,age,gender,sum.assured,claim)

scheme.data.formatted2.scaled<-scale(scheme.data.formatted2)
head(scheme.data.formatted2.scaled)

corPlot(scheme.data.formatted2.scaled)
