
# https://bookdown.org/rdpeng/exdata/exploratory-data-analysis-checklist.html

# Informal checklist:
# 1. Formulate your question - hypothesis
# 2. Read in your data
# 3. Check the packaging
# 4. Run str()
# 5. Look at the top and the bottom of your data
# 6. Check your “n”s
# 7. Validate with at least one external data source
# 8. Try the easy solution first
# 9. Challenge your solution
# 10. Follow up

# Connect to a local spark cluster
library(sparklyr)
library(dplyr)
sc <- spark_connect(master = "local")

# Read in the data
scheme.data<-read.csv("C:/Users/Admin/Documents/R_projects/ml_demographics/Dataset/data.csv")

# Have a quick look at the data
glimpse(scheme.data)
head(scheme.data)
tail(scheme.data)

# Format the data
scheme.data.formatted<-scheme.data%>%mutate(
  gender=case_when(
    gender=="M"~0,
    gender=="F"~1
  )
)

scheme.data.formatted<-scheme.data.formatted%>%mutate(
  claim=case_when(
    claim=="N"~0,
    claim=="Y"~1
  )
)
scheme.data.formatted<-select(scheme.data.formatted,age,gender,sum.assured,salary,occ,claim)

##### One way Analysis #####

# create mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

summary(scheme.data.formatted)

# Age
hist(scheme.data.formatted$age)
sd(scheme.data.formatted$age)

# Gender
hist(scheme.data.formatted$gender)
sd(scheme.data.formatted$gender)

# Sum Assured
hist(scheme.data.formatted$sum.assured)
sd(scheme.data.formatted$sum.assured)

# Salary
hist(scheme.data.formatted$salary)
sd(scheme.data.formatted$salary)

# Occupations
hist(scheme.data.formatted$occ)
getmode(scheme.data.formatted$occ)

# Claims
hist(scheme.data.formatted$claim)
mean(scheme.data.formatted$claim)*1000 #claims per mil

# summary descriptions of other variables location etc

##### Correlations #####
pairs(scheme.data.formatted) # or plot(scheme.data.formatted)
corPlot(scheme.data.formatted)
