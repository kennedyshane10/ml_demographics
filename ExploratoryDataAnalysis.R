
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

rm(list=ls())

# 1.Hypothesis:


# 2. Read in data
scheme.data<-read.csv("C:/Users/Admin/Documents/R_projects/ml_demographics/Dataset/data.csv")

#3. Check it
nrow(scheme.data)
ncol(scheme.data)

#4. Look at its structure
str(scheme.data)

#5. Look at top and bottom of data
head(scheme.data)
tail(scheme.data)

