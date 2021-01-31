# partition into train and test sets

## 80% of the sample size
smp_size <- floor(0.8 * nrow(scheme.data.formatted2.scaled))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(scheme.data.formatted2.scaled)), size = smp_size)

train <- scheme.data.formatted2.scaled[train_ind, ]
test <- scheme.data.formatted2.scaled[-train_ind, ]

# use Lasso/Ridge/Elastic net to select variables
# Logistic regression - GLM
# sparkling water (spark + H2O for large scale machine learning)
# adjust and iterate if necessary


# test model on test set

# evaluation metrics on model
# vif, AUROC, lift chart, other...