### Ridge regression


# Loading packages --------------------------------------------------------

library(glmnet)
library(doParallel)
library(parallel)
library(caret)
library(ROCR)



# Data preparation --------------------------------------------------------

print("Loading lasso data")
lasso_stab<-read.csv("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/Stability_selection/5_year/Outputs/stab_selection_lasso_selprop.csv")
colnames(lasso_stab)<-c("Variable","Proportion")

# Indices of variables with >=90% selected

var_indices<-dput(which(lasso_stab$Proportion>=0.9))

nrow(lasso_stab[var_indices,]) # Confirming that there are 100 selected


# X train and test --------------------------------------------------------
print("X train and test")
train <- read.csv("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/Stability_selection/5_year/Data/CRC_5yr_train_standardised.csv")
test <- read.csv("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/Stability_selection/5_year/Data/CRC_5yr_test_standardised.csv")

Y_train = train$cc_status
X_train = as.data.frame(train[-1])

Y_train = train$cc_status
head(Y_train)
X_train = as.data.frame(train[,-c(1,2)])
head(X_train)

Y_test = test$cc_status
head(Y_test)
X_test = as.data.frame(test[,-c(1,2)])
head(X_test)

print(all(rownames(X_train)==rownames(Y_train)))

# Therefore variables to be included in the ridge regression:

X_train_ridge<-X_train[,var_indices]
X_test_ridge<-X_test[,var_indices]

X_train_ridge<-as.matrix(X_train_ridge)
X_test_ridge<-as.matrix(X_test_ridge)



# Ridge -------------------------------------------------------------------
print("parallelising")
number_cores<-detectCores()
number_cores
registerDoParallel(number_cores-1)

print("cross-validation")
model_ridge <- cv.glmnet(X_train_ridge, Y_train, alpha = 0, family = "binomial", nfolds=1000, parallel=TRUE)

print("Extracting lambda 1se as best lambda")
best_lambda <- model_ridge$lambda.1se
best_lambda

print("Plot of lambda")
pdf("Outputs/ridge_lambda.pdf")
plot(model_ridge)

print("Predictions based on the ridge model with lambda 1se")
model_ridge_pred = predict(model_ridge, s = best_lambda,
                           newx = X_test_ridge, type="response")

#Results
print("Prediction based on cut-off of 0.4")
predict_bi <- factor(ifelse(model_ridge_pred>0.4,1,0)) # Tring 0.4 as cut-off -> change if not
predict_bi

# Confusion
#print("confusion matrix")
#confusionMatrix(predict_bi, Y_test, positive="1")

# ROC
print("ROC")
pred_ROCR <- prediction(model_ridge_pred, Y_test)
roc_ROCR <- performance(pred_ROCR, 'tpr','fpr')

# AUC
print("AUC")
auc_ROCR <- performance(pred_ROCR, measure = "auc")
auc_ROCR@y.values[[1]] # AUC 

print("AUC CI")
library(pROC)
set.seed(2)
auc_ci<-ci.auc(Y_test, model_ridge_pred, boot.n=1000) 
auc_ci


print("ROC plot")
pdf("Outputs/logistic_lasso_auc_stability_selection.pdf")
plot(roc_ROCR, colorize = TRUE)
title(main = "ROC curve: logistic regression - ridge using lasso variables")
abline(a = 0, b = 1)
text(x=0.9,y=0.0,paste0('AUC: ',round(auc_ROCR@y.values[[1]],2)))
dev.off()

print("done")
