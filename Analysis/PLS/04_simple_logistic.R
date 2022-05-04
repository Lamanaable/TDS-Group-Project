# Logistic regression

rm(list=ls())
# Loading Data
CRC_ordered <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_5_year_ordered_V3.rds')
Stab_sPLS <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/PLS/Outputs/stability_analysis_sPLS_sum.rds')
Stab_sgPLS <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/PLS/Outputs/stab_selection_sgPLS_selprop.rds')

# set parameter
lambda = 7
pi = 0.9
alpha.x = 0.9

# Loading packages
library(sgPLS)
library(pheatmap)
library(RColorBrewer)
library(utils)
library(pROC)
library(doParallel)
library(parallel)

print("Number of cores")
number_cores<-detectCores()
number_cores
registerDoParallel(number_cores-1)

# In test set apply logistic regression
train = CRC_ordered[CRC_ordered$tt_status=='train',]
test = CRC_ordered[CRC_ordered$tt_status=='test',]

X_train = as.data.frame(apply(train[,11:ncol(train)],2,as.numeric))
Y_train = train$cc_status

X_test = as.data.frame(apply(test[,11:ncol(train)],2,as.numeric))
Y_test = test$cc_status

print(all(rownames(X_train)==rownames(Y_train)))

# logistic stab sgPLS-------
keep = names(Stab_sgPLS[Stab_sgPLS> pi])

X_train = X_train[,names(X_train) %in% keep]
X_test = X_test[,names(X_test) %in% keep]

train = cbind(X_train,Y_train)
test = cbind(X_test,Y_test)
fit <- glm(Y_train ~ ., family='binomial',data = train)
summary(fit)

y_pred = predict(fit,newdata = test,type='response')
predict_bi <- factor(ifelse(y_pred>0.5,1,0))

library(caret)
# confusion table
confusionMatrix(predict_bi, test$Y_test,positive="1")

library(ROCR)
par(mfrow=c(1,1))
pred_ROCR <- prediction(y_pred, test$Y_test)
roc_ROCR <- performance(pred_ROCR, 'tpr','fpr')
# AUC
auc_ROCR <- performance(pred_ROCR, measure = "auc")
auc_ROCR@y.values[[1]]

pdf("Outputs/logistic_sgPLS_ROC.pdf")
plot(roc_ROCR, colorize = TRUE)
title(main = "ROC curve: logistic regression (sgPLS)")
abline(a = 0, b = 1)
text(x=0.9,y=0.0,paste0('AUC: ',round(auc_ROCR@y.values[[1]],2)))
dev.off()

# logistic stab sPLS -------
train = CRC_ordered[CRC_ordered$tt_status=='train',]
test = CRC_ordered[CRC_ordered$tt_status=='test',]

X_train = as.data.frame(apply(train[,11:ncol(train)],2,as.numeric))
Y_train = train$cc_status

X_test = as.data.frame(apply(test[,11:ncol(train)],2,as.numeric))
Y_test = test$cc_status

print(all(rownames(X_train)==rownames(Y_train)))

keep = names(Stab_sPLS[Stab_sPLS>0.80])

X_train = X_train[,names(X_train) %in% keep]
X_test = X_test[,names(X_test) %in% keep]

train = cbind(X_train,Y_train)
test = cbind(X_test,Y_test)
fit <- glm(Y_train ~ ., family='binomial',data = train)
summary(fit)

y_pred = predict(fit,newdata = test,type='response')
predict_bi <- factor(ifelse(y_pred>0.5,1,0))

library(caret)
# confusion table
confusionMatrix(predict_bi, test$Y_test,positive="1")

library(ROCR)
par(mfrow=c(1,1))
pred_ROCR <- prediction(y_pred, test$Y_test)
roc_ROCR <- performance(pred_ROCR, 'tpr','fpr')
# AUC
auc_ROCR <- performance(pred_ROCR, measure = "auc")
auc_ROCR@y.values[[1]]

pdf("Outputs/logistic_sPLS_ROC.pdf")
plot(roc_ROCR, main = "ROC curve: logistic regression (stability analysis of sPLS)", 
     colorize = TRUE)
abline(a = 0, b = 1)
text(x=0.9,y=0.0,paste0('AUC: ',round(auc_ROCR@y.values[[1]],2)))
dev.off()

