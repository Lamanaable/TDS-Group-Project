# load packages----
library(ggplot2);
library(tidyverse);
library(caTools);
library(dplyr);
library(e1071);
library(GGally);
library(ROCR);
library(caret);

#library(sjlabelled)
#library(sjmisc)

# load data------
setwd("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/SVM/Sens_colon")
train = readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/SVM/Sens_colon/pca_train_5_year.rds')
test = readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/SVM/Sens_colon/pca_test_5_year.rds')

Y_train = train[,'cc_status'] 
Y_test = test[,'cc_status'] 

X_train = as.data.frame(train[,2:ncol(train)])
X_test = as.data.frame(test[,2:ncol(test)])

costs <- table(Y_train)
costs[1] <- 1 
costs[2] <- 2
costs

# train model -------

m0_= svm(formula = Y_train ~ ., data = X_train, type = 'C-classification', 
         class.weights = costs, probability = TRUE)

m2_= svm(formula = Y_train ~ ., data = X_train, type = 'C-classification',
         kernel = 'radial', class.weights = costs,
         cost=0.4, gamma=0.001, probability = TRUE)

y_pred_prob_m0 = predict(m0_, newdata = X_test, probability=TRUE)
y_pred_prob_m2 = predict(m2_, newdata = X_test, probability=TRUE)

y_pred_prob_m0 = attr(y_pred_prob_m0 , "probabilities")[,2]
y_pred_prob_m2 = attr(y_pred_prob_m2 , "probabilities")[,2]

y_pred_prob_m0_bi <- predict(m0_, newdata = X_test)
y_pred_prob_m2_bi <- predict(m2_, newdata = X_test)

# Confusion matrix --------------------------------------------------------

library(caret)
# confusion table
str(y_pred_prob_m0_bi)
Y_test <- as.factor(Y_test)
str(Y_test)
cm0 = confusionMatrix(y_pred_prob_m0_bi, Y_test, mode='everything',positive="1")
cm2 = confusionMatrix(y_pred_prob_m2_bi, Y_test, mode='everything',positive="1")

cm0
cm2

#AUC CI------------
library(pROC)
set.seed(2)
auc_ci_linear<-ci.auc(Y_test, y_pred_prob_m0, boot.n=1000) 
auc_ci_radial<-ci.auc(Y_test, y_pred_prob_m2, boot.n=1000)

auc_ci_linear
auc_ci_radial
auc_ci_radial[2]
