### Logistic Regression

## Based on variables selected by Elastic net ->80% stable

# Author: Ellie


# Setup ------------------------------------------------------------------




# Reading the data --------------------------------------------------------
CRC_ordered <-readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Sensitivity_Data/CRC_colon_only.rds")

stab<-readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/Stability_selection/5_year/Outputs/stab_selection_lasso_selprop.rds")

# Removing the intercept
#stab<-stab[-1]

length(stab[stab>=0.9]) # 100 variables selected


# Test and train ----------------------------------------------------------



train = CRC_ordered[CRC_ordered$tt_status=='train',]
test = CRC_ordered[CRC_ordered$tt_status=='test',]

X_train = as.data.frame(apply(train[,11:ncol(train)],2,as.numeric))
Y_train = train$cc_status

X_test = as.data.frame(apply(test[,11:ncol(train)],2,as.numeric))
Y_test = test$cc_status

print(all(rownames(X_train)==rownames(Y_train)))


# Selecting variables and running model-------------------------------------------

keep = c(dput(names(stab[stab>0.9])),"family_history_CRC")
length(keep)


X_train = X_train[,names(X_train) %in% keep]
X_test = X_test[,names(X_test) %in% keep]

train = cbind(X_train,Y_train)
test = cbind(X_test,Y_test)
fit <- glm(Y_train ~ ., family='binomial',data = train)
summary(fit)

y_pred = predict(fit,newdata = test)
predict_bi <- factor(ifelse(y_pred>0.5,1,0))


# Confusion matrix --------------------------------------------------------

library(caret)
# confusion table
confusionMatrix(predict_bi, test$Y_test, positive="1")


# Metrics -----------------------------------------------------------------

library(ROCR)
par(mfrow=c(1,1))
pred_ROCR <- prediction(y_pred, test$Y_test)
roc_ROCR <- performance(pred_ROCR, 'tpr','fpr')
# AUC
auc_ROCR <- performance(pred_ROCR, measure = "auc")
auc_ROCR@y.values[[1]] # AUC of 0.548 using variables >90% stability -> this is worse than when using cross-validation


# ROC curve ---------------------------------------------------------------

#AUC CI
library(pROC)
set.seed(2)
auc_ci<-ci.auc(Y_test, y_pred, boot.n=1000) 
auc_ci
# AUC is 0.516-0.5813 (DeLong)

pdf("Outputs/logistic_lasso_auc_stability_selection.pdf")
plot(roc_ROCR, colorize = TRUE)
title(main = "ROC curve: logistic regression - lasso stability selection")
abline(a = 0, b = 1)
text(x=0.9,y=0.0,paste0('AUC: ',round(auc_ROCR@y.values[[1]],2)))
dev.off()

# forest plot -------------------------------------------------------------
results_train <- cbind(exp(coef(fit)), exp(confint(fit)))
colnames(results_train) <- c('OR','conf.low','conf.high')
results_train <- data.frame(results_train)
results_train$feature <- rownames(results_train)

#results_train_selected <- results_log_reg_train[2:21,]

ggplot(data=results_train[-1,], aes(x = feature, y = OR,
                                                ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 1, color = 'red',alpha=0.5,linetype=2) +
  geom_point() +
  geom_linerange() +
  coord_flip() +
  theme_bw() +
  ylab("Coefficient estimate \n and 95% CI") +
  xlab("Regression coefficient") +
  ggtitle('Odds Ratio')
