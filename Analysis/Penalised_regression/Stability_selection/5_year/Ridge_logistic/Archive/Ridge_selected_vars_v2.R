### Ridge regression


# Loading packages --------------------------------------------------------
print("loading packages")
library(glmnet)



# Creating empty results table --------------------------------------------
print("Creating an empty matrix")
matrix_results<-matrix(nrow=1,ncol=7)
summary_results<-as.data.frame(matrix_results)
colnames(summary_results)<-c("alpha","lambda min","lambda 1se", "No. predictors lambda min","No. predictors lambda 1se", "Test MSE lambda min", "Test MSE lambda 1se")
rownames(summary_results)<-c("Ridge")
summary_results


# Data preparation --------------------------------------------------------
print("Data preparation")
X_train <- readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/Data/CRC_5yr_train_standardised.rds")
X_test <- readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/Data/CRC_5yr_test_standardised.rds")

Y_train <-(subset(X_train, select=cc_status))
Y_test <-(subset(X_test, select=cc_status))

X_train <- (subset(X_train, select=-cc_status))
X_test <- (subset(X_test, select=-cc_status))


# Selecting stable variables ----------------------------------------------


stab<-readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/Stability_selection/5_year/Outputs/stab_selection_lasso_selprop.rds")


print("selecting vars")
keep = c(dput(names(stab[stab>0.9])))
length(keep)


X_train = X_train[,names(X_train) %in% keep]
ncol(X_train)
X_test = X_test[,names(X_test) %in% keep]
ncol(X_test)

train = as.matrix(cbind(X_train,Y_train))
test = as.matrix(cbind(X_test,Y_test))

X_train<-as.matrix(X_train)
X_test<-as.matrix(X_test)
Y_train<-as.matrix(Y_train)
Y_test<-as.matrix(Y_test)




# Ridge model -------------------------------------------------------------
print("Ridge cross-validation")
set.seed(1)
model_ridge <- cv.glmnet(X_train, Y_train, alpha = 0, family = "binomial", standardize=FALSE)

print("Plotting ridge")
jpeg(file="Outputs/ridge_lambda.jpeg")
plot(model_ridge)
dev.off()

print("Number of non-zero coefficients (should be full amount for ridge)")
length(which(coef(model_ridge)!=0))-1 # does this need the -1?
print("Number of variables")
ncol(X_train)

print("Extracting lambda min and 1se")
print("lambda min")
model_ridge$lambda.min
print("lambda 1se")
model_ridge$lambda.1se

print("MSE associated with lambda min")
min(model_ridge$cvm)

print("MSE associated with lambda 1se")
id_1se_ridge <- which(model_ridge$lambda == model_ridge$lambda.1se)
model_ridge$cvm[id_1se_ridge]

print("Extracting the desired lambda valued (1se) - check if appropriate")
bestlam_ridge <- model_ridge$lambda.1se

print("Making predictions based on the test set and extracting MSE on test set")
model_ridge_pred_lambda1se = predict(model_ridge, s = model_ridge$lambda.1se, newx = X_test)
mean((model_ridge_pred_lambda1se - Y_test)^2)
model_ridge_pred_lambdamin = predict(model_ridge, s = model_ridge$lambda.min, newx = X_test)
mean((model_ridge_pred_lambdamin - Y_test)^2)

# Adding to first row of summary results
print("Results")
summary_results[1]<-0
summary_results[2]<-model_ridge$lambda.min
summary_results[3]<-model_ridge$lambda.1se
summary_results[4]<-length(which(coef(model_ridge, s = model_ridge$lambda.min) !=0)) - 1
summary_results[5]<-length(which(coef(model_ridge, s = model_ridge$lambda.1se) !=0)) - 1
summary_results[6]<-mean((model_ridge_pred_lambdamin - Y_test)^2)
summary_results[7]<-mean((model_ridge_pred_lambda1se - Y_test)^2)
summary_results

saveRDS(summary_results, "Outputs/ridge_summary.rds")

# Predictions under 1se ---------------------------------------------------

predict_bi <- factor(ifelse(model_ridge_pred_lambda1se>0.5,1,0))


# Confusion matrix --------------------------------------------------------
print("loading caret")
library(caret)
# confusion table
print("confusion matrix")
confusionMatrix(predict_bi, test$Y_test, positive="1")


# Metrics -----------------------------------------------------------------
print("Loading ROCR")
library(ROCR)
par(mfrow=c(1,1))
pred_ROCR <- prediction(y_pred, test$Y_test)
roc_ROCR <- performance(pred_ROCR, 'tpr','fpr')

# AUC
auc_ROCR <- performance(pred_ROCR, measure = "auc")
auc_ROCR@y.values[[1]] 


# ROC curve ---------------------------------------------------------------

#AUC CI
print("Loading pROC")
library(pROC)
set.seed(2)
auc_ci<-ci.auc(Y_test, y_pred, boot.n=1000) 
print("AUC")
auc_ci
# AUC is 0.516-0.5813 (DeLong)

print("ROC curve")
pdf("Outputs/ridge_auc_stability_selection.pdf")
plot(roc_ROCR, colorize = TRUE)
title(main = "ROC curve: ridge logistic regression - based on variables selected by lasso stability")
abline(a = 0, b = 1)
text(x=0.9,y=0.0,paste0('AUC: ',round(auc_ROCR@y.values[[1]],2)))
dev.off()

# forest plot -------------------------------------------------------------
results_train <- cbind(coef(fit), confint(fit))
colnames(results_train) <- c('OR','conf.low','conf.high')
results_train <- data.frame(results_train)
results_train$feature <- rownames(results_train)
saveRDS(results_train,"Outputs/ridge_coefficients")

