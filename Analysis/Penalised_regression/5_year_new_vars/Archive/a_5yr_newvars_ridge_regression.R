### Ridgeregression


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
X_train <- read.csv("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/5_year_new_vars/Data/CRC_5yr_train_numeric.csv")
X_test <- read.csv("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/5_year_new_vars/Data/CRC_5yr_test_numeric.csv")

Y_train <-subset(X_train, select=cc_status)
Y_test <-subset(X_test, select=cc_status)

X_train <- subset(X_train, select=-cc_status)
X_test <- subset(X_test, select=-cc_status)

print("Checking everything is numeric")
table(sapply(X_train, class))
table(sapply(X_test, class))
str(Y_train)
str(Y_test)


print("Checking for NAs")
complete_index <- as.vector(complete.cases(X_train))
table(complete_index)
complete_index <- as.vector(complete.cases(X_test))
table(complete_index)
complete_index <- as.vector(complete.cases(Y_train))
table(complete_index)
complete_index <- as.vector(complete.cases(Y_test))
table(complete_index)


print("Converting to matrices to be used by glmnet")
X_train<-as.matrix(X_train)
X_test<-as.matrix(X_test)
Y_train<-as.matrix(Y_train)
Y_test<-as.matrix(Y_test)


print("Final check")
table(sapply(X_train, class))
table(sapply(X_test, class))
str(Y_train)
str(Y_test)


# Ridge model -------------------------------------------------------------
print("Ridge cross-validation")
set.seed(1)
model_ridge <- cv.glmnet(X_train, Y_train, alpha = 0, family = "binomial", standardize=FALSE)

print("Plotting ridge")
jpeg(file="Outputs/a_5yr_ridge_lambda.jpeg")
plot(model_ridge)
dev.off()

print("Number of non-zero coefficients (should be full amount for ridge)")
length(which(coef(model_ridge)!=0))-1 # does this need the -1?
print("Number of variables")
ncol(X_train)

print("Extracting lambda min and 1se")
model_ridge$lambda.min
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

saveRDS(summary_results, "Outputs/a_5yr_ridge_summary.rds")

print("done")


