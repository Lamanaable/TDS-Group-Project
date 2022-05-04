###  Lasso regression


# Loading packages --------------------------------------------------------
print("loading packages")
library(glmnet)
# library(pheatmap)
library(doParallel)
library(parallel)


# Setting number of cores
print("Number of cores")
number_cores<-detectCores()
number_cores
registerDoParallel(number_cores-1)


# Creating empty results table --------------------------------------------
print("Creating an empty matrix")
matrix_results<-matrix(nrow=1,ncol=7)
summary_results<-as.data.frame(matrix_results)
colnames(summary_results)<-c("alpha","lambda min","lambda 1se", "No. predictors lambda ,min","No. predictors lambda 1se", "Test MSE lambda min", "Test MSE lambda 1se")
rownames(summary_results)<-c("Lasso")
summary_results


# Data preparation --------------------------------------------------------
print("Data preparation")
X_train <- read.csv("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/5_year/Data/CRC_5yr_train_numeric.csv")
X_test <- read.csv("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/5_year/Data/CRC_5yr_test_numeric.csv")

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


      
# Lasso -------------------------------------------------------------------
      
print("Lasso cross-validation")
set.seed(1)
model_lasso <- cv.glmnet(X_train, Y_train, alpha = 1, family = "binomial", parallel=TRUE)
      
print("Plotting lasso")
jpeg(file="Outputs/b_5yr_lasso_lambda.jpeg")      
plot_lambda_lasso<-plot(model_lasso)
dev.off()
      
print("Number of non-zero coefficients ")
length(which(coef(model_lasso)!=0))-1 
ncol(X_train)
# Why are there 1949 variables to start off withbut 2095 columns?
      
print("Extracting lambda min and 1se")
model_lasso$lambda.min
model_lasso$lambda.1se
      
print("MSE associated with lambda min")
min(model_lasso$cvm)
      
print("MSE associated with lambda 1se")
id_1se_lasso <- which(model_lasso$lambda == model_lasso$lambda.1se)
model_lasso$cvm[id_1se_lasso]
      
print("Extracting the desired lambda valued (1se)")
bestlam_lasso <- model_lasso$lambda.1se
      
print("Making predictions based on the test set and extracting MSE on test set")
model_lasso_pred_lambda1se = predict(model_lasso, s = model_lasso$lambda.1se, newx = X_test)
mean((model_lasso_pred_lambda1se - Y_test)^2)
model_lasso_pred_lambdamin = predict(model_lasso, s = model_lasso$lambda.min, newx = X_test)
mean((model_lasso_pred_lambdamin - Y_test)^2)
      
print("Adding to first row of summary results")
      
summary_results[1]<-1
summary_results[2]<-model_lasso$lambda.min
summary_results[3]<-model_lasso$lambda.1se
summary_results[4]<-length(which(coef(model_lasso, s = model_lasso$lambda.min) !=0)) - 1
summary_results[5]<-length(which(coef(model_lasso, s = model_lasso$lambda.1se) !=0)) - 1
summary_results[6]<-mean((model_lasso_pred_lambdamin - Y_test)^2)
summary_results[7]<-mean((model_lasso_pred_lambda1se - Y_test)^2)
summary_results
saveRDS(summary_results, "Outputs/b_5yr_lasso_summary.rds")

print("Coefficients under lambda min")
a=which(coef(model_lasso, s = model_lasso$lambda.min) !=0)
coef_lasso_min<-data.frame(colnames(X_train)[a])
coef_lasso_min$OR<-exp(coef(model_lasso, s = model_lasso$lambda.min)[a])
coef_lasso_min

print("Exporting coefficients under lambda min")
saveRDS(coef_lasso_min, "Outputs/b_5yr_lasso_coefficients_lambdamin.rds")

print("Coefficients under lambda 1se")
b=which(coef(model_lasso, s = model_lasso$lambda.1se) !=0)
coef_lasso_1se<-data.frame(colnames(X_train)[b])
coef_lasso_1se$OR<-exp(coef(model_lasso, s = model_lasso$lambda.1se)[b])
coef_lasso_1se

print("Exporting coefficients under lambda min")
saveRDS(coef_lasso_1se, "Outputs/b_5yr_lasso_coefficients_lambda1se.rds")

# Stability analysis under lambda 1se--------------------------------------------
print("Stability analysis under lambda 1se")

# Estimating the probability of selection of each variable in the LASSO model via subsampling of the data.
print("Defining function")
LassoSub = function(k = 1, X=X_train, Y=Y_train, family = "binomial",
                    penalty.factor = NULL) {
  if (is.null(penalty.factor)) {
    penalty.factor = rep(1, ncol(X_train))
  }
  set.seed(k)
  s = sample(nrow(X), size = 0.8 * nrow(X))
  Xsub = X[s, ]
  Ysub = Y[s]
  model.sub = cv.glmnet(x = Xsub, y = Ysub, alpha = 1,
                        family = family, penalty.factor = penalty.factor, parallel=TRUE)
  coef.sub = coef(model.sub, s = "lambda.1se")[-1]
  return(coef.sub)
}

print("Running function with 100 iterations")
niter = 100
Sys.time()
lasso.stab = sapply(1:niter, FUN = LassoSub)
Sys.time()


Sys.time()
print("Model size for each iteration")
apply(lasso.stab, 2, FUN = function(x) {
  sum(x != 0)
})

print("The proportion of selection of each variable")
lasso.prop = apply(lasso.stab, 1, FUN = function(x) {
  sum(x != 0)/length(x)
})
names(lasso.prop) = colnames(X_train)


print("Visualisation of proportion selected")
lasso.prop = sort(lasso.prop, decreasing = TRUE)
# jpeg(file="b_lasso_stability_lambda1se.jpeg")    
# plot(lasso.prop[lasso.prop > 0.2], type = "h", col = "navy",
#      lwd = 3, xaxt = "n", xlab = "", ylab = expression(beta),
#      ylim = c(0, 1.2), las = 1)
# text(lasso.prop[lasso.prop > 0.2] + 0.07, labels = names(lasso.prop[lasso.prop >
#                                                                       0.2]), pos = 3, srt = 90, cex = 0.7)
# dev.off()

print("Table of proportion selected under lambda 1se")
lasso.prop
saveRDS(lasso.prop, "Outputs/b_5yr_lasso_stability_lambda1se.rds")

# print("Correlation between the high signals")
# X_invest = X_train[, unique(c(true.results, names(lasso.prop)[lasso.prop >
#                                                                 0.2]))]
# Xcor = cor(X_invest)
# jpeg(file="b_lasso_heatmap_lambda1se.jpeg")  
# pheatmap(Xcor, cluster_rows = FALSE, cluster_cols = FALSE,
#          border = FALSE, breaks = seq(-1, 1, length.out = 100))
# dev.off()




# Stability analysis under lambda min--------------------------------------------
print("Stability analysis under lambda min")

# Estimating the probability of selection of each variable in the LASSO model via subsampling of the data.
print("Defining function")
LassoSub_min = function(k = 1, X=X_train, Y=Y_train, family = "binomial",
                    penalty.factor = NULL) {
  if (is.null(penalty.factor)) {
    penalty.factor = rep(1, ncol(X))
  }
  set.seed(k)
  s = sample(nrow(X_train), size = 0.8 * nrow(X_train))
  Xsub = X[s, ]
  Ysub = Y[s]
  model.sub = cv.glmnet(x = Xsub, y = Ysub, alpha = 1,
                        family = family, penalty.factor = penalty.factor, parallel=TRUE)
  coef.sub = coef(model.sub, s = "lambda.min")[-1]
  return(coef.sub)
}

print("Running function with 100 iterations")
niter = 100
set.seed(1)
lasso.stab_min = sapply(1:niter, FUN = LassoSub_min)


print("Model size for each iteration")
set.seed(1)
apply(lasso.stab_min, 2, FUN = function(x) {
  sum(x != 0)
})

print("The proportion of selection of each variable")
lasso.prop_min = apply(lasso.stab_min, 1, FUN = function(x) {
  sum(x != 0)/length(x)
})
names(lasso.prop_min) = colnames(X_train)


print("Visualisation of proportion selected")
lasso.prop_min = sort(lasso.prop_min, decreasing = TRUE)
# jpeg(file="b_lasso_stability_lambdamin.jpeg")    
# plot(lasso.prop_min[lasso.prop_min > 0.2], type = "h", col = "navy",
#      lwd = 3, xaxt = "n", xlab = "", ylab = expression(beta),
#      ylim = c(0, 1.2), las = 1)
# text(lasso.prop_min[lasso.prop_min > 0.2] + 0.07, labels = names(lasso.prop_min[lasso.prop_min >
#                                                                       0.2]), pos = 3, srt = 90, cex = 0.7)
# dev.off()

print("Table of proportion selected")
lasso.prop_min
saveRDS(lasso.prop_min, "Outputs/b_5yr_lasso_stability_lambdamin.rds")

# print("Correlation between the high signals")
# X_invest_min = X_train[, unique(c(true.results, names(lasso.prop)[lasso.prop_min >
#                                                                 0.2]))]
# Xcor_min = cor(X_invest_min)
# jpeg(file="b_lasso_heatmap_lambdamin.jpeg")  
# pheatmap(Xcor_min, cluster_rows = FALSE, cluster_cols = FALSE,
#          border = FALSE, breaks = seq(-1, 1, length.out = 100))
# dev.off()

# If I want to run this check what true.results is meant ot be....(see error from job Lasso_regression.e5167024)

print("done")



      