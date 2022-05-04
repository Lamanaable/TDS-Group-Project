###  Enet regression


# Loading packages --------------------------------------------------------
print("loading packages")
library(glmnet)
#library(pheatmap)
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
colnames(summary_results)<-c("alpha","lambda min","lambda 1se", "No. predictors lambda min","No. predictors lambda 1se", "Test MSE lambda min", "Test MSE lambda 1se")
rownames(summary_results)<-c("enet")
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


      
# ENET -------------------------------------------------------------------

# Defining CVM function
print("Defining alpha cvm")
cvm = function(alpha) {
  model = cv.glmnet(x = X_train, y = Y_train, alpha = alpha, family="binomial", parallel=TRUE)
  with(model, cvm[which.min(lambda - lambda.1se)])
}

print("Running alpha cross validation and extracting the best alpha value")
set.seed(100)
Sys.time()
alpha.opt = optimize(cvm, c(0, 1))
Sys.time()
print("alpha.opt$minimum")
alpha.opt$minimum
print("alpha.opt")
alpha.opt$objective


print("Running cross-validaiton on lambda with set value of alpha (alpha.opt$minimum)")
set.seed(1)
model_enet = cv.glmnet(x = X_train, y = Y_train, alpha = alpha.opt$minimum,family = "binomial", parallel=TRUE)

print("Plotting EN model")
jpeg(file="Outputs/c_enet_5yr_lambda.jpeg")  
plot(model_enet)
dev.off()

print("Extracting lambda min and lambda 1se")
model_enet$lambda.min
model_enet$lambda.1se

print("MSE associated with lambda min")
min(model_enet$cvm)

print("MSE associated with lambda 1se")
id_1se_enet <- which(model_enet$lambda == model_enet$lambda.1se)
model_enet$cvm[id_1se_enet]

print("Number of coefficients included in the model")
bestlam_enet <- model_enet$lambda.1se
length(which(coef(model_enet, s = bestlam_enet) !=0)) - 1

print("Making predictions based on the test set and extracting MSE on test set")
model_enet_pred_lambda1se <- predict(model_enet, s = model_enet$lambda.1se, newx = X_test)
mean((model_enet_pred_lambda1se - Y_test)^2)
model_enet_pred_lambdamin <- predict(model_enet, s =model_enet$lambda.min, newx = X_test)
mean((model_enet_pred_lambdamin - Y_test)^2)

print("Adding to summary results")
summary_results[1]<-alpha.opt$minimum
summary_results[2]<-model_enet$lambda.min
summary_results[3]<-model_enet$lambda.1se
summary_results[4]<-length(which(coef(model_enet, s = model_enet$lambda.min) !=0)) - 1
summary_results[5]<-length(which(coef(model_enet, s = model_enet$lambda.1se) !=0)) - 1
summary_results[6]<-mean((model_enet_pred_lambdamin - Y_test)^2)
summary_results[7]<-mean((model_enet_pred_lambda1se - Y_test)^2)
saveRDS(summary_results, "Outputs/c_5yr_enet_summary.rds")


# Final results -----------------------------------------------------------
summary_results

print("Coefficients under lambda min")
a=which(coef(model_enet, s = model_enet$lambda.min) !=0)
coef_enet_min<-data.frame(colnames(X_train)[a])
coef_enet_min$OR<-exp(coef(model_enet, s = model_enet$lambda.min)[a])
coef_enet_min
print("Exporting coefficients")
saveRDS(coef_enet_min, "Outputs/c_enet_5yr_coefficients_lambdamin.rds")


print("Coefficients under lambda 1se")
b=which(coef(model_enet, s = model_enet$lambda.1se) !=0)
coef_enet_1se<-data.frame(colnames(X_train)[b])
coef_enet_1se$OR<-exp(coef(model_enet, s = model_enet$lambda.1se)[b])
coef_enet_1se
print("Exporting coefficients")
saveRDS(coef_enet_1se, "Outputs/c_enet_5yr_coefficients_lambda1se.rds")


# Stability analysis ------------------------------------------------------
print("Stability analysis under lambda 1se")

# Estimating the probability of selection of each variable in the enet model via subsampling of the data.
print("Defining function")
EnetSub = function(k = 1, X=X_train, Y=Y_train, family = "binomial",
                    penalty.factor = NULL) {
  if (is.null(penalty.factor)) {
    penalty.factor = rep(1, ncol(X))
  }
  set.seed(k)
  s = sample(nrow(X), size = 0.8 * nrow(X))
  Xsub = X[s, ]
  Ysub = Y[s]
  model.sub = cv.glmnet(x = Xsub, y = Ysub, alpha = alpha.opt$minimum,
                        family = family, penalty.factor = penalty.factor, parallel=TRUE)
  coef.sub = coef(model.sub, s = "lambda.1se")[-1]
  return(coef.sub)
}

print("Running function with 100 iterations")
niter = 100
enet.stab = sapply(1:niter, FUN = EnetSub)


print("Model size for each iteration")
apply(enet.stab, 2, FUN = function(x) {
  sum(x != 0)
})

print("The proportion of selection of each variable")
enet.prop = apply(enet.stab, 1, FUN = function(x) {
  sum(x != 0)/length(x)
})
names(enet.prop) = colnames(X_train)

# There are so many -> may need to raise to above 0.2 (or export results as a table to plot later-> export as r file)
print("Visualisation of proportion selected")
enet.prop = sort(enet.prop, decreasing = TRUE)
# jpeg(file="c_enet_stability_lambda1se.jpeg")    
# plot(enet.prop[enet.prop > 0.2], type = "h", col = "navy",
#      lwd = 3, xaxt = "n", xlab = "", ylab = expression(beta),
#      ylim = c(0, 1.2), las = 1)
# text(enet.prop[enet.prop > 0.2] + 0.07, labels = names(enet.prop[enet.prop >
#                                                                       0.2]), pos = 3, srt = 90, cex = 0.7)
# dev.off()

print("Table of proportion selected under lambda 1se")
enet.prop
saveRDS(enet.prop, "Outputs/c_enet_5yr_stability_lambda1se.rds")


# print("Correlation between the high signals")
# X_invest = X_train[, unique(c(true.results, names(enet.prop)[enet.prop >
#                                                                 0.2]))]
# Xcor = cor(X_invest)
# jpeg(file="c_enet_heatmap_lambda1se.jpeg")  
# pheatmap(Xcor, cluster_rows = FALSE, cluster_cols = FALSE,
#          border = FALSE, breaks = seq(-1, 1, length.out = 100))
# dev.off()


# Stability analysis under lambda min -------------------------------------

print("Stability analysis under lambda min")

# Estimating the probability of selection of each variable in the enet model via subsampling of the data.
print("Defining function")
EnetSub_min = function(k = 1, X=X_train, Y=Y_train, family = "binomial",
                   penalty.factor = NULL) {
  if (is.null(penalty.factor)) {
    penalty.factor = rep(1, ncol(X))
  }
  set.seed(k)
  s = sample(nrow(X), size = 0.8 * nrow(X))
  Xsub = X[s, ]
  Ysub = Y[s]
  model.sub = cv.glmnet(x = Xsub, y = Ysub, alpha = alpha.opt$minimum,
                        family = family, penalty.factor = penalty.factor, parallel=TRUE)
  coef.sub = coef(model.sub, s = "lambda.min")[-1]
  return(coef.sub)
}

print("Running function with 100 iterations")
niter = 100
enet.stab_min = sapply(1:niter, FUN = EnetSub_min)


print("Model size for each iteration")
apply(enet.stab_min, 2, FUN = function(x) {
  sum(x != 0)
})

print("The proportion of selection of each variable")
enet.prop_min = apply(enet.stab_min, 1, FUN = function(x) {
  sum(x != 0)/length(x)
})
names(enet.prop_min) = colnames(X_train)

# There are so many -> may need to raise to above 0.2 (or export results as a table to plot later-> export as r file)
print("Visualisation of proportion selected")
enet.prop_min = sort(enet.prop_min, decreasing = TRUE)
# jpeg(file="c_enet_stability_lambda1se.jpeg")    
# plot(enet.prop[enet.prop > 0.2], type = "h", col = "navy",
#      lwd = 3, xaxt = "n", xlab = "", ylab = expression(beta),
#      ylim = c(0, 1.2), las = 1)
# text(enet.prop[enet.prop > 0.2] + 0.07, labels = names(enet.prop[enet.prop >
#                                                                       0.2]), pos = 3, srt = 90, cex = 0.7)
# dev.off()

print("Table of proportion selected under lambda min")
enet.prop_min
saveRDS(enet.prop_min, "Outputs/c_enet_5yr_stability_lambdamin.rds")

print("done")


      