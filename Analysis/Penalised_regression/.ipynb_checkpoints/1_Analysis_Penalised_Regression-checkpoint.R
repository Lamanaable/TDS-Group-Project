##### Sample script for elastic net, ridge and lasso:

# To consider:
# - Are variables all the right structure for the glm package?
# - Can the package deal with any NAs?
# - What are the variables we want to include in the first place?

# What I know:
# - There are no NAs in CRC_case (outcome)
# - BMI has NAs which may be preventing it from working -> I don't think it can deal with NAs





# Load packages -----------------------------------------------------------

# MAKE SURE THESE PACKAGES ARE INTALLED ON THE CONDA ENVIRONMENT
print("Loading packages")
library(tidyr)
library(glmnet)
library(dplyr) 

# Loading data----------------------------------------------------------------
print("Loading data")
data<-CRC_case_data_QC_final <- readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/CRC_case_data_QC_final.rds")
nrow(data)




# Setup outcome and predictors --------------------------------------------
print("Setup outcome and predictors")
CRC_case <- which(colnames(data) == "CRC_case")
Y <- as.numeric(data[, CRC_case]) 
X <- as.data.frame(data[, -CRC_case])


# List of predictors in X:
#var_list <- dput(colnames(X))

test_vars_wanted<-c("bmi","sysBP","diaBP")


# Selecting just the columns wanted

X<-X[,test_vars_wanted]

complete_index <- as.vector(complete.cases(X))
X$complete_index <- complete_index

X<-filter(X,X$complete_index==TRUE)

X$bmi<-as.numeric((X$bmi))
X$sysBP<-as.numeric(X$sysBP)
X$diaBP<-as.numeric(X$diaBP)

X<-X[ ,1:3] # removing the index

nrow(X) # Without NAs there are 5915 rows


# Number of predictors in X:
ncol(X)

X<-as.matrix(X)

# Creating test and train dataset -----------------------------------------

# Change:
# - name of dataframe 
# - split accordingly

print("Setup test and train split")
set.seed(31)
train <- sample(1:nrow(data), 0.7 * nrow(data))
test <- seq(1, nrow(data))[-train]



# Creating summary results table to fill in -------------------------------

print("Creating empty results table")
matrix_results<-matrix(nrow=3,ncol=5)
summary_results<-as.data.frame(matrix_results)
colnames(summary_results)<-c("alpha","lambda min","lambda 1se", "No. predictors", "Test MSE")
rownames(summary_results)<-c("Ridge","Lasso","Elastic net")
summary_results


# Ridge -------------------------------------------------------------------

# Running ridge model (family="binomial for logisitic)
print("Running ridge model")
model_ridge <- suppressWarnings(cv.glmnet(X[train, ], Y[train], alpha = 0, family = "binomial"))

# Plotting ridge
print("Plotting ridge model")
plot(model_ridge, main="Ridge")

# Number of non-zero coefficients (should be full amount for ridge)
length(which(coef(model_ridge)!=0))-1

# Extracting lambda min and 1se
model_ridge$lambda.min
model_ridge$lambda.1se

# MSE associated with lambda min
min(model_ridge$cvm)

# MSE associated with lambda 1se
id_1se_ridge <- which(model_ridge$lambda == model_ridge$lambda.1se)
model_ridge$cvm[id_1se_ridge]

# Extracting the desired lambda valued (1se) - check if appropriate
bestlam_ridge <- model_ridge$lambda.1se

# Making predictions based on the test set and extracting MSE on test set
model_ridge_pred = predict(model_ridge, s = bestlam_ridge, newx = X[test, ])
mean((model_ridge_pred - Y[test])^2)

# Adding to first row of summary results
print("Adding ridge model to results table")
summary_results[1,1]<-0
summary_results[1,2]<-model_ridge$lambda.min
summary_results[1,3]<-model_ridge$lambda.1se
summary_results[1,4]<-length(which(coef(model_ridge)!=0))-1
summary_results[1,5]<-mean((model_ridge_pred - Y[test])^2)




# Lasso -------------------------------------------------------------------

# Running to lasso model
set.seed(100)
model_lasso <- cv.glmnet(X[train, ], Y[train], alpha = 1, family = "binomial")

# Plotting the ridge model
plot(model_lasso, main="Lasso")

# Extracting limbda min and 1se
model_lasso$lambda.min
model_lasso$lambda.1se

# MSE associated with lambda min
min(model_lasso$cvm)

# MSE associated with lambda 1se
id_1se_lasso <- which(model_lasso$lambda == model_lasso$lambda.1se)
model_lasso$cvm[id_1se_lasso]

# Extracting the desired lambda value (1se) - check if appropriate
bestlam_lasso <- model_lasso$lambda.1se

# Number of coefficients included under the bestlam_lasso
length(which(coef(model_lasso, s = bestlam_lasso) !=0)) - 1

# Making predictions based on the test set and extracting MSE on test set
model_lasso_pred <- predict(model_lasso, s = bestlam_lasso, newx = X[test, ])
mean((model_lasso_pred - Y[test])ˆ2)

# Adding to second row of summary results
summary_results[2,1]<-1
summary_results[2,2]<-model_lasso$lambda.min
summary_results[2,3]<-model_lasso$lambda.1se
summary_results[2,4]<-length(which(coef(model_lasso)!=0))-1
summary_results[2,5]<-mean((model_lasso_pred - Y[test])^2)
summary_results


# Elastic net -------------------------------------------------------------

# Tuning the alpha parameter (where the scale is 0=ridge -> 1=lasso)

# Defining alpha cross validation function:

# Does this need a binomial argument somewhere?
cvm = function(alpha) {
  model = cv.glmnet(x = X[train, ], y = Y[train], alpha = alpha)
  with(model, cvm[which.min(lambda - lambda.1se)])
}

# Running alpha cross validation and extracting the best alpha value
set.seed(100)
alpha.opt = optimize(cvm, c(0, 1))
alpha.opt

# Running cross-validaiton on lambda with set value of alpha (alpha.opt$minimum)
model_enet = cv.glmnet(x = X, y = Y, alpha = alpha.opt$minimum,family = "binomial")

# Plotting EN model
plot(model_enet)

# Extracting lambda min and lambda 1se
model_enet$lambda.min
model_enet$lambda.1se

# MSE associated with lambda min
min(model_enet$cvm)

# MSE associated with lambda 1se
id_1se_enet <- which(model_enet$lambda == model_enet$lambda.1se)
model_enet$cvm[id_1se_enet]

# Number of coefficients included in the model
bestlam_enet <- model_enet$lambda.1se
length(which(coef(model_enet, s = bestlam_enet) !=0)) - 1

# Making predictions based on the test set and extracting MSE on test set
model_enet_pred <- predict(model_enet, s = bestlam_enet, newx = X[test, ])
mean((model_enet_pred - Y[test])^2)

# Adding to third row of summary results
summary_results[3,1]<-alpha.opt$minimum
summary_results[3,2]<-model_enet$lambda.min
summary_results[3,3]<-model_enet$lambda.1se
summary_results[3,4]<-length(which(coef(model_enet)!=0))-1
summary_results[3,5]<-mean((model_enet_pred - Y[test])^2)





# Final results -----------------------------------------------------------

summary_results

