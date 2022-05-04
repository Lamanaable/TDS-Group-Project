# PLS ANALYSIS-SENSITIVITY

# 1. Setup -------------------------------------------------------------------

# Package used for date conversion:
rm(list=ls())
library(tidyverse)

# 2. Reading the data --------------------------------------------------------
setwd("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/PLS/Sensitivity_Analysis/Colon_only")
CRC <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Sensitivity_Data/CRC_colon_only.rds')
.libPaths()
#.libPaths(c( "/rds/general/user/wq21/home/R/x86_64-pc-linux-gnu-library/4.1", .libPaths() ) )

# 3. Analysis ------------------------------------------
#library(tidyverse)
#install.packages('sgPLS')
#install.packages('devtools')
#library(devtools)
install_github("mixOmicsTeam/mixOmics")
#library(pheatmap)
#library(RColorBrewer)
#library(utils)
#source("../Scripts/pls_functions.R")

train = CRC[CRC$tt_status=='train',]
test = CRC[CRC$tt_status=='test',]

X_train = as.data.frame(apply(train[,11:ncol(train)],2,as.numeric))
Y_train = train$cc_status

X_test = as.data.frame(apply(test[,11:ncol(train)],2,as.numeric))
Y_test = test$cc_status

print(all(rownames(X_train)==rownames(Y_train)))

# sgPLS and LASSO logistic prediction--------
stab_sgPLS <- readRDS('../Outputs/stab_selection_colon_selprop.rds')
names = rownames(stab_sgPLS)
stab_sgPLS <- sort(stab_sgPLS,decreasing = TRUE)
names(stab_sgPLS) <- names
n = 5
alpha = 0.100 
pi = 0.900

LASSO <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/Stability_selection/5_year/Outputs_colon/stab_selection_lasso_selprop.rds")
stab_sgPLS = stab_sgPLS[stab_sgPLS>pi]

inter <- intersect(names(LASSO),names(stab_sgPLS))

# logistic stab sgPLS-------
library(pROC)
keep = names(stab_sgPLS)

X_train = X_train[,names(X_train) %in% keep]
X_test = X_test[,names(X_test) %in% keep]

fit <- glm(Y_train ~ ., family='binomial',data = X_train)
summary(fit)

y_pred = predict(fit,newdata = X_test,type='response')

AUC.CI = ci.auc(Y_test, y_pred,boot.n=1000,parallel=TRUE) 

AUC <- AUC.CI[2]
AUC.l <- AUC.CI[1]
AUC.h <- AUC.CI[3]

# Metrics 

ROC <- plot.roc(roc(Y_test,y_pred))
thr <- coords(ROC, "best", ret="threshold", best.method="youden")
thr

library(caret) 
y_pred_bi <- as.factor(ifelse(y_pred > 0.349,1,0))
cm = confusionMatrix(y_pred_bi, Y_test,mode = 'everything', positive="1")
cm

# logistic inter-------
keep = inter

X_train = X_train[,names(X_train) %in% keep]
X_test = X_test[,names(X_test) %in% keep]

fit <- glm(Y_train ~ ., family='binomial',data = X_train)
summary(fit)

y_pred = predict(fit,newdata = X_test,type='response')

AUC.CI = ci.auc(Y_test, y_pred,boot.n=1000,parallel=TRUE) 

AUC <- AUC.CI[2]
AUC.l <- AUC.CI[1]
AUC.h <- AUC.CI[3]

# Metrics 

ROC <- plot.roc(roc(Y_test,y_pred))
thr <- coords(ROC, "best", ret="threshold", best.method="youden")
thr

install.packages('caret')
library(caret)
y_pred_bi <- as.factor(ifelse(y_pred > 0.365,1,0))
cm = confusionMatrix(y_pred_bi, Y_test, mode = 'everything',positive="1")
cm






# AUC plot sgPLS--------

train = CRC[CRC$tt_status=='train',]
test = CRC[CRC$tt_status=='test',]

X_train = apply(train[,11:ncol(train)],2,as.numeric)
Y_train = train$cc_status

X_test = apply(test[,11:ncol(train)],2,as.numeric)
Y_test = test$cc_status

# results
suppressPackageStartupMessages(library(sgPLS))
library(pheatmap)
library(utils)
library(pROC)
library(doParallel)
library(parallel)
library(tidyverse)
print("Number of cores")
number_cores<-detectCores()
number_cores
registerDoParallel(number_cores-1)

library(mixOmics)

SgplsSub = function(k=1, Xdata, Ydata, Nkeep, Xgroups, alpha, K=0.5) {
  set.seed(k)
  s = sample(nrow(Xdata), size = K * nrow(Xdata))
  Xsub = Xdata[s, ]
  Ysub = Ydata[s]
  model.sub = sgPLSda(Xsub, Ysub, ncomp = 1, 
                      ind.block.x = Xgroups, 
                      keepX = Nkeep, 
                      alpha.x = alpha)
  coef.sub = model.sub$loadings$X
  return(coef.sub)
}

Niter=1000

cl <- makeCluster(number_cores-1,type='FORK')

beta = parSapply(cl=cl, 1:Niter, FUN = SgplsSub, Xdata = X_train, Ydata = Y_train, 
                 Nkeep = n, Xgroups = Xgroups, alpha=alpha)

stopCluster(cl)

beta[beta=='0'] <-NA

beta <- -as.data.frame(beta)

meanbeta <- as.data.frame(apply(beta,1,mean,na.rm=TRUE))
rownames(meanbeta) <- colnames(CRC[,11:ncol(CRC)])
names(meanbeta) = 'beta'
meanbeta$feature = rownames(meanbeta)
head(meanbeta)

stab_merge<- as.data.frame(stab_selected)
stab_merge$feature <- names(stab_selected)

meanbeta_merge <- merge(meanbeta[order(abs(meanbeta$beta),decreasing = TRUE),],stab_merge,by='feature')
meanbeta_merge <- meanbeta_merge[order(abs(meanbeta_merge$beta),decreasing = TRUE),]
rownames(meanbeta_merge) <- meanbeta_merge$feature
meanbeta_merge = meanbeta_merge[which(meanbeta_merge$feature %in% names(stab_selected)),]

AUC = NULL

AUC.l = NULL
AUC.h = NULL

# compute AUC---------
library(pROC)
train = CRC[CRC$tt_status=='train',]
test = CRC[CRC$tt_status=='test',]

X_train = train[,11:ncol(train)]
Y_train = train$cc_status

X_test = test[,11:ncol(test)]
Y_test = test$cc_status

for (Nkeep in seq(1,length(stab_selected))) {
  print(Nkeep)
  keep = rownames(meanbeta_merge[1:Nkeep,])
  
  if (Nkeep ==1){
    X_train_sub = X_train[,names(X_train) %in% keep]
    X_test_sub = X_test[,names(X_test) %in% keep]
    
    train_sub = as.data.frame(cbind(X_train_sub,Y_train))
    test_sub = as.data.frame(cbind(X_test_sub,Y_test))
    
    train_sub$Y_train = as.numeric(train_sub$Y_train)-1
    test_sub$Y_test = as.numeric(test_sub$Y_test)-1
    
    fit <- glm(Y_train ~ ., family='binomial',data = train_sub)
    
    y_pred = coef(fit)[1] + test_sub[,1]*coef(fit)[2]
    AUC.CI = ci.auc(test_sub$Y_test, y_pred,boot.n=1000,parallel=TRUE) 
    
    AUC <- cbind(AUC,AUC.CI[2])
    AUC.l <- cbind(AUC.l, AUC.CI[1])
    AUC.h <- cbind(AUC.h, AUC.CI[3])
    
  }else {
    X_train_sub = X_train[,names(X_train) %in% keep]
    X_test_sub = X_test[,names(X_test) %in% keep]
    
    fit <- glm(Y_train ~ ., family='binomial',data = X_train_sub)
    
    y_pred = predict(fit,newdata = X_test_sub, type='response')
    set.seed(2)
    AUC.CI = ci.auc(Y_test, y_pred,boot.n=1000,parallel=TRUE) 
    
    AUC <- cbind(AUC,AUC.CI[2])
    AUC.l <- cbind(AUC.l, AUC.CI[1])
    AUC.h <- cbind(AUC.h, AUC.CI[3])
  }
}

AUC = as.vector(AUC)

AUC.l = as.vector(AUC.l)

AUC.h = as.vector(AUC.h)

# make a results table
results <- cbind(AUC,AUC.l,AUC.h)
results <- as.data.frame(results)

results$AUC <- round(results$AUC,3)
results$AUC.l <- round(results$AUC.l,3)
results$AUC.h <- round(results$AUC.h,3)
rownames(results) = rownames(meanbeta_merge)
results$feature = rownames(results)

results$beta <- round(meanbeta_merge$beta,3)
results$stab = stab_selected

head(results)

saveRDS(results,'../Outputs/logistic_sgPLS_stab_colon_summary.rds')

# Overall AUC---------
results[nrow(results),]
