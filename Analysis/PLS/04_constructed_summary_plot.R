# STABILITY SELECTION

rm(list=ls())
# Loading Data
setwd("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/PLS")
CRC_ordered <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_5_year_ordered_V3.rds')
# Stab_sgPLS <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/PLS/Outputs/stability_analysis_sgPLS_sum.rds')

# Loading packages
.libPaths(.libPaths()[1])
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

# train/test data loading
train = CRC_ordered[CRC_ordered$tt_status=='train',]
test = CRC_ordered[CRC_ordered$tt_status=='test',]

X_train = as.data.frame(apply(train[,11:ncol(train)],2,as.numeric))
Y_train = train$cc_status

X_test = as.data.frame(apply(test[,11:ncol(train)],2,as.numeric))
Y_test = test$cc_status

print(all(rownames(X_train)==rownames(Y_train)))

# Visualization of AUC and stability selection in lasso--------
LASSO <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/Stability_selection/5_year/Outputs/stab_selection_lasso_selprop.rds')

lambda = 0.005 
pi = 0.900
LASSO <- sort(LASSO[LASSO>pi],decreasing = TRUE)

# a single test of glmnet
suppressPackageStartupMessages(library(glmnet))

model <- glmnet(x=as.matrix(X_train), y=Y_train, family='binomial',
                alpha=1, lambda=lambda)

coef(model)

# compute mean beta for unpenalised-logistic-------

lassoSub = function(k=1, Xdata, Ydata, lambda, K=0.5) {
  set.seed(k)
  s = sample(nrow(Xdata), size = K * nrow(Xdata))
  Xsub = Xdata[s, ]
  Ysub = Ydata[s]
  model.sub = glmnet(x=as.matrix(Xsub), y=Ysub, family='binomial',
                     alpha=1, lambda=lambda)
  coef.sub = as.vector(coef(model.sub))
  return(coef.sub)
}

Niter=1000

cl <- makeCluster(number_cores-1,type='FORK')

beta = parSapply(cl=cl, 1:Niter, FUN = lassoSub, Xdata = X_train, Ydata = Y_train, lambda=lambda)
stopCluster(cl)

beta <- beta[2:204,]
rownames(beta) <- names(X_train)
  
beta[beta=='0'] <-NA
beta <- as.data.frame(beta)

meanbeta <- as.data.frame(apply(beta,1,mean,na.rm=TRUE))
names(meanbeta) = 'beta'
meanbeta$feature = rownames(meanbeta)
head(meanbeta)

stab_merge<- as.data.frame(LASSO)
stab_merge$feature <- names(LASSO)

meanbeta_merge <- merge(meanbeta[order(abs(meanbeta$beta),decreasing = TRUE),],stab_merge,by='feature')
meanbeta_merge <- meanbeta_merge[order(abs(meanbeta_merge$beta),decreasing = TRUE),]
rownames(meanbeta_merge) <- meanbeta_merge$feature
meanbeta_merge = meanbeta_merge[which(meanbeta_merge$feature %in% names(LASSO)),]
head(meanbeta_merge)

AUC = NULL
AUC.l = NULL
AUC.h = NULL

AUC_train = NULL
AUC.l_train = NULL
AUC.h_train = NULL

# compute AUC---------
library(pROC)
train = CRC_ordered[CRC_ordered$tt_status=='train',]
test = CRC_ordered[CRC_ordered$tt_status=='test',]

X_train = train[,11:ncol(train)]
Y_train = train$cc_status

X_test = test[,11:ncol(test)]
Y_test = test$cc_status

for (Nkeep in seq(1,length(LASSO))) {
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
    
    y_pred_train = coef(fit)[1] + train_sub[,1]*coef(fit)[2]
    AUC.CI_train = ci.auc(train_sub$Y_train, y_pred_train,boot.n=1000,parallel=TRUE) 
    
    AUC_train <- cbind(AUC_train,AUC.CI_train[2])
    AUC.l_train <- cbind(AUC.l_train, AUC.CI_train[1])
    AUC.h_train <- cbind(AUC.h_train, AUC.CI_train[3])
    
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
    
    y_pred_train = predict(fit,newdata = X_train_sub, type='response')
    AUC.CI_train = ci.auc(train_sub$Y_train, y_pred_train,boot.n=1000,parallel=TRUE) 
    
    AUC_train <- cbind(AUC_train,AUC.CI_train[2])
    AUC.l_train <- cbind(AUC.l_train, AUC.CI_train[1])
    AUC.h_train <- cbind(AUC.h_train, AUC.CI_train[3])
  }
}

AUC = as.vector(AUC)
AUC.l = as.vector(AUC.l)
AUC.h = as.vector(AUC.h)

AUC_train = as.vector(AUC_train)
AUC.l_train = as.vector(AUC.l_train)
AUC.h_train = as.vector(AUC.h_train)


# make a results table
results <- cbind(AUC,AUC.l,AUC.h,AUC_train,AUC.l_train,AUC.h_train)
results <- as.data.frame(results)

results$AUC <- round(results$AUC,3)
results$AUC.l <- round(results$AUC.l,3)
results$AUC.h <- round(results$AUC.h,3)

results$AUC_train <- round(results$AUC_train,3)
results$AUC.l_train <- round(results$AUC.l_train,3)
results$AUC.h_train <- round(results$AUC.h_train,3)

rownames(results) = rownames(meanbeta_merge)
results$feature = rownames(results)

results$beta <- meanbeta_merge$beta
results$stab = meanbeta_merge$LASSO

head(results)

saveRDS(results,'Outputs/logistic_lasso_stab_summary.rds')

## Plot------
# Change variable names
# Load Data
vars = read.csv('/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Variable_definition/vars_lookup.csv')
results = readRDS('Outputs/logistic_lasso_stab_summary.rds')
vars$Oldname <- as.character(vars$Oldname)
vars$Newname <- as.character(vars$Newname)
vars$feature <- vars$Oldname
vars = vars[vars$Oldname %in% results$feature,]
head(vars)

# Rename columns of the dataset
df = results # can change dataset to which you want to change the names
df$index <- 1:(dim(results)[1])

df <- merge(df,vars[,c(3,5)],by='feature')
df <- merge(df,vars[,c(4,5)],by='feature')
df <- df[order(df$index),]
rownames(df)= df$index

# remove underscores
df$feature <- gsub(x = df$Newname, pattern = "_", replacement = " ")

# see what has been changed

## plot mean beta-------
results <- df

# change x axis type to make features ordered
results_ori = readRDS('Outputs/logistic_lasso_stab_summary.rds')

results$feature <- as.character(results$feature)
#Then turn it back into a factor with the levels in the correct order
results$feature <- factor(results$feature, levels=unique(results$feature))

pdf('Outputs/logistic_lasso_summary_Beta.pdf',width=10,height=3.5)
par(mar=c(6,5,2,4))

library(RColorBrewer)
MyPal = brewer.pal("Paired", n = 8)

p1 <- ggplot(data=results, aes(x=feature,y=beta)) +
  geom_bar(stat = "identity", width = 0.01, 
           color = ifelse(results$beta > 0, MyPal[6],'royalblue')) +
  ggtitle(label = 'lasso summary: Mean logOR') +
  geom_hline(yintercept = 0, color = 'black', alpha=0.5, linetype=2) + 
  ylab('Mean logOR') +
  theme_light()

p1 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=6))
dev.off()

## plot AUC and error bar-------

pdf('Outputs/logistic_lasso_summary_AUC_CI.pdf',width=10,height=4)
par(mar=c(6,5,2,4))

breaks = c(seq(0.5,0.6,by=0.02), 0.45, round(results$AUC[length(LASSO)],2), 0.65)
labels = as.character(breaks)

library(RColorBrewer)
MyPal = brewer.pal("Paired", n = 8)
mycol = as.character(recode(results$Groups, 
                            "Outcome" = 'yellow',
                            "Baseline" = 'lightcoral',
                            "Physical" = 'mediumpurple1',
                            "SocioDemo" = 'maroon1',
                            "Lifestyle" = 'cyan3',
                            "Family_history" = 'seagreen3',
                            "Biomarker"= 'darkgoldenrod',
                            "Meds_Ops" = 'steelblue1',
                            "Comorbidity" = 'olivedrab4'))

p2 <- ggplot(data=results, aes(x=feature,y=AUC))+
  geom_errorbar(aes(ymin= AUC.l,ymax=AUC.h),width=0.2, 
                color = ifelse(results$stab>pi,'lightblue','grey')) +
  geom_point(color = ifelse(results$stab>pi,'royalblue','grey')) +
  ggtitle(label = 'lasso summary: AUC') +
  scale_y_continuous(limits = c(0.45,0.65), breaks = breaks, labels = labels,
                     name = "AUC in test data") +
  geom_hline(yintercept = results$AUC[length(LASSO)], color = 'black',alpha=0.5,linetype=2) + 
  theme_bw() 

p2 + theme(axis.text.x = element_text(
  angle = 90, vjust = 0.5, hjust=1, size=6, color = mycol))

dev.off()

pdf('Outputs/logistic_lasso_summary_AUC_CI_train.pdf',width=10,height=4)
par(mar=c(6,5,2,4))

breaks = c(seq(0.5,0.6,by=0.02), 0.45, round(results$AUC_train[length(LASSO)],2), 0.65)
labels = as.character(breaks)

library(RColorBrewer)
MyPal = brewer.pal("Paired", n = 8)
mycol = as.character(recode(results$Groups, 
                            "Outcome" = 'yellow',
                            "Baseline" = 'lightcoral',
                            "Physical" = 'mediumpurple1',
                            "SocioDemo" = 'maroon1',
                            "Lifestyle" = 'cyan3',
                            "Family_history" = 'seagreen3',
                            "Biomarker"= 'darkgoldenrod',
                            "Meds_Ops" = 'steelblue1',
                            "Comorbidity" = 'olivedrab4'))

p2_train <- ggplot(data=results, aes(x=feature,y=AUC_train))+
  geom_errorbar(aes(ymin= AUC.l_train,ymax=AUC.h_train),width=0.2, 
                color = ifelse(results$stab>pi,'lightblue','grey')) +
  geom_point(color = ifelse(results$stab>pi,'royalblue','grey')) +
  ggtitle(label = 'lasso summary: AUC in train') +
  scale_y_continuous(limits = c(0.45,0.65), breaks = breaks, labels = labels,
                     name = "AUC in train data") +
  geom_hline(yintercept = results$AUC_train[length(LASSO)], color = 'black',alpha=0.5,linetype=2) + 
  theme_bw() 

p2_train + theme(axis.text.x = element_text(
  angle = 90, vjust = 0.5, hjust=1, size=6, color = mycol))

dev.off()

## plot proportion selected -------

pdf('Outputs/logistic_lasso_summary_prop.pdf',width=10,height=4)
par(mar=c(6,5,2,4))

p3 <- ggplot(data=results, aes(x=feature,y=stab)) +
  geom_bar(stat = "identity", width = 0.01, 
           color = 'royalblue') +
  geom_hline(yintercept = pi, color = 'black', alpha=0.5, linetype=2) +
  ylim(0.0,1.0) +
  ggtitle(label = 'sgPLS summary: Selection proportion') +
  ylab('Selection prop.') +
  theme_light()

p3 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=6))
dev.off()



# Visualization of AUC and stability selection in sgPLS--------

train = CRC_ordered[CRC_ordered$tt_status=='train',]
test = CRC_ordered[CRC_ordered$tt_status=='test',]

X_train = as.data.frame(apply(train[,11:ncol(train)],2,as.numeric))
Y_train = train$cc_status

X_test = as.data.frame(apply(test[,11:ncol(train)],2,as.numeric))
Y_test = test$cc_status

print(all(rownames(X_train)==rownames(Y_train)))

## sgPLS stab-------
stab_sgPLS <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/PLS/Outputs/stab_selection_sgPLS_selprop.rds')
names <- rownames(stab_sgPLS)
stab_sgPLS <- as.vector(stab_sgPLS)
names(stab_sgPLS) <- names

stab_sgPLS <- sort(stab_sgPLS,decreasing = TRUE)

n = 4.000
alpha = 0.100
pi = 0.900

stab_selected = stab_sgPLS[stab_sgPLS>pi]
stab_plot = stab_sgPLS[1:45]

# a single test of sgPLS
Xgroups=c(23, 28, 51, 92, 106, 124, 149)

MysgPLSDA <- sgPLSda(X_train, Y_train, ncomp = 1, ind.block.x = Xgroups, 
                            keepX = n, alpha.x = alpha)

length(MysgPLSDA$loadings$X[MysgPLSDA$loadings$X!=0])

MysgPLSDA$explained_variance

# compute mean beta-------
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
rownames(meanbeta) <- colnames(CRC_ordered[,11:ncol(CRC_ordered)])
names(meanbeta) = 'beta'
meanbeta$feature = rownames(meanbeta)
head(meanbeta)

stab_merge<- as.data.frame(stab_plot)
stab_merge$feature <- names(stab_plot)

meanbeta_merge <- merge(meanbeta[order(abs(meanbeta$beta),decreasing = TRUE),],stab_merge,by='feature')
meanbeta_merge <- meanbeta_merge[order(abs(meanbeta_merge$beta),decreasing = TRUE),]
rownames(meanbeta_merge) <- meanbeta_merge$feature
A = meanbeta_merge[which(meanbeta_merge$feature %in% names(stab_selected)),]
B = meanbeta_merge[-(which(meanbeta_merge$feature %in% names(stab_selected))),]
meanbeta_merge <- rbind(A,B)
  
AUC = NULL
AUC.l = NULL
AUC.h = NULL

AUC_train = NULL
AUC.l_train = NULL
AUC.h_train = NULL

# compute AUC---------
library(pROC)
train = CRC_ordered[CRC_ordered$tt_status=='train',]
test = CRC_ordered[CRC_ordered$tt_status=='test',]

X_train = train[,11:ncol(train)]
Y_train = train$cc_status

X_test = test[,11:ncol(test)]
Y_test = test$cc_status

for (Nkeep in seq(1,length(stab_plot))) {
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
    
    y_pred_train = coef(fit)[1] + train_sub[,1]*coef(fit)[2]
    AUC.CI_train = ci.auc(train_sub$Y_train, y_pred_train,boot.n=1000,parallel=TRUE) 
    
    AUC_train <- cbind(AUC_train,AUC.CI_train[2])
    AUC.l_train <- cbind(AUC.l_train, AUC.CI_train[1])
    AUC.h_train <- cbind(AUC.h_train, AUC.CI_train[3])
    
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
  
  y_pred_train = predict(fit,newdata = X_train_sub, type='response')
  AUC.CI_train = ci.auc(train_sub$Y_train, y_pred_train,boot.n=1000,parallel=TRUE) 
  
  AUC_train <- cbind(AUC_train,AUC.CI_train[2])
  AUC.l_train <- cbind(AUC.l_train, AUC.CI_train[1])
  AUC.h_train <- cbind(AUC.h_train, AUC.CI_train[3])
  }
}

AUC = as.vector(AUC)
AUC.l = as.vector(AUC.l)
AUC.h = as.vector(AUC.h)

AUC_train = as.vector(AUC_train)
AUC.l_train = as.vector(AUC.l_train)
AUC.h_train = as.vector(AUC.h_train)

# make a results table
results <- cbind(AUC,AUC.l,AUC.h,AUC_train,AUC.l_train,AUC.h_train)
results <- as.data.frame(results)

results$AUC <- round(results$AUC,3)
results$AUC.l <- round(results$AUC.l,3)
results$AUC.h <- round(results$AUC.h,3)

results$AUC_train <- round(results$AUC_train,3)
results$AUC.l_train <- round(results$AUC.l_train,3)
results$AUC.h_train <- round(results$AUC.h_train,3)

rownames(results) = rownames(meanbeta_merge)
results$feature = rownames(results)

results$beta <- round(meanbeta_merge$beta,3)
results$stab = stab_plot

head(results)

saveRDS(results,'Outputs/logistic_sgPLS_stab_summary.rds')

## Plot------
# Change variable names
# Load Data
vars = read.csv('/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Variable_definition/vars_lookup.csv')
results = readRDS('Outputs/logistic_sgPLS_stab_summary.rds')
results <- results[1:38,]
vars$Oldname <- as.character(vars$Oldname)
vars$Newname <- as.character(vars$Newname)
vars$feature <- vars$Oldname
vars = vars[vars$Oldname %in% results$feature,]
head(vars)

# Rename columns of the dataset
df = results # can change dataset to which you want to change the names
df$index <- 1:(dim(results)[1])

df <- merge(df,vars[,c(3,5)],by='feature')
df <- merge(df,vars[,c(4,5)],by='feature')
df <- df[order(df$index),]
rownames(df)= df$index

# remove underscores
df$feature <- gsub(x = df$Newname, pattern = "_", replacement = " ")

# see what has been changed

## plot mean beta-------
results <- df

# change x axis type to make features ordered
results_ori = readRDS('Outputs/logistic_sgPLS_stab_summary.rds')
 
results$feature <- as.character(results$feature)
#Then turn it back into a factor with the levels in the correct order
results$feature <- factor(results$feature, levels=unique(results$feature))

pdf('Outputs/logistic_sgPLS_summary_Beta.pdf',width=7,height=3)
par(mar=c(6,5,2,4))

library(RColorBrewer)
MyPal = brewer.pal("Paired", n = 8)

p1 <- ggplot(data=results, aes(x=feature,y=beta)) +
  geom_bar(stat = "identity", width = 0.01, 
           color = ifelse(results$beta > 0, MyPal[6],'royalblue')) +
  ggtitle(label = 'sgPLS summary: Mean Beta') +
  geom_hline(yintercept = 0, color = 'black', alpha=0.5, linetype=2) + 
  ylab('Mean Beta') +
  theme_light()

p1 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=6))
dev.off()

## plot AUC and error bar-------

pdf('Outputs/logistic_sgPLS_summary_AUC_CI.pdf',width=7,height=4)
par(mar=c(6,5,2,4))

breaks = c(seq(0.5,0.6,by=0.02), 0.45, round(results$AUC[length(stab_selected)],2), 0.65)
labels = as.character(breaks)

library(RColorBrewer)
MyPal = brewer.pal("Paired", n = 8)
mycol = as.character(recode(results$Groups, 
                            "Outcome" = 'yellow',
                            "Baseline" = 'lightcoral',
                            "Physical" = 'mediumpurple1',
                            "SocioDemo" = 'maroon1',
                            "Lifestyle" = 'cyan3',
                            "Family_history" = 'seagreen3',
                            "Biomarker"= 'darkgoldenrod',
                            "Meds_Ops" = 'steelblue1',
                            "Comorbidity" = 'olivedrab4'))

p2 <- ggplot(data=results, aes(x=feature,y=AUC))+
  geom_errorbar(aes(ymin= AUC.l,ymax=AUC.h),width=0.2, 
                color = 'lightblue') +
  geom_point(color = 'royalblue') +
  ggtitle(label = 'logistic sgPLS summary: AUC') +
  scale_y_continuous(limits = c(0.45,0.65), breaks = breaks, labels = labels,
                     name = "AUC in test data") +
  geom_hline(yintercept = results$AUC[dim(results)[1]], color = 'black',alpha=0.5,linetype=2) + 
  theme_bw() 

p2 + theme(axis.text.x = element_text(
  angle = 90, vjust = 0.5, hjust=1, size=6, color = mycol))

dev.off()


pdf('Outputs/logistic_sgPLS_summary_AUC_CI_train.pdf',width=7,height=4)
par(mar=c(6,5,2,4))

breaks = c(seq(0.5,0.6,by=0.02), 0.45, round(results$AUC_train[length(stab_selected)],2), 0.65)
labels = as.character(breaks)

library(RColorBrewer)
MyPal = brewer.pal("Paired", n = 8)
mycol = as.character(recode(results$Groups, 
                            "Outcome" = 'yellow',
                            "Baseline" = 'lightcoral',
                            "Physical" = 'mediumpurple1',
                            "SocioDemo" = 'maroon1',
                            "Lifestyle" = 'cyan3',
                            "Family_history" = 'seagreen3',
                            "Biomarker"= 'darkgoldenrod',
                            "Meds_Ops" = 'steelblue1',
                            "Comorbidity" = 'olivedrab4'))

p2_train <- ggplot(data=results, aes(x=feature,y=AUC_train))+
  geom_errorbar(aes(ymin= AUC.l_train,ymax=AUC.h_train),width=0.2, 
                color = 'lightblue') +
  geom_point(color = 'royalblue') +
  ggtitle(label = 'logistic sgPLS summary: AUC in train') +
  scale_y_continuous(limits = c(0.45,0.65), breaks = breaks, labels = labels,
                     name = "AUC in train data") +
  geom_hline(yintercept = results$AUC_train[dim(results)[1]], color = 'black',alpha=0.5,linetype=2) + 
  theme_bw() 

p2_train + theme(axis.text.x = element_text(
  angle = 90, vjust = 0.5, hjust=1, size=6, color = mycol))

dev.off()

## plot proportion selected -------

pdf('Outputs/logistic_sgPLS_summary_prop.pdf',width=7,height=4)
par(mar=c(6,5,2,4))
  
p3 <- ggplot(data=results, aes(x=feature,y=stab)) +
  geom_bar(stat = "identity", width = 0.01, 
           color ='royalblue') +
  geom_hline(yintercept = pi, color = 'black', alpha=0.5, linetype=2) +
  ylim(0.0,1.0) +
  ggtitle(label = 'sgPLS summary: Selection proportion') +
  ylab('Selection prop.') +
  theme_light()

p3 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=6))
dev.off()








# Summary of LASSO and sgPLS---------------
LASSO <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/Stability_selection/5_year/Outputs/stab_selection_lasso_selprop.rds')
LASSO <- sort(LASSO[LASSO>pi],decreasing = TRUE)
stab_sgPLS <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/PLS/Outputs/stab_selection_sgPLS_selprop.rds')
names <- rownames(stab_sgPLS)
stab_sgPLS <- as.vector(stab_sgPLS)
names(stab_sgPLS) <- names

stab_sgPLS <- sort(stab_sgPLS[stab_sgPLS>pi],decreasing = TRUE)

inter <- intersect(names(LASSO),names(stab_sgPLS))
  
### compute ORs-------
X_train = train[,colnames(train) %in% inter]
Y_train = train$cc_status

X_test = test[,colnames(test) %in% inter]
Y_test = test$cc_status

print(all(rownames(X_train)==rownames(Y_train)))

fit <- glm(data=X_train, formula=Y_train~.,family='binomial')
OR = coef(fit)[2:length(coef(fit))]
OR.l = (confint(fit)[,1])[2:length(confint(fit)[,1])]
OR.h = (confint(fit)[,2])[2:length(confint(fit)[,2])]

results_inter = as.data.frame(cbind(OR,OR.l,OR.h))
results_inter <- results_inter[order(abs(results_inter$OR),decreasing = TRUE),]
rownames(results_inter)[2] <- 'diet_alcohol_Previous'
rownames(results_inter)[3] <- 'smoking_status_Previous'
rownames(results_inter)[4] <- 'height_age_ten_Shorter'
rownames(results_inter)[5] <- 'diet_non_oily_fish_cat'
rownames(results_inter)[7] <- 'body_size_age_ten_Plumper'
rownames(results_inter)[8] <- 'diet_oily_fish_cat'
rownames(results_inter)[9] <- 'diet_coffee_Decaffeinated'
rownames(results_inter)[10] <- 'diet_very_hot_drink'
rownames(results_inter)[17] <- 'height_age_ten_Average'
rownames(results_inter)[19] <- 'early_life_breastfed_as_baby_Yes'
results_inter

AUC = NULL
AUC.l = NULL
AUC.h = NULL

AUC_train = NULL
AUC.l_train = NULL
AUC.h_train = NULL

# compute AUC---------
library(pROC)

for (Nkeep in seq(1,length(inter))) {
  print(Nkeep)
  keep = rownames(results_inter)[1:Nkeep]
  
  if (Nkeep ==1){
    X_train_sub = X_train[,names(X_train) %in% keep]
    X_test_sub = X_test[,names(X_test) %in% keep]
    
    train_sub = as.data.frame(cbind(X_train_sub,Y_train))
    test_sub = as.data.frame(cbind(X_test_sub,Y_test))
    
    train_sub$Y_train = train_sub$Y_train-1
    test_sub$Y_test = test_sub$Y_test-1
    
    fit <- glm(Y_train ~ ., family='binomial',data = train_sub)
    
    y_pred = coef(fit)[1] + test_sub[,1]*coef(fit)[2]
    y_pred <- exp(y_pred)/(1+exp(y_pred))
    AUC.CI = ci.auc(test_sub$Y_test, y_pred,boot.n=1000,parallel=TRUE) 
    
    AUC <- cbind(AUC,AUC.CI[2])
    AUC.l <- cbind(AUC.l, AUC.CI[1])
    AUC.h <- cbind(AUC.h, AUC.CI[3])
    
    y_pred_train = coef(fit)[1] + train_sub[,1]*coef(fit)[2]
    AUC.CI_train = ci.auc(train_sub$Y_train, y_pred_train,boot.n=1000,parallel=TRUE) 
    
    AUC_train <- cbind(AUC_train,AUC.CI_train[2])
    AUC.l_train <- cbind(AUC.l_train, AUC.CI_train[1])
    AUC.h_train <- cbind(AUC.h_train, AUC.CI_train[3])
    
  }else {
    X_train_sub = X_train[,names(X_train) %in% keep]
    X_test_sub = X_test[,names(X_test) %in% keep]
    
    train_sub = cbind(X_train_sub,Y_train)
    test_sub = cbind(X_test_sub,Y_test)
    
    fit <- glm(Y_train ~ ., family='binomial',data = train_sub)
    
    y_pred = predict(fit,newdata = test_sub, type='response')
    AUC.CI = ci.auc(test_sub$Y_test, y_pred,boot.n=1000,parallel=TRUE) 
    
    AUC <- cbind(AUC,AUC.CI[2])
    AUC.l <- cbind(AUC.l, AUC.CI[1])
    AUC.h <- cbind(AUC.h, AUC.CI[3])
    
    y_pred_train = predict(fit,newdata = train_sub, type='response')
    AUC.CI_train = ci.auc(train_sub$Y_train, y_pred_train,boot.n=1000,parallel=TRUE) 
    
    AUC_train <- cbind(AUC_train,AUC.CI_train[2])
    AUC.l_train <- cbind(AUC.l_train, AUC.CI_train[1])
    AUC.h_train <- cbind(AUC.h_train, AUC.CI_train[3])
  }
}

AUC = as.vector(AUC)
AUC.l = as.vector(AUC.l)
AUC.h = as.vector(AUC.h)

AUC_train = as.vector(AUC_train)
AUC.l_train = as.vector(AUC.l_train)
AUC.h_train = as.vector(AUC.h_train)

# make a results table
results_inter <- cbind(results_inter,AUC,AUC.l,AUC.h,
                       AUC_train,AUC.l_train,AUC.h_train)

results_inter$feature = rownames(results_inter)

LASSO <- LASSO[inter]
LASSO <- as.data.frame(LASSO)
LASSO$feature <- rownames(LASSO)
results_inter <- merge(results_inter,LASSO,by='feature')

stab_sgPLS <- stab_sgPLS[inter]
stab_sgPLS <- as.data.frame(stab_sgPLS)
stab_sgPLS$feature <- rownames(stab_sgPLS)
results_inter <- merge(results_inter,stab_sgPLS,by='feature')

results_inter <- results_inter[order(abs(results_inter$OR),decreasing=TRUE),]
head(results_inter)

saveRDS(results_inter,'Summary_lasso_sgPLS/logistic_lasso_sgPLS_stab_summary.rds')

## Plot------
# Change variable names
# Load Data
vars = read.csv('/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Variable_definition/vars_lookup.csv')
results = readRDS('Summary_lasso_sgPLS/logistic_lasso_sgPLS_stab_summary.rds')
uni = read.csv('/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Univariate/univariate_model_results.csv')
vars$Oldname <- as.character(vars$Oldname)
vars$Newname <- as.character(vars$Newname)
vars$feature <- vars$Oldname
vars = vars[vars$Oldname %in% results$feature,]
head(vars)

# Rename columns of the dataset
df = results # can change dataset to which you want to change the names
df$index <- 1:(dim(df)[1])

df <- merge(df,vars[,c(3,5)],by='feature')
df <- merge(df,vars[,c(4,5)],by='feature')
df <- df[order(df$index),]
rownames(df)= df$index

# remove underscores
df$feature <- gsub(x = df$Newname, pattern = "_", replacement = " ")

# see what has been changed

## plot mean beta-------
results_inter <- df

results_inter$feature <- as.character(results_inter$feature)
results_inter$feature <- factor(results_inter$feature, levels=unique(results_inter$feature))

results_inter

pdf('Summary_lasso_sgPLS/logistic_lasso_sgPLS_summary_OR.pdf',width=7,height=3.5)
par(mar=c(6,5,2,4))

MyPal = brewer.pal("Paired", n = 8)

p1 <- ggplot(data=results_inter, aes(x=feature,y=OR)) +
  geom_errorbar(aes(ymin= OR.l,ymax=OR.h),width=0.2, 
                color = ifelse(results_inter$OR > 0, MyPal[6],'royalblue'), alpha=0.5) +
  geom_point(color = ifelse(results_inter$OR > 0, MyPal[6],'royalblue')) +
  ggtitle(label = 'lasso-sgPLS summary: OR') +
  scale_y_discrete(limits = seq(-1,2,by=0.5), name = 'logOR') +
  geom_hline(yintercept = 0, color = 'black', alpha=0.5, linetype=2) + 
  theme_light()

p1 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=8),
           axis.text.y = element_text(color='gray44'),
           axis.title.y = element_text(size = 9))
dev.off()

## plot AUC and error bar-------

pdf('Summary_lasso_sgPLS/logistic_lasso_sgPLS_summary_AUC_CI.pdf',width=7,height=4)
par(mar=c(6,5,2,4))

breaks = c(seq(0.5,0.6,by=0.02), 0.45, round(results_inter$AUC[dim(results_inter)[1]],2), 0.65)
labels = as.character(breaks)

mycol = as.character(recode(results_inter$Groups,
                            "Physical" = 'mediumpurple1',
                            "Lifestyle" = 'cyan3',
                            "Family_history" = 'seagreen3',
                            "Biomarker"= 'darkgoldenrod',
                            "Meds_Ops" = 'steelblue1',
                            "Comorbidity" = 'olivedrab4'))

p2 <- ggplot(data=results_inter, aes(x=feature,y=AUC))+
  geom_errorbar(aes(ymin= AUC.l,ymax=AUC.h), width=0.2, 
                color = 'royalblue',alpha=0.5) +
  geom_point(color = 'royalblue') +
  ggtitle(label = 'logistic lasso-sgPLS summary: cumulative AUC') +
  scale_y_continuous(limits = c(0.45,0.65), breaks = breaks, labels = labels,
                     name = "AUC in unseen data") +
  geom_hline(yintercept = results_inter$AUC[dim(results_inter)[1]], color = 'black', alpha=0.5,linetype=2) + 
  theme_bw() 

p2 + theme(axis.text.x = element_text(
  angle = 90, vjust = 0.5, hjust=1, size=8, color = mycol),
  axis.text.y = element_text(color='gray44'),
  axis.title.y = element_text(size = 9))

dev.off()

pdf('Summary_lasso_sgPLS/logistic_lasso_sgPLS_summary_AUC_CI_train.pdf',width=7,height=4)
par(mar=c(6,5,2,4))

breaks = c(seq(0.5,0.6,by=0.02), 0.45, round(results_inter$AUC_train[dim(results_inter)[1]],2), 0.65)
labels = as.character(breaks)

mycol = as.character(recode(results_inter$Groups,
                            "Physical" = 'mediumpurple1',
                            "Lifestyle" = 'cyan3',
                            "Family_history" = 'seagreen3',
                            "Biomarker"= 'darkgoldenrod',
                            "Meds_Ops" = 'steelblue1',
                            "Comorbidity" = 'olivedrab4'))

p2 <- ggplot(data=results_inter, aes(x=feature,y=AUC_train))+
  geom_errorbar(aes(ymin= AUC.l_train,ymax=AUC.h_train), width=0.2, 
                color = 'royalblue',alpha=0.5) +
  geom_point(color = 'royalblue') +
  ggtitle(label = 'logistic lasso-sgPLS summary: cumulative AUC in train') +
  scale_y_continuous(limits = c(0.45,0.65), breaks = breaks, labels = labels,
                     name = "AUC in training data") +
  geom_hline(yintercept = results_inter$AUC_train[dim(results_inter)[1]], color = 'black', alpha=0.5,linetype=2) + 
  theme_bw() 

p2 + theme(axis.text.x = element_text(
  angle = 90, vjust = 0.5, hjust=1, size=8, color = mycol),
  axis.text.y = element_text(color='gray44'),
  axis.title.y = element_text(size = 9))

dev.off()

## plot proportion selected -------

pdf('Summary_lasso_sgPLS/logistic_lasso_sgPLS_summary_prop_lasso.pdf',width=7,height=3.5)
par(mar=c(6,5,2,4))

p3 <- ggplot(data=results_inter, aes(x=feature,y=LASSO)) +
  geom_bar(stat = "identity", width = 0.01, 
           color ='royalblue') +
  geom_point(color ='royalblue') +
  geom_hline(yintercept = pi, color = 'black', alpha=0.5, linetype=2) +
  ylim(0.0,1.0) +
  ggtitle(label = 'sgPLS summary: Selection proportion') +
  ylab('LASSO Selection prop.') +
  theme_light()

p3 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=8),
           axis.text.y = element_text(color='gray44'),
           axis.title.y = element_text(size = 9))
dev.off()

pdf('Summary_lasso_sgPLS/logistic_lasso_sgPLS_summary_prop_sgpls.pdf',width=7,height=3.5)
par(mar=c(6,5,2,4))

p4 <- ggplot(data=results_inter, aes(x=feature,y=stab_sgPLS)) +
  geom_bar(stat = "identity", width = 0.01, 
           color ='royalblue') +
  geom_point(color ='royalblue') +
  geom_hline(yintercept = pi, color = 'black', alpha=0.5, linetype=2) +
  ylim(0.0,1.0) +
  ggtitle(label = 'sgPLS summary: Selection proportion') +
  ylab('SgPLS Selection prop.') +
  theme_light()

p4 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=8),
           axis.text.y = element_text(color='gray44'),
           axis.title.y = element_text(size = 9))
dev.off()


## plot univariate results -----------
results_inter$beta = log(results_inter$beta)

pdf('Summary_lasso_sgPLS/logistic_lasso_sgPLS_summary_uni.pdf',width=7,height=4)
par(mar=c(6,5,2,4))

MyPal = brewer.pal("Paired", n = 8)
  
p5 <- ggplot(data=results_inter, aes(x=feature,y=beta)) +
  geom_bar(stat = "identity", width = 0.01, 
           color = ifelse(results_inter$beta > 0, MyPal[6],'royalblue')) +
  ggtitle(label = 'Beta for univariate') +
  geom_hline(yintercept = 0, color = 'black', alpha=0.5, linetype=2) + 
  ylab('logOR (univariate)') +
  theme_light()

p5 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=8),
           axis.text.y = element_text(color='gray44'),
           axis.title.y = element_text(size = 9))
dev.off()


# Summary of adjusted LASSO and sgPLS---------------
LASSO <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/Stability_selection/5_year/Outputs/stab_selection_lasso_selprop.rds')
LASSO <- sort(LASSO[LASSO>pi],decreasing = TRUE)

stab_sgPLS <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/PLS/Outputs/stab_selection_sgPLS_selprop.rds')
names <- rownames(stab_sgPLS)
stab_sgPLS <- as.vector(stab_sgPLS)
names(stab_sgPLS) <- names

stab_sgPLS <- sort(stab_sgPLS[stab_sgPLS>pi],decreasing = TRUE)

inter <- intersect(names(LASSO),names(stab_sgPLS))

### compute ORs-------
X_train = train[,colnames(train) %in% inter]
Y_train = train$cc_status

X_test = test[,colnames(test) %in% inter]
Y_test = test$cc_status

print(all(rownames(X_train)==rownames(Y_train)))
covars = train[,c('age_recr','family_history_CRC')]

fit <- glm(data=cbind(covars,X_train), formula=Y_train~.,family='binomial')
OR = coef(fit)[4:length(coef(fit))]
OR.l = (confint(fit)[,1])[4:length(confint(fit)[,1])]
OR.h = (confint(fit)[,2])[4:length(confint(fit)[,2])]

results_inter = as.data.frame(cbind(OR,OR.l,OR.h))
results_inter <- results_inter[order(abs(results_inter$OR),decreasing = TRUE),]
rownames(results_inter)[12] <- 'Comorb_Hypertension'

AUC = NULL
AUC.l = NULL
AUC.h = NULL

# compute AUC---------
library(pROC)
library(tidyverse)

for (Nkeep in seq(1,length(inter))) {
  print(Nkeep)
  keep = rownames(results_inter)[1:Nkeep]
  covars_train = train[,c('age_recr','family_history_CRC')]
  covars_test = test[,c('age_recr','family_history_CRC')]
  
  if (Nkeep ==1){
    train_sub = X_train[,names(X_train) %in% keep]
    test_sub = X_test[,names(X_test) %in% keep]
    
    fit <- glm(Y_train ~ ., family='binomial',data = cbind(covars_train,train_sub))
    
    y_pred = coef(fit)[1] + covars_test[,1]*coef(fit)[2] + (as.numeric(covars_test[,2])-1)*coef(fit)[3] + test_sub*coef(fit)[4]
    y_pred <- exp(y_pred)/(1+exp(y_pred))
    AUC.CI = ci.auc(Y_test, y_pred,boot.n=1000,parallel=TRUE) 
    
    AUC <- cbind(AUC,AUC.CI[2])
    AUC.l <- cbind(AUC.l, AUC.CI[1])
    AUC.h <- cbind(AUC.h, AUC.CI[3])
    
  }else {
    train_sub = X_train[,names(X_train) %in% keep]
    test_sub = X_test[,names(X_test) %in% keep]
    
    fit <- glm(Y_train ~ ., family='binomial',data = cbind(covars_train,train_sub))
    
    y_pred = predict(fit,newdata = cbind(covars_test,test_sub),type='response')
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
results_inter <- cbind(results_inter,AUC,AUC.l,AUC.h)

results_inter$feature = rownames(results_inter)

LASSO <- LASSO[inter]
LASSO <- as.data.frame(LASSO)
LASSO$feature <- rownames(LASSO)
results_inter <- merge(results_inter,LASSO,by='feature')

stab_sgPLS <- stab_sgPLS[inter]
stab_sgPLS <- as.data.frame(stab_sgPLS)
stab_sgPLS$feature <- rownames(stab_sgPLS)
results_inter <- merge(results_inter,stab_sgPLS,by='feature')

head(results_inter)

results_inter <- results_inter[order(abs(results_inter$OR),decreasing=TRUE),]

saveRDS(results_inter,'Summary_adjusted_lasso_sgPLS/logistic_lasso_sgPLS_stab_summary.rds')


## Plot------
# Change variable names
# Load Data
vars = read.csv('/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Variable_definition/vars_lookup.csv')
results = readRDS('Summary_adjusted_lasso_sgPLS/logistic_lasso_sgPLS_stab_summary.rds')
vars$Oldname <- as.character(vars$Oldname)
vars$Newname <- as.character(vars$Newname)
vars$feature <- vars$Oldname
vars = vars[vars$Oldname %in% results$feature,]
head(vars)

# Rename columns of the dataset
df = results # can change dataset to which you want to change the names
df$index <- 1:(dim(df)[1])

df <- merge(df,vars[,c(3,5)],by='feature')
df <- merge(df,vars[,c(4,5)],by='feature')
df <- df[order(df$index),]
rownames(df)= df$index

# remove underscores
df$feature <- gsub(x = df$Newname, pattern = "_", replacement = " ")

# see what has been changed

## plot mean beta-------
results_inter <- df

results_inter$feature <- as.character(results_inter$feature)
results_inter$feature <- factor(results_inter$feature, levels=unique(results_inter$feature))

pdf('Summary_adjusted_lasso_sgPLS/logistic_lasso_sgPLS_summary_OR.pdf',width=7,height=4)
par(mar=c(6,5,2,4))

MyPal = brewer.pal("Paired", n = 8)

p1 <- ggplot(data=results_inter, aes(x=feature,y=OR)) +
  geom_errorbar(aes(ymin= OR.l,ymax=OR.h),width=0.2, 
                color = ifelse(results_inter$OR > 0, MyPal[6],'royalblue'), alpha=0.5) +
  geom_point(color = ifelse(results_inter$OR > 0, MyPal[6],'royalblue')) +
  ggtitle(label = 'lasso-sgPLS summary: OR') +
  scale_y_discrete(limits = seq(-1,2,by=0.5), name = 'logOR') +
  geom_hline(yintercept = 0, color = 'black', alpha=0.5, linetype=2) + 
  theme_light()

p1 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=8),
           axis.text.y = element_text(color='gray44'),
           axis.title.y = element_text(size = 9))
dev.off()

## plot AUC and error bar-------

pdf('Summary_adjusted_lasso_sgPLS/logistic_lasso_sgPLS_summary_AUC_CI.pdf',width=7,height=5)
par(mar=c(6,5,2,4))

breaks = c(seq(0.5,0.6,by=0.02), 0.45, round(results_inter$AUC[dim(results_inter)[1]],2), 0.65)
labels = as.character(breaks)

library(RColorBrewer)
MyPal = brewer.pal("Paired", n = 8)
mycol = as.character(recode(results_inter$Groups, 
                            "Outcome" = 'yellow',
                            "Baseline" = MyPal[1],
                            "Physical" = MyPal[2],
                            "SocioDemo" = MyPal[3],
                            "Lifestyle" = MyPal[4],
                            "Family_history" = MyPal[5],
                            "Biomarker"= MyPal[6],
                            "Meds_Ops" = MyPal[7],
                            "Comorbidity" = MyPal[8]))

p2 <- ggplot(data=results_inter, aes(x=feature,y=AUC))+
  geom_errorbar(aes(ymin= AUC.l,ymax=AUC.h), width=0.2, 
                color = 'royalblue',alpha=0.5) +
  geom_point(color = 'royalblue') +
  ggtitle(label = 'logistic lasso-sgPLS summary: cumulative AUC') +
  scale_y_continuous(limits = c(0.45,0.65), breaks = breaks, labels = labels,
                     name = "AUC in unseen data") +
  geom_hline(yintercept = results_inter$AUC[dim(results_inter)[1]], color = 'black', alpha=0.5,linetype=2) + 
  theme_bw() 

p2 + theme(axis.text.x = element_text(
  angle = 90, vjust = 0.5, hjust=1, size=8, color = mycol),
  axis.text.y = element_text(color='gray44'),
  axis.title.y = element_text(size = 9))

dev.off()

## plot proportion selected -------

pdf('Summary_adjusted_lasso_sgPLS/logistic_lasso_sgPLS_summary_prop_lasso.pdf',width=7,height=3)
par(mar=c(6,5,2,4))

p3 <- ggplot(data=results_inter, aes(x=feature,y=LASSO)) +
  geom_bar(stat = "identity", width = 0.01, 
           color ='royalblue') +
  geom_point(color ='royalblue') +
  geom_hline(yintercept = pi, color = 'black', alpha=0.5, linetype=2) +
  ylim(0.0,1.0) +
  ggtitle(label = 'sgPLS summary: Selection proportion') +
  ylab('LASSO Selection prop.') +
  theme_light()

p3 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=8),
           axis.text.y = element_text(color='gray44'),
           axis.title.y = element_text(size = 9))
dev.off()

pdf('Summary_adjusted_lasso_sgPLS/logistic_lasso_sgPLS_summary_prop_sgpls.pdf',width=7,height=3)
par(mar=c(6,5,2,4))

p4 <- ggplot(data=results_inter, aes(x=feature,y=stab_sgPLS)) +
  geom_bar(stat = "identity", width = 0.01, 
           color ='royalblue') +
  geom_point(color ='royalblue') +
  geom_hline(yintercept = pi, color = 'black', alpha=0.5, linetype=2) +
  ylim(0.0,1.0) +
  ggtitle(label = 'sgPLS summary: Selection proportion') +
  ylab('SgPLS Selection prop.') +
  theme_light()

p4 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=8),
           axis.text.y = element_text(color='gray44'),
           axis.title.y = element_text(size = 9))
dev.off()

