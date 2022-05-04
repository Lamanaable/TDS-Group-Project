# STABILITY SELECTION

rm(list=ls())
# Loading Data
CRC_ordered <- readRDS('/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_5_year_ordered_V2.rds')
#Stab_sgPLS <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/PLS/Outputs/stability_analysis_sgPLS_sum.rds')

# Loading packages
#suppressPackageStartupMessages(library(sgPLS))
library(pheatmap)
library(utils)
library(pROC)
library(doParallel)
library(parallel)
library(tidyverse)

print("Number of cores")
number_cores<-detectCores()-1
number_cores

# train/test data loading
train = CRC_ordered[CRC_ordered$tt_status=='train',]
test = CRC_ordered[CRC_ordered$tt_status=='test',]

X_train = as.data.frame(apply(train[,11:ncol(train)],2,as.numeric))
Y_train = train$cc_status

X_test = as.data.frame(apply(test[,11:ncol(train)],2,as.numeric))
Y_test = test$cc_status

print(all(rownames(X_train)==rownames(Y_train)))

# stability selection --------
library(focus)
library(igraph)
library(glmnet)
library(pheatmap)

Xgroups = c(23, 5, 26, 42, 15, 18, 28, 58)

stab_sgPLS = VariableSelection(xdata = X_train, ydata = Y_train, verbose = FALSE,
                               implementation= SparseGroupPLS,
                               group_x = Xgroups, 
                               alpha.x = 0.9,
                               n_cores = 5,
                               K=1000, tau=0.5,
                               group_penalisation = TRUE,
                               resampling = "subsampling",
                               family = "binomial")

saveRDS(stab_sgPLS,'Outputs/stab_selection_lasso_test.rds')

pdf('Outputs/stab_selection_sgPLS.pdf')
CalibrationPlot(stab_sgPLS)
dev.off()

summary(stab_sgPLS)
# 
# selprop = SelectionProportions(stab_sgPLS)
# saveRDS(selprop,'Outputs/stab_selection_lasso_test_selprop.rds')
# 
# hat_params=Argmax(stab_sgPLS)
# hat_params
# 
# pdf('Outputs/stab_selection_sgPLS_selprop_plot.pdf')
# par(mar=c(14,5,1,1))
# plot(selprop, type="h", lwd=3, las=1, xlab="", ylab="Selection Proportion", xaxt="n",
#      col=ifelse(selprop>=hat_params[2], yes="red", no="grey"), cex.lab=1.5)
# abline(h=hat_params[2], lty=2, col="darkred")
# for (i in 1:length(selprop)){
#   axis(side=1, at=i, labels=names(selprop)[i], cex.axis=0.6,las=2, 
#        col=ifelse(selprop[i]>=hat_params[2], yes="red", no="grey"),
#        col.axis=ifelse(selprop[i]>=hat_params[2], yes="red", no="grey"))
# }
# dev.off()
# 
# # Visualization of AUC and stability selection in sgPLS--------
# stab_sgPLS <- readRDS('Outputs/stab_selection_sgPLS_selprop.rds')
# stab_sgPLS <- sort(stab_sgPLS,decreasing = TRUE)
# lambda = 7
# pi = 0.810
# alpha.x = 0.9
# 
# stab_selected = stab_sgPLS[stab_sgPLS>pi]
# stab_plot = stab_sgPLS[1:40]
# 
# train = CRC_ordered[CRC_ordered$tt_status=='train',]
# test = CRC_ordered[CRC_ordered$tt_status=='test',]
#   
# X_train = apply(train[,11:ncol(train)],2,as.numeric)
# Y_train = train$cc_status
#   
# X_test = apply(test[,11:ncol(train)],2,as.numeric)
# Y_test = test$cc_status
# 
# # a single test of sgPLS
# Xgroups = c(23, 28, 54, 96, 111, 129, 157)
# 
# MysgPLSDA <- sgPLSda(X_train, Y_train, ncomp = 1, ind.block.x = Xgroups, 
#                             keepX = lambda, alpha.x = alpha.x)
# 
# length(MysgPLSDA$loadings$X[MysgPLSDA$loadings$X!=0])
# 
# 
# # compute mean beta-------
# library(mixOmics)
# 
# SgplsSub = function(k=1, Xdata, Ydata, Nkeep, Xgroups, alpha, K=0.5) {
#   set.seed(k)
#   s = sample(nrow(Xdata), size = K * nrow(Xdata))
#   Xsub = Xdata[s, ]
#   Ysub = Ydata[s]
#   model.sub = sgPLSda(Xsub, Ysub, ncomp = 1, 
#                        ind.block.x = Xgroups, 
#                        keepX = Nkeep, 
#                        alpha.x = alpha)
#   coef.sub = model.sub$loadings$X
#   return(coef.sub)
# }
# 
# Niter=1000
# 
# cl <- makeCluster(number_cores-1,type='FORK')
# 
# beta = parSapply(cl=cl, 1:Niter, FUN = SgplsSub, Xdata = X_train, Ydata = Y_train, 
#               Nkeep = 7, Xgroups = Xgroups, alpha=alpha.x)
# stopCluster(cl)
# 
# beta[beta=='0'] <-NA
# beta <- as.data.frame(beta)
# 
# meanbeta <- as.data.frame(apply(beta,1,mean,na.rm=TRUE))
# rownames(meanbeta) <- colnames(CRC_ordered[,11:ncol(CRC_ordered)])
# names(meanbeta) = 'beta'
# meanbeta$feature = rownames(meanbeta)
# View(meanbeta)
# 
# AUC = NULL
# 
# AUC.l = NULL
# AUC.h = NULL
# 
# # compute AUC---------
# X_train = as.data.frame(X_train)
# X_test = as.data.frame(X_test)
# 
# for (Nkeep in seq(1,length(stab_plot))) {
#   print(Nkeep)
#   keep = names(stab_plot[1:Nkeep])
#   
#   if (Nkeep ==1){
#     X_train_sub = X_train[,names(X_train) %in% keep]
#     X_test_sub = X_test[,names(X_test) %in% keep]
#     
#     train_sub = as.data.frame(cbind(X_train_sub,Y_train))
#     test_sub = as.data.frame(cbind(X_test_sub,Y_test))
#     
#     train_sub$Y_train = train_sub$Y_train-1
#     test_sub$Y_test = test_sub$Y_test-1
#     
#     fit <- glm(Y_train ~ ., family='binomial',data = train_sub)
#     
#     y_pred = coef(fit)[1] + test_sub[,1]*coef(fit)[2]
#     AUC.CI = ci.auc(test_sub$Y_test, y_pred,boot.n=1000,parallel=TRUE) 
#     
#     AUC <- cbind(AUC,AUC.CI[2])
#     AUC.l <- cbind(AUC.l, AUC.CI[1])
#     AUC.h <- cbind(AUC.h, AUC.CI[3])
#     
#   }else {
#   X_train_sub = X_train[,names(X_train) %in% keep]
#   X_test_sub = X_test[,names(X_test) %in% keep]
# 
#   train_sub = cbind(X_train_sub,Y_train)
#   test_sub = cbind(X_test_sub,Y_test)
# 
#   fit <- glm(Y_train ~ ., family='binomial',data = train_sub)
#   
#   y_pred = predict(fit,newdata = test_sub)
#   AUC.CI = ci.auc(test_sub$Y_test, y_pred,boot.n=1000,parallel=TRUE) 
#   
#   AUC <- cbind(AUC,AUC.CI[2])
#   AUC.l <- cbind(AUC.l, AUC.CI[1])
#   AUC.h <- cbind(AUC.h, AUC.CI[3])
#   }
# }
# 
# AUC = as.vector(AUC)
# 
# AUC.l = as.vector(AUC.l)
# 
# AUC.h = as.vector(AUC.h)
# 
# 
# # make a results table------
# results <- cbind(AUC,AUC.l,AUC.h)
# results <- as.data.frame(results)
# 
# results$AUC <- round(results$AUC,3)
# results$AUC.l <- round(results$AUC.l,3)
# results$AUC.h <- round(results$AUC.h,3)
# rownames(results) = names(stab_plot)
# results$feature = rownames(results)
# 
# results <- left_join(results,meanbeta,by='feature')
# results$beta <- round(results$beta,3)
# results$stab = round(stab_plot,3)
# 
# head(results)
# 
# saveRDS(results,'Outputs/logistic_sgPLS_summary.rds')
# 
# 
# ## plot mean beta-------
# 
# # change x axis type to make features ordered
# results$feature <- as.character(results$feature)
# #Then turn it back into a factor with the levels in the correct order
# results$feature <- factor(results$feature, levels=unique(results$feature))
# 
# pdf('Outputs/logistic_sgPLS_summary_Beta.pdf',width=7,height=4)
# par(mar=c(6,5,2,4))
# 
# p1 <- ggplot(data=results, aes(x=feature,y=beta)) +
#   geom_bar(stat = "identity", width = 0.05, 
#            color = ifelse(results$stab>pi,'royalblue','lightblue')) +
#   ggtitle(label = 'logistic sgPLS summary: Mean Beta') +
#   geom_hline(yintercept = 0, color = 'black', alpha=0.5, linetype=2) + 
#   ylab('Mean Beta') +
#   theme_light()
# 
# p1 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=6))
# dev.off()
# 
# ## plot AUC and error bar-------
# 
# pdf('Outputs/logistic_sgPLS_summary_AUC.pdf')
# par(mar=c(15,5,2,4))
# plot(AUC, pch = 2, col = "navy",
#      lwd = 1.5, xaxt = "n", xlab = "", ylab = 'AUC (95%)',
#      ylim = c(0.5,0.6), las = 1)
# title(main='logistic sgPLS summary: AUC')
# axis(1, at=seq(1,dim(results)[1]), srt = 60, adj = 1, xpd = TRUE,labels = results$feature, las=2, cex.axis=0.7)
# dev.off()
# 
# 
# pdf('Outputs/logistic_sgPLS_summary_AUC_CI.pdf',width=7,height=5)
# par(mar=c(6,5,2,4))
# 
# breaks = c(seq(0.5,0.6,by=0.02), 0.45, round(max(results$AUC),2), 0.65)
# labels = as.character(breaks)
# 
# p2 <- ggplot(data=results, aes(x=feature,y=AUC))+
#   geom_errorbar(aes(ymin= AUC.l,ymax=AUC.h),width=0.2, 
#                 color = ifelse(results$stab>pi,'lightblue','grey')) +
#   geom_point(color = ifelse(results$stab>pi,'royalblue','grey')) +
#   ggtitle(label = 'logistic sgPLS summary: AUC') +
#   scale_y_continuous(limits = c(0.45,0.65), breaks = breaks, labels = labels,
#                      name = "AUC in test data") +
#   geom_hline(yintercept = max(results$AUC), color = 'red',alpha=0.5,linetype=2) + 
#   theme_bw()
# 
# p2 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=6))
# dev.off()
# 
# ## plot proportion selected -------
# 
# pdf('Outputs/logistic_sgPLS_summary_prop.pdf',width=7,height=4)
# par(mar=c(6,5,2,4))
#   
# p3 <- ggplot(data=results, aes(x=feature,y=stab)) +
#   geom_bar(stat = "identity", width = 0.05, 
#            color = ifelse(results$stab>pi,'royalblue','lightblue')) +
#   geom_hline(yintercept = pi, color = 'black', alpha=0.5, linetype=2) +
#   ylim(0.0,1.0) +
#   ggtitle(label = 'logistic sgPLS summary: Selection proportion') +
#   ylab('Selection prop.') +
#   theme_light()
# 
# p3 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=6))
# dev.off()
#    
# 
