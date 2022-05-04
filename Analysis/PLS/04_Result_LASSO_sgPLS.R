
# Set.up-----------

rm(list=ls())
# Loading Data
CRC_ordered <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_5_year_ordered_V3.rds')
Stab_sgPLS <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/PLS/Outputs/stab_selection_sgPLS_selprop.rds')
names <- rownames(Stab_sgPLS)
Stab_sgPLS <- as.vector(Stab_sgPLS)
names(Stab_sgPLS) <- names

Stab_sgPLS <- sort(Stab_sgPLS[Stab_sgPLS>pi],decreasing = TRUE)

LASSO <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/Stability_selection/5_year/Outputs/stab_selection_lasso_selprop.rds')

n = 4.000
alpha = 0.100 
pi=0.9

# Loading packages
suppressPackageStartupMessages(library(VennDiagram))
library(tidyverse)
dir.create("Figures", showWarnings = FALSE)

# Variables
sgPLS <- names(Stab_sgPLS[Stab_sgPLS>pi])
LASSO <- names(LASSO[LASSO>pi])
dput(intersect(sgPLS,LASSO))

# Venn plot-------------

vars=list(sgPLS=sgPLS, LASSO=LASSO)
colors=c("forestgreen", "skyblue")
names=paste0(c("sgPLS", "LASSO"), " (N=", unlist(lapply(vars, length)), ")")
venn.diagram(vars, filename = "Figures/Venn.png", 
             fill=colors, category.names = names, 
             main='Venn Diagram of Feature Selection',imagetype = "png")

library(RColorBrewer)
myCol <- c("forestgreen", "skyblue")

v <- venn.diagram(vars, filename = NULL, 
                  lwd = 2,
                  lty = 'blank',
                  fill = myCol, category.names = names, 
                  height = 360 , 
                  width = 480 , 
                  resolution = 480,
                  main='Venn Diagram of Feature Selection',imagetype = "png",
                  cat.cex = 1,
                  cex = 0.6,
                  cat.dist = c(-0.4, -0.2),
                  cat.pos = c(5, 8))

grid.newpage()
grid.draw(v)

# have a look at the names in the plot object v
lapply(v,  names)
# We are interested in the labels
lapply(v, function(i) i$label)

# Over-write labels (5 to 7 chosen by manual check of labels)
#v[[5]]$label <- paste(setdiff(LASSO,sgPLS),collapse="\n")
#v[[6]]$label <- paste(setdiff(sgPLS,LASSO),collapse="\n")
v[[7]]$label <- paste(intersect(LASSO,sgPLS),collapse="\n")
#v[[9]]$label <- paste(setdiff(LASSO,union(union(sPLS,sgPLS),Enet)))  
#setdiff(intersect(LASSO,Enet),union(sPLS,sgPLS))
#names = c("occp_home","comorb_AHD","comorb_Gastritis_duodenitis",
#          "Model_Edu_HighSchool","comorb_R0_Circ_Res","comorb_Gastric_ulcer","comorb_Liver")
#v[[10]]$label <- paste(names,collapse="\n")
#v[[11]]$label <- paste(setdiff(Enet,union(union(sPLS,sgPLS),LASSO))) 
#setdiff(intersect(intersect(LASSO,Enet),sPLS),sgPLS)
#names = c("time_TV","ops_appendicectomy","meds_statins","comorb_Ear","Model_SmokePackYr",
#          "Smoking_Previous","medication_glucosamine","medication_BetaBlocker" )
#v[[13]]$label <- paste(names,collapse="\n") 
#v[[14]]$label <- paste(intersect(intersect(intersect(LASSO,Enet),sPLS),sgPLS),collapse="\n")
#v[[17]]$label <- paste(setdiff(sPLS,union(union(LASSO,Enet),sgPLS)),collapse="\n")  
#v[[23]]$label <- paste(setdiff(intersect(sPLS,sgPLS),union(LASSO,Enet)),collapse="\n")

# plot  
grid.newpage()
png("Figures/Venn_Labels.png",width=8,height=8,units="in",res=800)
pdf("Figures/Venn_Labels.pdf")
grid.draw(v)
dev.off()

## adjusted result matrix-------
LASSO <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/Stability_selection/5_year/Outputs/stab_selection_lasso_selprop.rds')
LASSO <- sort(LASSO[LASSO>pi],decreasing = TRUE)
stab_sgPLS <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/PLS/Outputs/stab_selection_sgPLS_selprop.rds')
names <- rownames(stab_sgPLS)
stab_sgPLS <- as.vector(stab_sgPLS)
names(stab_sgPLS) <- names
stab_sgPLS <- sort(stab_sgPLS[stab_sgPLS>pi],decreasing = TRUE)

inter <- intersect(names(LASSO),names(stab_sgPLS))

train = CRC_ordered[CRC_ordered$tt_status=='train',]
test = CRC_ordered[CRC_ordered$tt_status=='test',]

X_train = train[,colnames(train) %in% inter]
Y_train = train$cc_status

X_test = test[,colnames(test) %in% inter]
Y_test = test$cc_status
covars_train = train[,c('age_recr','family_history_CRC')]
covars_test = test[,c('age_recr','family_history_CRC')]

fit <- glm(Y_train ~ ., family='binomial',data = cbind(covars_train,X_train))

y_pred = predict(fit,newdata = cbind(covars_test,X_test),type='response')

AUC.CI = ci.auc(Y_test, y_pred,boot.n=1000,parallel=TRUE) 

AUC <- AUC.CI[2]
AUC.l <- AUC.CI[1]
AUC.h <- AUC.CI[3]

ROC <- plot.roc(roc(Y_test,y_pred))
thr<- coords(ROC, "best", ret="threshold", best.method="youden")
thr

library(caret)
y_pred_bi <- as.factor(ifelse(y_pred>0.36,1,0)) #?
cm = confusionMatrix(y_pred_bi, Y_test, positive="1")
cm

sum = NULL
sum = c(cm$byClass,cm$overall[1])

# ROC curve ---------------------------------------------------------------

#AUC CI
library(pROC)
set.seed(2)
auc_ci<-ci.auc(Y_test, y_pred, boot.n=1000)

AUC.l <- auc_ci[1]
AUC <- auc_ci[2]
AUC.h <- auc_ci[3]

sum <- c(sum,AUC.l,AUC,AUC.h)
names(sum)[13:15] = c('AUC.l','AUC','AUC.h')
sum
saveRDS(sum,'Summary_adjusted_lasso_sgPLS/PLS_logistic_final.rds')


## simple result matrix-------
LASSO <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/Stability_selection/5_year/Outputs/stab_selection_lasso_selprop.rds')
LASSO <- sort(LASSO[LASSO>pi],decreasing = TRUE)
stab_sgPLS <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/PLS/Outputs/stab_selection_sgPLS_selprop.rds')
names <- rownames(stab_sgPLS)
stab_sgPLS <- as.vector(stab_sgPLS)
names(stab_sgPLS) <- names
stab_sgPLS <- sort(stab_sgPLS[stab_sgPLS>pi],decreasing = TRUE)

inter <- intersect(names(LASSO),names(stab_sgPLS))

train = CRC_ordered[CRC_ordered$tt_status=='train',]
test = CRC_ordered[CRC_ordered$tt_status=='test',]

X_train = train[,colnames(train) %in% inter]
Y_train = train$cc_status

X_test = test[,colnames(test) %in% inter]
Y_test = test$cc_status

fit <- glm(Y_train ~ ., family='binomial',data = X_train)

y_pred = predict(fit,newdata = X_test,type='response')

AUC.CI = ci.auc(Y_test, y_pred,boot.n=1000,parallel=TRUE) 

AUC <- AUC.CI[2]
AUC.l <- AUC.CI[1]
AUC.h <- AUC.CI[3]

# Metrics -----------------------------------------------------------------

ROC <- plot.roc(roc(Y_test,y_pred))
thr <- coords(ROC, "best", ret="threshold", best.method="youden")
thr

library(caret)
y_pred_bi <- as.factor(ifelse(y_pred > 0.365,1,0))
cm = confusionMatrix(y_pred_bi, Y_test, positive="1")

sum = NULL
sum = c(cm$byClass,cm$overall[1])

# ROC curve ---------------------------------------------------------------

#AUC CI
library(pROC)
set.seed(2)
auc_ci<-ci.auc(Y_test, y_pred, boot.n=1000)

AUC.l <- auc_ci[1]
AUC <- auc_ci[2]
AUC.h <- auc_ci[3]

sum <- c(sum,AUC.l,AUC,AUC.h)
names(sum)[13:15] = c('AUC.l','AUC','AUC.h')
sum
saveRDS(sum,'Summary_lasso_sgPLS/lasso_PLS_logistic_final.rds')

sens <- coords(ROC, "best", ret="all", best.method="youden")['sensitivity']
minus_spc <- coords(ROC, "best", ret="all", best.method="youden")['1-specificity']
prec <- coords(ROC, "best", ret="all", best.method="youden")['precision']
recall <- coords(ROC, "best", ret="all", best.method="youden")['recall']
pred_ROCR <- prediction(y_pred, Y_test)
roc_ROCR <- performance(pred_ROCR, 'tpr','fpr')
auc_ROCR <- performance(pred_ROCR, measure = "auc")
auc_ROCR <- performance(pred_ROCR, measure = "auc")

pdf('Summary_lasso_sgPLS/lasso_PLS_logistic_final_ROC.pdf')
plot(roc_ROCR, colorize = TRUE,main='ROC for logistic regression on lasso-sgPLS variables')
abline(a = 0, b = 1)
text(x=0.9,y=0.05, labels=paste0('AUC: ',round(auc_ROCR@y.values[[1]],3)))
points(x=minus_spc,y=sens)
text(x=minus_spc-0.1,y=sens+0.05,labels=paste0('Precision: ',round(prec,3),'\n',
                                               'Recall: ',round(recall,3)))
dev.off()

## sgPLS result-------
stab_sgPLS <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/PLS/Outputs/stab_selection_sgPLS_selprop.rds')
names <- rownames(stab_sgPLS)
stab_sgPLS <- as.vector(stab_sgPLS)
names(stab_sgPLS) <- names
stab_sgPLS <- sort(stab_sgPLS[stab_sgPLS>pi],decreasing = TRUE)

train = CRC_ordered[CRC_ordered$tt_status=='train',]
test = CRC_ordered[CRC_ordered$tt_status=='test',]

X_train = train[,colnames(train) %in% names(stab_sgPLS)]
Y_train = train$cc_status

X_test = test[,colnames(test) %in% names(stab_sgPLS)]
Y_test = test$cc_status

fit <- glm(Y_train ~ ., family='binomial',data = X_train)
y_pred = predict(fit,newdata = X_test, type='response')

set.seed(2)
AUC.CI = ci.auc(Y_test, y_pred,boot.n=1000,parallel=TRUE) 

AUC <- AUC.CI[2]
AUC.l <- AUC.CI[1]
AUC.h <- AUC.CI[3]


# Metrics -----------------------------------------------------------------

ROC <- plot.roc(roc(Y_test,y_pred))
thr <- coords(ROC, "best", ret="threshold", best.method="youden")
thr

library(caret)
y_pred_bi <- as.factor(ifelse(y_pred > 0.360,1,0))
cm = confusionMatrix(y_pred_bi, Y_test, positive="1")
cm

sum = NULL
sum = c(cm$byClass,cm$overall[1])

# ROC curve ---------------------------------------------------------------


#AUC CI
library(pROC)
set.seed(2)
auc_ci<-ci.auc(Y_test, y_pred, boot.n=1000)

AUC.l <- auc_ci[1]
AUC <- auc_ci[2]
AUC.h <- auc_ci[3]

sum <- c(sum,AUC.l,AUC,AUC.h)
names(sum)[13:15] = c('AUC.l','AUC','AUC.h')
sum
saveRDS(sum,'Summary_lasso_sgPLS/PLS_logistic_final.rds')

sens <- coords(ROC, "best", ret="all", best.method="youden")['sensitivity']
minus_spc <- coords(ROC, "best", ret="all", best.method="youden")['1-specificity']
prec <- coords(ROC, "best", ret="all", best.method="youden")['precision']
recall <- coords(ROC, "best", ret="all", best.method="youden")['recall']
pred_ROCR <- prediction(y_pred, Y_test)
roc_ROCR <- performance(pred_ROCR, 'tpr','fpr')
auc_ROCR <- performance(pred_ROCR, measure = "auc")
auc_ROCR <- performance(pred_ROCR, measure = "auc")

pdf('Summary_lasso_sgPLS/PLS_logistic_final_ROC.pdf')
plot(roc_ROCR, colorize = TRUE,main='ROC for logistic regression on sgPLS variables')
abline(a = 0, b = 1)
text(x=0.9,y=0.05, labels=paste0('AUC: ',round(auc_ROCR@y.values[[1]],3)))
points(x=minus_spc,y=sens)
text(x=minus_spc-0.1,y=sens+0.05,labels=paste0('Precision: ',round(prec,3),'\n',
                                              'Recall: ',round(recall,3)))
dev.off()

