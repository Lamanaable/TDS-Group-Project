# 1. Setup -------------------------------------------------------------------

# Package used for date conversion:
rm(list=ls())
library(tidyverse)
library(survival)
setwd("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/PLS")

# 2. Reading the data --------------------------------------------------------
CRC <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Sensitivity_Data/CRC_data_time_status.rds')
Stab_sgPLS <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/PLS/Outputs/stab_selection_sgPLS_selprop.rds')
Stab_sgPLS <- sort(Stab_sgPLS)

# Fitting PLS models---------------------

train = CRC[CRC$tt_status=='train',]
test = CRC[CRC$tt_status=='test',]

X_train = train[,7:ncol(train)]
Y_time = train$time
Y_status = as.numeric(train$status)-1

X_test = test[,7:ncol(test)]
Y_test_time = test$time
Y_test_status = as.numeric(test$status)-1

pi=0.9
keep = names(Stab_sgPLS[Stab_sgPLS>pi])
  
X_train = X_train[,names(X_train) %in% keep]
X_test = X_test[,names(X_test) %in% keep]

print(all(rownames(X_train)==rownames(Y_time)))

surv <- Surv(Y_time,Y_status)
cox <- coxph(surv~.,data=X_train)
predicted_score <-predict(cox, newdata=X_test) 

predicted_score_split <- (predicted_score>quantile(predicted_score,1/3))+(predicted_score>quantile(predicted_score,2/3))

diff = survdiff(Surv(Y_test_time,Y_test_status)~factor(predicted_score_split))
pval = round(broom::glance(diff)$p.value,3)

myCol <- brewer.pal(3, "Set2")

pdf("Outputs/Cox_sgPLS_plot.pdf")
plot(survfit(Surv(Y_test_time,Y_test_status)~factor(predicted_score_split)),col=myCol,
     main='CRC risk Estimated by sgPLS stability selection',
     xlab = 'Year',
     ylab = 'CRC-free survival')
legend("topright",c("Low","Medium","High"),lty=c(1,1,1),col=myCol)
text(x=4,y=0.0, labels= paste0('log-rank test p value: ',pval),cex=0.8)
dev.off()

## intersection on LASSO and sgPLS--------
LASSO <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/Stability_selection/5_year/Outputs/stab_selection_lasso_selprop.rds')
LASSO <- sort(LASSO[LASSO>pi],decreasing = TRUE)
stab_sgPLS <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/PLS/Outputs/stab_selection_sgPLS_selprop.rds')
names = rownames(stab_sgPLS)
stab_sgPLS <- as.vector(stab_sgPLS)
names(stab_sgPLS) <- names
stab_sgPLS <- sort(stab_sgPLS[stab_sgPLS>pi],decreasing = TRUE)

inter <- intersect(names(LASSO),names(stab_sgPLS))

# Fitting PLS models---------------------

train = CRC[CRC$tt_status=='train',]
test = CRC[CRC$tt_status=='test',]

X_train = train[,7:ncol(train)]
Y_time = train$time
Y_status = as.numeric(train$status)-1

X_test = test[,7:ncol(test)]
Y_test_time = test$time
Y_test_status = as.numeric(test$status)-1

X_train = X_train[,names(X_train) %in% inter]
X_test = X_test[,names(X_test) %in% inter]

print(all(rownames(X_train)==rownames(Y_time)))

surv <- Surv(Y_time,Y_status)
cox <- coxph(surv~., data=X_train)

# predicted risk score
predicted_score <- predict(cox, newdata=X_test,type='risk') 

print(quantile(predicted_score,1/2))

predicted_score_split <- predicted_score> quantile(predicted_score,1/2)

diff = survdiff(Surv(Y_test_time,Y_test_status)~factor(predicted_score_split))
pval = round(broom::glance(diff)$p.value,3)
pval

cox_split <- coxph(Surv(Y_test_time,Y_test_status)~predicted_score_split)
exp(coef(cox_split))
exp(confint(cox_split))

# get survival rate at 5 year
surv_fit <- survfit(Surv(Y_test_time,Y_test_status)~factor(predicted_score_split))
summary(surv_fit,times=c(5))

pdf("Summary_lasso_sgPLS/Cox_lasso_sgPLS_plot.pdf")
plot(surv_fit,
     col=c('olivedrab4','lightcoral'),
     main='CRC risk Estimated by lasso-sgPLS stability selection',
     xlab = 'Year',
     ylab = 'CRC-free survival')
legend("topright",c("Low Risk","High Risk"),lty=c(1,1),col=c('olivedrab4','lightcoral'))
text(x=4,y=0.0, labels= paste0('Log-rank test p value: ',pval),cex=0.8)
text(x=4,y=0.04,labels=paste0('Hazard ratio: ', round(exp(coef(cox_split)),2),
                              ' (95% CI: ',round(exp(confint(cox_split))[1],2),
                              ' to ',round(exp(confint(cox_split))[2],2),
                              ')'),cex=0.8)
dev.off()

pdf("Summary_lasso_sgPLS/Cox_lasso_sgPLS_PH_assump.pdf")
plot(surv_fit,col = 2:3,fun='cloglog')
dev.off()

# ROC
library(pROC)
set.seed(2)
auc_ci <- ci.auc(Y_test, predicted_score, boot.n=1000)
AUC.l <- auc_ci[1]
AUC <- auc_ci[2]
AUC.h <- auc_ci[3]

sum <- c(AUC.l,AUC,AUC.h)
sum
saveRDS(sum,'Summary_lasso_sgPLS/Cox_lasso_sgPLS_AUC.rds')

