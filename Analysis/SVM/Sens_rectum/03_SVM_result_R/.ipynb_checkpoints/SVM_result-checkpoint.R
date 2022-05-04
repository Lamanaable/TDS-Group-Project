# load packages----
library(ggplot2);
library(tidyverse);
library(caTools);
library(dplyr);
library(e1071);
library(GGally);
library(ROCR);
library(caret);

library(sjPlot);
#library(sjlabelled)
#library(sjmisc)

# load data------
setwd("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/SVM/Sens_colon")
train = readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/SVM/Sens_colon/train_5_year.rds')
test = readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/SVM/Sens_colon/test_5_year.rds')

Y_train = train[,'cc_status'] 
Y_test = test[,'cc_status'] 

X_train = as.data.frame(train[,2:ncol(train)])
X_test = as.data.frame(test[,2:ncol(test)])

costs <- table(Y_train)
costs[1] <- 1 
costs[2] <- 2
costs

# train model -------
{'C': 0.2, 'gamma': 0.003, 'kernel': 'rbf'}

m0_= svm(formula = Y_train ~ ., data = X_train, type = 'C-classification', 
         class.weights = costs, probability = TRUE)

m2_= svm(formula = Y_train ~ ., data = X_train, type = 'C-classification',
         kernel = 'radial', class.weights = costs,
         cost=0.4, gamma=0.001, probability = TRUE)

y_pred_prob_m0 = predict(m0_, newdata = X_test, probability=TRUE)
y_pred_prob_m2 = predict(m2_, newdata = X_test, probability=TRUE)

y_pred_prob_m0 = attr(y_pred_prob_m0 , "probabilities")[,2]
y_pred_prob_m2 = attr(y_pred_prob_m2 , "probabilities")[,2]

y_pred_prob_m0_bi <- predict(m0_, newdata = X_test)
y_pred_prob_m2_bi <- predict(m2_, newdata = X_test)

## Visualization------
# variable importance

w <- as.data.frame(t(t(m0_$coefs) %*% m0_$SV))
w$feature = rownames(w)
w <- w %>% rename(coef=V1)
w <- w[order(abs(w$coef),decreasing=TRUE),]
w <- w[1:10,]
w <- w[order(abs(w$coef)),]

w$feature <- as.character(w$feature)
w$feature <- factor(w$feature, levels=unique(w$feature))

vars = read.csv('/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Variable_definition/vars_lookup.csv')
vars$Oldname <- as.character(vars$Oldname)
vars$Newname <- as.character(vars$Newname)
vars$feature <- vars$Oldname
vars = vars[vars$Oldname %in% w$feature,]
head(vars)

# Rename columns of the dataset
df = w # can change dataset to which you want to change the names
df$index <- 1:(dim(df)[1])

df <- merge(df,vars[,c(3,5)],by='feature')
df <- df[order(df$index),]
rownames(df)= df$index

# remove underscores
df$feature <- gsub(x = df$Newname, pattern = "_", replacement = " ")

# see what has been changed

## plot mean beta-------
w <- df

w$Newname <- as.character(w$Newname)
w$Newname <- factor(w$Newname, levels=unique(w$Newname))

pdf('Outputs/SVM_Variable_Importance.pdf',width=6,height=4)
ggplot(data=w, aes(x=Newname,y=abs(coef))) +
  geom_bar(stat = "identity", fill=ifelse(w$coef >0,'indianred','royalblue')) +
  labs(x='Features',y='Coefficients',title='Variable Importance for Linear SVM') +
  coord_flip() +
  theme_light() +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))
dev.off()

# Confusion matrix --------------------------------------------------------

library(caret)
# confusion table
str(y_pred_prob_m0_bi)
Y_test <- as.factor(Y_test)
str(Y_test)
cm0 = confusionMatrix(y_pred_prob_m0_bi, Y_test, positive="1")
cm2 = confusionMatrix(y_pred_prob_m2_bi, Y_test, positive="1")

cm_SVM_PCA = cbind(cm0$byClass,cm2$byClass) 
cm_SVM_PCA <- as.data.frame(cm_SVM_PCA)
cm_SVM_PCA <- rbind(cm_SVM_PCA,c(cm0$overall[1],m2$overall[1]))
names(cm_SVM_PCA) = c('linear','radial')
rownames(cm_SVM_PCA)[12] <- 'Accuracy'
cm_SVM_PCA

#AUC CI------------
library(pROC)
set.seed(2)
auc_ci_linear<-ci.auc(Y_test, y_pred_prob_m0, boot.n=1000) 
auc_ci_radial<-ci.auc(Y_test, y_pred_prob_m2, boot.n=1000)

auc_ci_linear
auc_ci_radial

AUC.l <- c(auc_ci_linear[1],
           auc_ci_radial[1])
AUC <- c(auc_ci_linear[2],
         auc_ci_radial[2])
AUC.h <- c(auc_ci_linear[3],
           auc_ci_radial[3])
cm_SVM_PCA <- rbind(cm_SVM_PCA,AUC.l,AUC,AUC.h)
rownames(cm_SVM_PCA)[13:15] = c('AUC.l','AUC','AUC.h')

saveRDS(cm_SVM_PCA,'../Results_SVM/PCA_SVM_summary.rds')
