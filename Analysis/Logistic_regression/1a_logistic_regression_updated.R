# setup -------------------------------------------------------------------
rm(list=ls())

library(tidyverse)
library(ROCR)
library(pROC)
library(caret)
library(GGally)

setwd("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Logistic_regression")


# load data ---------------------------------------------------------------
#5yr dataset
CRC_data <- readRDS("/rds/general/user/syl416/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_5_year_logistic.rds")
CRC_adjust <- readRDS("/rds/general/user/syl416/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_5_year_ordered_V3.rds")

CRC_data$age_recr <- CRC_adjust$age_recr
CRC_data$family_history_CRC <- CRC_adjust$family_history_CRC


# variable distribution- Model_smoking_pack_year  ---------------------------------------------------
hist(CRC_data$Smoking_pack_year)

par(mfrow=c(2,2), mar=c(2,2,1,1))

#smoking_pack_year looks positively skewed
qqnorm(CRC_data$Smoking_pack_year, 
       main = "Smoking pack year - Normal Q-Q Plot");
qqline(CRC_data$Smoking_pack_year)

#smoking_pack_year sqrt transformation - best one
smok_sqrt <- sqrt(CRC_data$Model_smoking_pack_year)
qqnorm(smok_sqrt, 
       main = "Smoking pack year - sqrt");
qqline(smok_sqrt)

# #smoking_pack_year log transformation
# smok_log <- ifelse(CRC_data$Model_smoking_pack_year==0, 
#                    CRC_data$Model_smoking_pack_year+0.0001, 
#                    CRC_data$Model_smoking_pack_year)
# smok_log <- log(smok_log)
# qqnorm(smok_log, 
#        main = "Smoking pack year - log");
# qqline(smok_log)
# 
# #smoking_pack_year reciprocal transformation
# smok_recip <- ifelse(CRC_data$Model_smoking_pack_year==0, 
#                    CRC_data$Model_smoking_pack_year+0.0001, 
#                    CRC_data$Model_smoking_pack_year)
# smok_recip <- 1/smok_recip
# qqnorm(smok_recip, 
#        main = "Smoking pack year - reciprocal");
# qqline(smok_recip)
# 
# #smoking_pack_year cube root transformation
# smok_cuberoot <- CRC_data$Model_smoking_pack_year^1/3
# qqnorm(smok_cuberoot, 
#        main = "Smoking pack year - cube root");
# qqline(smok_cuberoot)


# variable distribution- age_recr -----------------------------------------
#age_recr looks heavy tailed
qqnorm(CRC_data$age_recr, 
       main = "Age - Normal Q-Q Plot");
qqline(CRC_data$age_recr)

#age_recr cube transformation - best one
age_cube <- (CRC_data$age_recr)^3
qqnorm(age_cube, 
       main = "Age - cube");
qqline(age_cube)

# #age_recr log transformation
# age_log <- log(CRC_data$age_recr)
# qqnorm(age_log, 
#        main = "Age - log");
# qqline(age_log)
# 
# #age_recr reciprocal transformation
# age_recip <- 1/(CRC_data$age_recr)
# qqnorm(age_recip, 
#        main = "Age - reciprocal");
# qqline(age_recip)
# 
# #age_recr square transformation
# age_square <- (CRC_data$age_recr)^2
# qqnorm(age_square, 
#        main = "Age - square");
# qqline(age_square)
# 
# #age_recr sqrt transformation
# age_sqrt <- sqrt(CRC_data$age_recr)
# qqnorm(age_sqrt, 
#        main = "Age - sqrt");
# qqline(age_sqrt)
# 
# #age_recr cube log transformation
# age_cube_log <- log(((CRC_data$age_recr)^3))
# qqnorm(age_cube_log, 
#        main = "Age - cube, log");
# qqline(age_cube_log)
# 
# #age_recr cube reciprocal transformation
# age_cube_recip <- 1/((CRC_data$age_recr)^3)
# qqnorm(age_cube_recip, 
#        main = "Age - cube, reciprocal");
# qqline(age_cube_recip)
# 
# #age_recr cube sqrt transformation
# age_cube_sqrt <- sqrt(((CRC_data$age_recr)^3))
# qqnorm(age_cube_sqrt, 
#        main = "Age - cube, sqrt");
# qqline(age_cube_sqrt)


# correlation between pairs -----------------------------------------------
# CRC_data_correlation <- CRC_data %>%
#   select(BMI, Height, Smoking_pack_year, Sedentary, Alcohol,
#          Education, Diabetes, Low_folate, Low_fruit, Low_veg,
#          Greater_red_meat, Greater_processed_meat, Lower_fibre,
#          Lower_Ca, Asprin, NSAIDS, age_recr, family_history_CRC)
CRC_data_correlation <- CRC_data %>%
    select(Height, Smoking_pack_year, age_recr)

ggpairs(CRC_data_correlation)


# test and train datasets -------------------------------------------------
train <- CRC_data[CRC_data$tt_status=="train",]
test <- CRC_data[CRC_data$tt_status=="test",]


# logistic regression train -----------------------------------------------
#transformed version
# log_reg_train <- glm(cc_status ~ 
#                                 Model_BMI_cat +
#                                 Model_height +
#                                 sqrt(Model_smoking_pack_year) +
#                                 Model_Sedentary +
#                                 Model_Alcohol_more +
#                                 Model_Education +
#                                 Model_Diabetes +
#                                 Model_Low_folate +
#                                 Model_Low_fruit +
#                                 Model_Low_veg +
#                                 Model_Greater_red_meat +
#                                 Model_Greater_processed_meat +
#                                 Model_Lower_fibre +
#                                 Model_Lower_Ca +
#                                 Model_Asprin +
#                                 Model_NSAID +
#                                 age_recr^3 + `family_history_father_20107_Bowel cancer` +
#                                 `family_history_mother_20110_Bowel cancer` + 
#                                 `family_history_sibling_20111_Bowel cancer`,
#                               data = train,
#                               family = "binomial")
# summary(log_reg_train)

#not transformed version
log_reg_train <- glm(cc_status ~
                       BMI +
                       Height +
                       Smoking_pack_year +
                       Sedentary +
                       Alcohol +
                       Education +
                       Diabetes +
                       Low_folate +
                       Low_fruit +
                       Low_veg +
                       Greater_red_meat +
                       Greater_processed_meat +
                       Lower_fibre +
                       Lower_Ca +
                       Asprin +
                       NSAIDS +
                       age_recr +
                       family_history_CRC,
                     data = train,
                     family = "binomial")
summary(log_reg_train)


# forest plot -------------------------------------------------------------
results_log_reg_train <- cbind(exp(coef(log_reg_train)), exp(confint(log_reg_train)))
colnames(results_log_reg_train) <- c('OR','conf.low','conf.high')
results_log_reg_train <- data.frame(results_log_reg_train)
results_log_reg_train$feature <- rownames(results_log_reg_train)

results_log_reg_train_selected <- results_log_reg_train[2:21,]
results_log_reg_train_selected$feature <- c("BMI (subhealthy)", "BMI (obese)",
                                               "Height (cm)", "Smoking pack years",
                                               "Sedentary", "Alcohol (at least daily)",
                                            "Alcohol (at least once a week)", "Education (high school)",
                                            "Education (other professions)", "Education (college/university)",
                                            "Diabetes", "Lower folate", "Lower fruit", "Lower vegetables",
                                            "Greater red meat", "Greater processed meat",
                                            "Lower fibre", "Lower calcium", "Aspirin", "NSAIDS")
results_log_reg_train_selected$feature <- factor(results_log_reg_train_selected$feature, 
                                                 levels=results_log_reg_train_selected$feature)
head(results_log_reg_train_selected, 20)
OR_list <- format(round(results_log_reg_train_selected$OR,digits=2), nsmall=2)
conf_low_list <- format(round(results_log_reg_train_selected$conf.low,digits=2), nsmall=2)
conf_high_list <- format(round(results_log_reg_train_selected$conf.high,digits=2), nsmall=2)
conf_lowhigh_list <- paste(conf_low_list, conf_high_list, sep=",")
text1 <- rep("(95% CI: ", 20)
text2 <- rep(")", 20)
conf_final_list <- paste(text1, conf_lowhigh_list, text2, sep="")
OR_confint_list <- paste(OR_list, conf_final_list, sep=" ")
OR_confint_list

ggplot(data=results_log_reg_train_selected, aes(x = feature, y = OR,
                                         ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 1, color = 'red',alpha=0.5,linetype=2) +
  geom_point() +
  geom_linerange() +
  scale_x_discrete(limits=rev(levels(results_log_reg_train_selected$feature))) +
  annotate(geom="text", x=1:20, y=4, label=rev(OR_confint_list), hjust=1) +
  coord_flip() +
  theme_bw() +
  ylab("Odds Ratio (95% CI)") +
  xlab("Variable") +
  ggtitle('Adjusted Odds Ratio - Baseline Model') +
  ggsave(path="Results", filename="train_adjusted_log_reg_5yr_FINAL.png")


# predict on test set -----------------------------------------------------
predict_test <- predict(log_reg_train, type="response", newdata=test)
predict_bi <- factor(ifelse(predict_test>0.5, 1, 0))

# confusion matrix --------------------------------------------------------
confusionMatrix(predict_bi, test$cc_status, positive="1")

#ROC <- plot.roc(roc(Y_test,y_pred))
#Y_test is test$cc_status
#y_pred is predict_test

ROC <- plot.roc(roc(test$cc_status, predict_test))
coords(ROC, "best", ret="threshold", best.method="youden")

# confusion_matrix <- table(test$cc_status, predict_test > 0.5)
# confusion_matrix


# ROC and AUC --------------------------------------------------------
#AUC
ROCRpred = prediction(predict_test, test$cc_status)
as.numeric(performance(ROCRpred, "auc")@y.values)

#plot ROC
par(mfrow=c(1,1))
perf <- performance(ROCRpred,"tpr","fpr")
auc_ROCR <- performance(ROCRpred, measure = "auc")
plot(perf,colorize=TRUE, xlab="False positive rate", ylab="True positive rate") 
title(main = "ROC curve: logistic regression (baseline)")
abline(a = 0, b = 1, lty="dashed")
text(x=0.9,y=0.0,paste0('AUC: ',round(auc_ROCR@y.values[[1]],3)))


#AUC CI
set.seed(1)
ci.auc(test$cc_status, predict_test, method="bootstrap", boot.stratified=T, boot.n=1000)



# sensitivity analysis- colon --------------------------------------------------
#Colon
CRC_colon_only <- readRDS("/rds/general/user/syl416/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Sensitivity_Data/CRC_colon_only.rds")

#colon train dataset based on eid
colon_train <- CRC_colon_only[CRC_colon_only$tt_status=="train",]
colon_train_eid <- rownames(colon_train)
CRC_data_colon_train <- CRC_data[c(colon_train_eid),]

#colon test dataset based on eid
colon_test <- CRC_colon_only[CRC_colon_only$tt_status=="test",]
colon_test_eid <- rownames(colon_test)
CRC_data_colon_test <- CRC_data[c(colon_test_eid),]

#colon logistic regression
log_reg_CRC_data_colon_train <- glm(cc_status ~
                       BMI +
                       Height +
                       Smoking_pack_year +
                       Sedentary +
                       Alcohol +
                       Education +
                       Diabetes +
                       Low_folate +
                       Low_fruit +
                       Low_veg +
                       Greater_red_meat +
                       Greater_processed_meat +
                       Lower_fibre +
                       Lower_Ca +
                       Asprin +
                       NSAIDS +
                       age_recr +
                       family_history_CRC,
                     data = CRC_data_colon_train,
                     family = "binomial")
summary(log_reg_CRC_data_colon_train)

results_log_reg_CRC_data_colon_train <- cbind(exp(coef(log_reg_CRC_data_colon_train)), exp(confint(log_reg_CRC_data_colon_train)))
colnames(results_log_reg_CRC_data_colon_train) <- c('OR','conf.low','conf.high')
results_log_reg_CRC_data_colon_train <- data.frame(results_log_reg_CRC_data_colon_train)
results_log_reg_CRC_data_colon_train$feature <- rownames(results_log_reg_CRC_data_colon_train)

results_log_reg_CRC_data_colon_train_selected <- results_log_reg_CRC_data_colon_train[2:21,]
results_log_reg_CRC_data_colon_train_selected$feature <- c("BMI (subhealthy)", "BMI (obese)",
                                            "Height (cm)", "Smoking pack years",
                                            "Sedentary", "Alcohol (at least daily)",
                                            "Alcohol (at least once a week)", "Education (high school)",
                                            "Education (other professions)", "Education (college/university)",
                                            "Diabetes", "Lower folate", "Lower fruit", "Lower vegetables",
                                            "Greater red meat", "Greater processed meat",
                                            "Lower fibre", "Lower calcium", "Aspirin", "NSAIDS")
results_log_reg_CRC_data_colon_train_selected$feature <- factor(results_log_reg_CRC_data_colon_train_selected$feature, 
                                                 levels=results_log_reg_CRC_data_colon_train_selected$feature)
OR_list <- format(round(results_log_reg_CRC_data_colon_train_selected$OR,digits=2), nsmall=2)
conf_low_list <- format(round(results_log_reg_CRC_data_colon_train_selected$conf.low,digits=2), nsmall=2)
conf_high_list <- format(round(results_log_reg_CRC_data_colon_train_selected$conf.high,digits=2), nsmall=2)
conf_lowhigh_list <- paste(conf_low_list, conf_high_list, sep=",")
text1 <- rep("(95% CI: ", 20)
text2 <- rep(")", 20)
conf_final_list <- paste(text1, conf_lowhigh_list, text2, sep="")
OR_confint_list <- paste(OR_list, conf_final_list, sep=" ")
OR_confint_list

ggplot(data=results_log_reg_CRC_data_colon_train_selected, aes(x = feature, y = OR,
                                                ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 1, color = 'red',alpha=0.5,linetype=2) +
  geom_point() +
  geom_linerange() +
  scale_x_discrete(limits=rev(levels(results_log_reg_CRC_data_colon_train_selected$feature))) +
  annotate(geom="text", x=1:20, y=4, label=rev(OR_confint_list), hjust=1) +
  coord_flip() +
  theme_bw() +
  ylab("Odds Ratio (95% CI)") +
  xlab("Variable") +
  ggtitle('Adjusted Odds Ratio - Baseline Model, Colon only') +
  ggsave(path="Results", filename="log_reg_baseline_colon_only.png")

#predict on test set
predict_test <- predict(log_reg_CRC_data_colon_train, type="response", newdata=CRC_data_colon_test)
predict_bi <- factor(ifelse(predict_test>0.5, 1, 0))

#confusion matrix
confusionMatrix(predict_bi, CRC_data_colon_test$cc_status, positive="1")

ROC <- plot.roc(roc(CRC_data_colon_test$cc_status, predict_test))
coords(ROC, "best", ret="threshold", best.method="youden")

#ROC and AUC
#AUC
ROCRpred = prediction(predict_test, CRC_data_colon_test$cc_status)
as.numeric(performance(ROCRpred, "auc")@y.values)

#plot ROC
par(mfrow=c(1,1))
perf <- performance(ROCRpred,"tpr","fpr")
auc_ROCR <- performance(ROCRpred, measure = "auc")
plot(perf,colorize=TRUE, xlab="False positive rate", ylab="True positive rate") 
title(main = "ROC curve: logistic regression (baseline, colon only)")
abline(a = 0, b = 1, lty="dashed")
text(x=0.9,y=0.0,paste0('AUC: ',round(auc_ROCR@y.values[[1]],3)))

#AUC CI
set.seed(1)
ci.auc(CRC_data_colon_test$cc_status, predict_test, method="bootstrap", boot.stratified=T, boot.n=1000)



# sensitivity analysis- rectum --------------------------------------------
#Rectum
CRC_rectum_only <- readRDS("/rds/general/user/syl416/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Sensitivity_Data/CRC_rectum_only.rds")

#rectum train dataset based on eid
rectum_train <- CRC_rectum_only[CRC_rectum_only$tt_status=="train",]
rectum_train_eid <- rownames(rectum_train)
CRC_data_rectum_train <- CRC_data[c(rectum_train_eid),]

rectum_test <- CRC_rectum_only[CRC_rectum_only$tt_status=="test",]
rectum_test_eid <- rownames(rectum_test)
CRC_data_rectum_test <- CRC_data[c(rectum_test_eid),]

#rectum logistic regression
log_reg_CRC_data_rectum_train <- glm(cc_status ~
                                      BMI +
                                      Height +
                                      Smoking_pack_year +
                                      Sedentary +
                                      Alcohol +
                                      Education +
                                      Diabetes +
                                      Low_folate +
                                      Low_fruit +
                                      Low_veg +
                                      Greater_red_meat +
                                      Greater_processed_meat +
                                      Lower_fibre +
                                      Lower_Ca +
                                      Asprin +
                                      NSAIDS +
                                      age_recr +
                                      family_history_CRC,
                                    data = CRC_data_rectum_train,
                                    family = "binomial")
summary(log_reg_CRC_data_rectum_train)

results_log_reg_CRC_data_rectum_train <- cbind(exp(coef(log_reg_CRC_data_rectum_train)), exp(confint(log_reg_CRC_data_rectum_train)))
colnames(results_log_reg_CRC_data_rectum_train) <- c('OR','conf.low','conf.high')
results_log_reg_CRC_data_rectum_train <- data.frame(results_log_reg_CRC_data_rectum_train)
results_log_reg_CRC_data_rectum_train$feature <- rownames(results_log_reg_CRC_data_rectum_train)

results_log_reg_CRC_data_rectum_train_selected <- results_log_reg_CRC_data_rectum_train[2:21,]
results_log_reg_CRC_data_rectum_train_selected$feature <- c("BMI (subhealthy)", "BMI (obese)",
                                                           "Height (cm)", "Smoking pack years",
                                                           "Sedentary", "Alcohol (at least daily)",
                                                           "Alcohol (at least once a week)", "Education (high school)",
                                                           "Education (other professions)", "Education (college/university)",
                                                           "Diabetes", "Lower folate", "Lower fruit", "Lower vegetables",
                                                           "Greater red meat", "Greater processed meat",
                                                           "Lower fibre", "Lower calcium", "Aspirin", "NSAIDS")
results_log_reg_CRC_data_rectum_train_selected$feature <- factor(results_log_reg_CRC_data_rectum_train_selected$feature, 
                                                                levels=results_log_reg_CRC_data_rectum_train_selected$feature)
OR_list <- format(round(results_log_reg_CRC_data_rectum_train_selected$OR,digits=2), nsmall=2)
conf_low_list <- format(round(results_log_reg_CRC_data_rectum_train_selected$conf.low,digits=2), nsmall=2)
conf_high_list <- format(round(results_log_reg_CRC_data_rectum_train_selected$conf.high,digits=2), nsmall=2)
conf_lowhigh_list <- paste(conf_low_list, conf_high_list, sep=",")
text1 <- rep("(95% CI: ", 20)
text2 <- rep(")", 20)
conf_final_list <- paste(text1, conf_lowhigh_list, text2, sep="")
OR_confint_list <- paste(OR_list, conf_final_list, sep=" ")
OR_confint_list

ggplot(data=results_log_reg_CRC_data_rectum_train_selected, aes(x = feature, y = OR,
                                                               ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 1, color = 'red',alpha=0.5,linetype=2) +
  geom_point() +
  geom_linerange() +
  scale_x_discrete(limits=rev(levels(results_log_reg_CRC_data_rectum_train_selected$feature))) +
  annotate(geom="text", x=1:20, y=4, label=rev(OR_confint_list), hjust=1) +
  coord_flip() +
  theme_bw() +
  ylab("Odds Ratio (95% CI)") +
  xlab("Variable") +
  ggtitle('Adjusted Odds Ratio - Baseline Model, Rectum only') +
  ggsave(path="Results", filename="log_reg_baseline_rectum_only.png")

#predict on test set
predict_test <- predict(log_reg_CRC_data_rectum_train, type="response", newdata=CRC_data_rectum_test)
predict_bi <- factor(ifelse(predict_test>0.5, 1, 0))

#confusion matrix
confusionMatrix(predict_bi, CRC_data_rectum_test$cc_status, positive="1")

ROC <- plot.roc(roc(CRC_data_rectum_test$cc_status, predict_test))
coords(ROC, "best", ret="threshold", best.method="youden")

#ROC and AUC
#AUC
ROCRpred = prediction(predict_test, CRC_data_rectum_test$cc_status)
as.numeric(performance(ROCRpred, "auc")@y.values)

#plot ROC
par(mfrow=c(1,1))
perf <- performance(ROCRpred,"tpr","fpr")
auc_ROCR <- performance(ROCRpred, measure = "auc")
plot(perf,colorize=TRUE, xlab="False positive rate", ylab="True positive rate") 
title(main = "ROC curve: logistic regression (baseline, rectum only)")
abline(a = 0, b = 1, lty="dashed")
text(x=0.9,y=0.0,paste0('AUC: ',round(auc_ROCR@y.values[[1]],3)))

#AUC CI
set.seed(1)
ci.auc(CRC_data_rectum_test$cc_status, predict_test, method="bootstrap", boot.stratified=T, boot.n=1000)











