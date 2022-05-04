rm(list=ls())

library(tidyverse)
#install.packages("ROCR")
library(ROCR)
#install.packages("pROC")
library(pROC)

setwd("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Logistic_regression")

#10yr dataset
CRC_data <- readRDS("/rds/general/user/syl416/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_final.rds")

#creating 5 year dataset
# setwd("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration")
# 
# CRC_data$group <- as.character(CRC_data$group)
# length(which(CRC_data$CRC_dvlp_year <=5))
# keep = CRC_data[which(CRC_data$CRC_dvlp_year <=5),]
# group = keep$group
# CRC_data <- CRC_data[CRC_data$group %in% group,]
# saveRDS(CRC_data,'Outputs/CRC_data_final_5_year.rds')

#5yr dataset
CRC_data <- readRDS("/rds/general/user/syl416/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_final_5_year.rds")

#check levels of factors
levels(CRC_data$Model_BMI_cat)

#check distribution of all numeric variables
str(CRC_data)
class(CRC_data$diet_bread)
numeric_test <- data.frame(sapply(CRC_data,is.numeric))
dim(numeric_test)
numeric_test["diet_bread",]
numeric_test["diet_beef_cat",]

colnames(numeric_test)
numeric_test[,2] <- rownames(numeric_test)
colnames(numeric_test)[1] <- "numeric"
colnames(numeric_test)[2] <- "variable"

numeric_true <- numeric_test[numeric_test$numeric,]
true_rownames <- c(numeric_true$variable)

CRC_data_numeric <- CRC_data[,true_rownames]
summary(CRC_data_numeric)


#distribution plots
# par(mfrow=c(3,3), mar=c(2,2,1,1))
# for (k in 1:9) {
#   print(k)
#   plot(hist(CRC_data_numeric[,k]),
#        xlab=colnames(CRC_data_numeric)[k],
#        main=k)
# }

#check distribution of variables in log reg
qqnorm(CRC_data$Model_smoking_pack_year, 
       main = "Smoking pack year - Normal Q-Q Plot");
qqline(CRC_data$Model_smoking_pack_year)

qqnorm(CRC_data$age_recr, 
       main = "Age - Normal Q-Q Plot");
qqline(CRC_data$age_recr)

#check NAs
CRC_data_checkNA <- CRC_data %>%
  select(Model_BMI_cat, Model_height, Model_smoking_pack_year, 
         Model_Sedentary, Model_Alcohol_more, Model_Education,
         Model_Diabetes, Model_Low_folate, Model_Low_fruit,
         Model_Low_veg, Model_Greater_red_meat, Model_Greater_processed_meat,
         Model_Lower_fibre, Model_Lower_Ca, Model_Asprin, Model_NSAID)
sum(is.na(CRC_data_checkNA))


# log reg not adjusted ----------------------------------------------------
##overall- male and female (no adjustments)
log_reg <- glm(cc_status ~ 
                 Model_BMI_cat +
                 Model_height +
                 Model_smoking_pack_year +
                 Model_Sedentary +
                 Model_Alcohol_more +
                 Model_Education +
                 Model_Diabetes +
                 Model_Low_folate +
                 Model_Low_fruit +
                 Model_Low_veg +
                 Model_Greater_red_meat +
                 Model_Greater_processed_meat +
                 Model_Lower_fibre +
                 Model_Lower_Ca +
                 Model_Asprin +
                 Model_NSAID,
               data = CRC_data,
               family = "binomial")
summary(log_reg)


# forest plot not adjusted ------------------------------------------------
#forest plot
results_baseline <- cbind(exp(coef(log_reg)), exp(confint(log_reg)))
colnames(results_baseline) <- c('OR','conf.low','conf.high')
results_baseline <- data.frame(results_baseline)
results_baseline$feature <- rownames(results_baseline)

results_baseline1 <- results_baseline[2:21,]

ggplot(data=results_baseline1, aes(x = feature, y = OR,
                                  ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 1, color = 'red',alpha=0.5,linetype=2) +
  geom_point() +
  geom_linerange() +
  coord_flip() +
  theme_bw() +
  ylab("Coefficient estimate \n and 95% CI") +
  xlab("Regression coefficient") +
  ggtitle('Odds Ratio \n(not adjusted)') +
  ggsave(path="Results", filename="not_adjusted_log_reg.png")




# log reg adjusted --------------------------------------------------------
##adjusted log reg
#In addition, models were adjusted for (study), age, family history, and (endoscopy history)
summary(CRC_data$age_recr)
table(CRC_data$`family_history_father_20107_Bowel cancer`)
table(CRC_data$`family_history_mother_20110_Bowel cancer`)
table(CRC_data$`family_history_sibling_20111_Bowel cancer`)

log_reg_adjusted <- glm(cc_status ~ 
                 Model_BMI_cat +
                 Model_height +
                 Model_smoking_pack_year +
                 Model_Sedentary +
                 Model_Alcohol_more +
                 Model_Education +
                 Model_Diabetes +
                 Model_Low_folate +
                 Model_Low_fruit +
                 Model_Low_veg +
                 Model_Greater_red_meat +
                 Model_Greater_processed_meat +
                 Model_Lower_fibre +
                 Model_Lower_Ca +
                 Model_Asprin +
                 Model_NSAID +
                   age_recr + `family_history_father_20107_Bowel cancer` +
                   `family_history_mother_20110_Bowel cancer` + 
                   `family_history_sibling_20111_Bowel cancer`,
               data = CRC_data,
               family = "binomial")
summary(log_reg_adjusted)


# forest plot adjusted ----------------------------------------------------
#forest plot
results_adjusted <- cbind(exp(coef(log_reg_adjusted)), exp(confint(log_reg_adjusted)))
colnames(results_adjusted) <- c('OR','conf.low','conf.high')
results_adjusted <- data.frame(results_adjusted)
results_adjusted$feature <- rownames(results_adjusted)

results_adjusted1 <- results_adjusted[2:21,]

#forest plot with adjusted variables
ggplot(data=results_adjusted1, aes(x = feature, y = OR,
             ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 1, color = 'red',alpha=0.5,linetype=2) +
  geom_point() +
  geom_linerange() +
  coord_flip() +
  theme_bw() +
  ylab("Coefficient estimate \n and 95% CI") +
  xlab("Regression coefficient") +
  ggtitle('Odds Ratio \n(adjusted)') +
  ggsave(path="Results", filename="adjusted_log_reg.png")

# log reg adjusted with AC ---------------------------------
log_reg_adjusted_AC <- glm(cc_status ~ 
                          Model_BMI_cat +
                          Model_height +
                          Model_smoking_pack_year +
                          Model_Sedentary +
                          Model_Alcohol_more +
                          Model_Education +
                          Model_Diabetes +
                          Model_Low_folate +
                          Model_Low_fruit +
                          Model_Low_veg +
                          Model_Greater_red_meat +
                          Model_Greater_processed_meat +
                          Model_Lower_fibre +
                          Model_Lower_Ca +
                          Model_Asprin +
                          Model_NSAID +
                          age_recr + `family_history_father_20107_Bowel cancer` +
                          `family_history_mother_20110_Bowel cancer` + 
                          `family_history_sibling_20111_Bowel cancer`+ 
                            assessment_centre,
                        data = CRC_data,
                        family = "binomial")
summary(log_reg_adjusted_AC)

#forest plot without adjusted variables and assessment_centre
results_adjusted_AC <- cbind(exp(coef(log_reg_adjusted_AC)), exp(confint(log_reg_adjusted_AC)))
colnames(results_adjusted_AC) <- c('OR','conf.low','conf.high')
results_adjusted_AC <- data.frame(results_adjusted_AC)
results_adjusted_AC$feature <- rownames(results_adjusted_AC)

results_adjusted_AC1 <- results_adjusted_AC[2:21,]

ggplot(data=results_adjusted_AC1, aes(x = feature, y = OR,
                                     ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 1, color = 'red',alpha=0.5,linetype=2) +
  geom_point() +
  geom_linerange() +
  coord_flip() +
  theme_bw() +
  ylab("Coefficient estimate \n and 95% CI") +
  xlab("Regression coefficient") +
  ggtitle('Odds Ratio \n(adjusted_AC)') +
  ggsave(path="Results", filename="adjusted_log_reg_withAC.png")



# training dataset without AC --------------------------------------------------------
train <- CRC_data[CRC_data$tt_status=="train",]
test <- CRC_data[CRC_data$tt_status=="test",]

#adjusted without AC
log_reg_adjusted_train <- glm(cc_status ~ 
                          Model_BMI_cat +
                          Model_height +
                          Model_smoking_pack_year +
                          Model_Sedentary +
                          Model_Alcohol_more +
                          Model_Education +
                          Model_Diabetes +
                          Model_Low_folate +
                          Model_Low_fruit +
                          Model_Low_veg +
                          Model_Greater_red_meat +
                          Model_Greater_processed_meat +
                          Model_Lower_fibre +
                          Model_Lower_Ca +
                          Model_Asprin +
                          Model_NSAID +
                          age_recr + `family_history_father_20107_Bowel cancer` +
                          `family_history_mother_20110_Bowel cancer` + 
                          `family_history_sibling_20111_Bowel cancer`,
                        data = train,
                        family = "binomial")
summary(log_reg_adjusted_train)

#forest plot
results_adjusted_train <- cbind(exp(coef(log_reg_adjusted_train)), exp(confint(log_reg_adjusted_train)))
colnames(results_adjusted_train) <- c('OR','conf.low','conf.high')
results_adjusted_train <- data.frame(results_adjusted_train)
results_adjusted_train$feature <- rownames(results_adjusted_train)

results_adjusted_train1 <- results_adjusted_train[2:21,]

#forest plot with adjusted variables
ggplot(data=results_adjusted_train1, aes(x = feature, y = OR,
                                   ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 1, color = 'red',alpha=0.5,linetype=2) +
  geom_point() +
  geom_linerange() +
  coord_flip() +
  theme_bw() +
  ylab("Coefficient estimate \n and 95% CI") +
  xlab("Regression coefficient") +
  ggtitle('Odds Ratio \n(adjusted)') +
  ggsave(path="Results", filename="train_adjusted_log_reg.png")



# training dataset with AC ------------------------------------------------
#adjusted with AC
log_reg_adjusted_train_withAC <- glm(cc_status ~ 
                                Model_BMI_cat +
                                Model_height +
                                Model_smoking_pack_year +
                                Model_Sedentary +
                                Model_Alcohol_more +
                                Model_Education +
                                Model_Diabetes +
                                Model_Low_folate +
                                Model_Low_fruit +
                                Model_Low_veg +
                                Model_Greater_red_meat +
                                Model_Greater_processed_meat +
                                Model_Lower_fibre +
                                Model_Lower_Ca +
                                Model_Asprin +
                                Model_NSAID +
                                age_recr + `family_history_father_20107_Bowel cancer` +
                                `family_history_mother_20110_Bowel cancer` + 
                                `family_history_sibling_20111_Bowel cancer` +
                                  assessment_centre,
                              data = train,
                              family = "binomial")
summary(log_reg_adjusted_train_withAC)

#forest plot
results_adjusted_train_withAC <- cbind(exp(coef(log_reg_adjusted_train_withAC)), 
                                exp(confint(log_reg_adjusted_train_withAC)))
colnames(results_adjusted_train_withAC) <- c('OR','conf.low','conf.high')
results_adjusted_train_withAC <- data.frame(results_adjusted_train_withAC)
results_adjusted_train_withAC$feature <- rownames(results_adjusted_train_withAC)

results_adjusted_train_withAC1 <- results_adjusted_train_withAC[2:21,]

#forest plot with adjusted variables
ggplot(data=results_adjusted_train_withAC1, aes(x = feature, y = OR,
                                         ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 1, color = 'red',alpha=0.5,linetype=2) +
  geom_point() +
  geom_linerange() +
  coord_flip() +
  theme_bw() +
  ylab("Coefficient estimate \n and 95% CI") +
  xlab("Regression coefficient") +
  ggtitle('Odds Ratio \n(adjusted_AC)') +
  ggsave(path="Results", filename="train_adjusted_log_reg_withAC.png")


# predict on test set without AC -----------------------------------------------------
predict_test <- predict(log_reg_adjusted_train, type="response", newdata=test)
confusion_matrix <- table(test$cc_status, predict_test > 0.5)
confusion_matrix

# ROC (without AC) --------------------------------------------------------
ROCRpred = prediction(predict_test, test$cc_status)
as.numeric(performance(ROCRpred, "auc")@y.values)
#0.5029089

perf <- performance(ROCRpred,"tpr","fpr")
plot(perf,colorize=TRUE) 
abline(a=0, b=1, lty="dashed")

#AUC CI
ci.auc(test$cc_status, predict_test, boot.n=1000)

# predict on test set with AC -----------------------------------------------------
predict_test_withAC <- predict(log_reg_adjusted_train_withAC, type="response", newdata=test)
confusion_matrix_withAC <- table(test$cc_status, predict_test_withAC > 0.5)
confusion_matrix_withAC

# ROC (with AC) --------------------------------------------------------
ROCRpred_withAC = prediction(predict_test_withAC, test$cc_status)
as.numeric(performance(ROCRpred_withAC, "auc")@y.values)
#0.510965

perf_withAC <- performance(ROCRpred_withAC,"tpr","fpr")
plot(perf_withAC,colorize=TRUE) 
abline(a=0, b=1, lty="dashed")

#AUC CI
ci.auc(test$cc_status, predict_test_withAC, boot.n=1000)


# stratified male female --------------------------------------------------
#CRC_data_male <- CRC_data[CRC_data$sex==1,]
#CRC_data_female <- CRC_data[CRC_data$sex==0,]
#exp(cbind(coef(log_reg_male), confint(log_reg_male), 
          #coef(log_reg_female), confint(log_reg_female)))








