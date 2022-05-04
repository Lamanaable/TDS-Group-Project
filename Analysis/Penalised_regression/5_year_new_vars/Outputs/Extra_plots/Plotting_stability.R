library(tidyverse)


# Lasso stability analysis under lambda min -------------------------------



lasso_5yr_min <- readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/5_year_new_vars/Outputs/b_5yr_lasso_stability_lambdamin.rds")
# Removing the intercept (although weird that doesn't appear 100% of the time??)
X_row <- which(names(lasso_5yr_min) == "X")
lasso_5yr_min <-lasso_5yr_min [-X_row]

# Looking at names of >0.2 proportion variables
dput(names(lasso_5yr_min [lasso_5yr_min  > 0.2]))



# Manually renaming
var_names_lasso_min<-c("time_TV", "weekly_beer_intake", "biomarker_Neu", "pulse", 
                       "operation_20004_appendicectomy", "occupation_Looking.after.home.and.or.family", 
                       "medication_20003_Statins", "comorbidity_ICD10_I25.1.Atherosclerotic.heart.disease", 
                       "comorbidity_G_Ear", "Model_smoking_pack_year", "comorbidity_K_Gastritis_duodenitis", 
                       "smoking_status_Previous", "medication_20003_glucosamine", "Model_Education_high.school", 
                       "comorbidity_R0_Other_symptoms", "comorbidity_K_Gastric_ulcer", 
                       "medication_20003_BetaBlocker", "diet_cereal", "comorbidity_K7_Liver", 
                       "comorbidity_R1_Other_symptoms", "biomarker_HCT", "biomarker_Tprotein", 
                       "family_history_BreastCancer", "comorbidity_G_Eye_adnexa", "comorbidity_C_D_neoplasms", 
                       "early_life_comparative_height_age_ten_Shorter", "score_housing", 
                       "biomarker_platelet", "diet_non_oily_fish_cat", "ethnicity_cat_Asian", 
                       "comorbidity_K8_Gallbladder", "family_history_CRC", "family_history_HBP", 
                       "biomarker_TRG", "family_history_Diabetes", "comorbidity_N_Renal_Failure", 
                       "Model_Education_college.uni", "family_history_Parkinson", "waist_hip_ratio", 
                       "diet_oily_fish_cat", "Model_Lower_Ca", "comorbidity_J_Respiratory", 
                       "comorbidity_K_other_stomach", "Model_Low_folate", "Model_BMI_cat_0", 
                       "diet_coffee_Decaffeinated", "medication_20003_metformin", "biomarker_urine_K", 
                       "medication_20003_CCB", "medication_vitamin_6155_Vitamin.D", 
                       "smoking_status_Never", "comorbidity_K_Gastrooesophageal_reflux_disease", 
                       "diet_bread_type_Wholemeal.or.wholegrain", "medication_20003_antibiotics", 
                       "weekly_mobile_len_hrs", "early_life_comparative_body_size_age_ten_Plumper", 
                       "diet_alcohol_Previous", "biomarker_lymphocyte", "comorbidity_ICD10_E78.0.Pure.hypercholesterolaemia", 
                       "comorbidity_F_Mental_disorder")


#jpeg(file="b_5yr_lasso_stability_lambdamin.jpeg")
plot(lasso_5yr_min[lasso_5yr_min > 0.2], type = "h", col = "navy",
           lwd = 3, yaxt="n", xaxt = "n", xlab = "", ylab = expression(beta),
           ylim = c(0, 2), las = 1, main="Lasso stability analysis under lambda min (5 year)")
axis(side = 2, at = c(0,0.5,1.0))

text(lasso_5yr_min[lasso_5yr_min > 0.2] + 0.07, 
     labels = var_names_lasso_min, pos = 4,offset=-0.1, srt = 90, cex = 0.7)








# Lasso stability analysis under lambda 1se -------------------------------

lasso_5yr_1se <- readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/5_year_new_vars/Outputs/b_5yr_lasso_stability_lambda1se.rds")

# Removing the intercept (although weird that doesn't appear 100% of the time??)
X_row <- which(names(lasso_5yr_1se) == "X")
lasso_5yr_1se<-lasso_5yr_1se[-X_row]

# Looking at names of >0.2 proportion variables
dput(names(lasso_5yr_1se[lasso_5yr_1se > 0.2]))

# Manually renaming
var_names_lasso_1se<-c("Diet - beer", "Smokings packs", "Pulse", "TV time", 
                   "Medication - statins", "Biomarker - neu")
  
  
# Plot
plot(lasso_5yr_1se[lasso_5yr_1se > 0.2], type = "h", col = "navy",
     lwd = 3, yaxt="n", xaxt = "n", xlab = "", ylab = expression(beta),
     ylim = c(0, 1.2), las = 1, main="Lasso stability analysis under lambda 1se (5 year)")
axis(side = 2, at = c(0,0.5,1.0))

text(lasso_5yr_1se[lasso_5yr_1se > 0.2] + 0.07, 
     labels = var_names_lasso_1se, pos = 4,offset=-0.1, srt = 90, cex = 0.7)





# Enet stability analysis under lambda 1se -------------------------------

enet_5yr_1se <- readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/5_year_new_vars/Outputs/c_enet_5yr_stability_lambda1se.rds")

# Removing the intercept (although weird that doesn't appear 100% of the time??)
X_row <- which(names(enet_5yr_1se) == "X")
enet_5yr_1se<-enet_5yr_1se[-X_row]

# Looking at names of >0.2 proportion variables
dput(names(enet_5yr_1se[enet_5yr_1se > 0.2]))

# Manually renaming
var_names_enet_1se<-c("weekly_beer_intake", "Model_smoking_pack_year", "pulse", "medication_20003_Statins", 
                      "time_TV", "biomarker_Neu", "smoking_status_Never", "smoking_status_Previous", 
                      "comorbidity_G_Ear", "operation_20004_appendicectomy", "waist_hip_ratio", 
                      "medication_20003_glucosamine")



plot(enet_5yr_1se[enet_5yr_1se > 0.2], type = "h", col = "navy",
     lwd = 3, yaxt="n", xaxt = "n", xlab = "", ylab = expression(beta),
     ylim = c(0, 1.4), las = 1, main="Elastic net stability analysis under lambda 1se (5 year)")
axis(side = 2, at = c(0,0.5,1.0))

text(enet_5yr_1se + 0.07, 
     labels = var_names_enet_1se, pos = 4, offset=-0.35, srt = 90, cex = 0.7)





# Enet stability analysis under lambda min -------------------------------

enet_5yr_min <- readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/5_year_new_vars/Outputs/c_enet_5yr_stability_lambdamin.rds")
# Removing the intercept (although weird that doesn't appear 100% of the time??)
X_row <- which(names(enet_5yr_min) == "X")
enet_5yr_min<-enet_5yr_min[-X_row]

# Looking at names of >0.2 proportion variables
dput(names(enet_5yr_min[enet_5yr_min > 0.2]))

# Manually renaming
var_names_enet_min<-c("Model_smoking_pack_year", "time_TV", "weekly_beer_intake", 
                      "biomarker_Neu", "pulse", "medication_20003_Statins", "operation_20004_appendicectomy", 
                      "comorbidity_ICD10_I25.1.Atherosclerotic.heart.disease", "smoking_status_Previous", 
                      "occupation_Looking.after.home.and.or.family", "comorbidity_G_Ear", 
                      "comorbidity_K_Gastritis_duodenitis", "Model_Education_high.school", 
                      "medication_20003_glucosamine", "medication_20003_BetaBlocker", 
                      "comorbidity_R0_Other_symptoms", "diet_cereal", "comorbidity_K_Gastric_ulcer", 
                      "comorbidity_K7_Liver", "biomarker_Tprotein", "family_history_BreastCancer", 
                      "comorbidity_R1_Other_symptoms", "biomarker_HCT", "early_life_comparative_height_age_ten_Shorter", 
                      "comorbidity_C_D_neoplasms", "comorbidity_G_Eye_adnexa", "biomarker_platelet", 
                      "score_housing", "diet_non_oily_fish_cat", "comorbidity_K8_Gallbladder", 
                      "family_history_HBP", "ethnicity_cat_Asian", "family_history_CRC", 
                      "waist_hip_ratio", "biomarker_TRG", "Model_Education_college.uni", 
                      "smoking_status_Never", "comorbidity_J_Respiratory", "diet_oily_fish_cat", 
                      "family_history_Diabetes", "comorbidity_N_Renal_Failure", "comorbidity_K_other_stomach", 
                      "family_history_Parkinson", "Model_Lower_Ca", "medication_20003_metformin", 
                      "Model_Low_folate", "biomarker_lymphocyte", "diet_coffee_Decaffeinated", 
                      "Model_BMI_cat_0", "medication_vitamin_6155_Vitamin.D", "medication_20003_CCB", 
                      "comorbidity_I_Circ_Ischaemic_heart", "comorbidity_K_Gastrooesophageal_reflux_disease", 
                      "biomarker_urine_K", "diet_bread_type_Wholemeal.or.wholegrain", 
                      "medication_20003_antibiotics", "weekly_mobile_len_hrs", "diet_alcohol_Previous", 
                      "early_life_comparative_body_size_age_ten_Plumper", "comorbidity_20002_hypertension", 
                      "comorbidity_ICD10_E78.0.Pure.hypercholesterolaemia", "comorbidity_20002_diabetes", 
                      "comorbidity_F_Mental_disorder", "Model_NSAID", "sex_male", "diet_very_hot_drink", 
                      "early_life_comparative_body_size_age_ten_Thinner", "pollution_particulate_PM_course", 
                      "comorbidity_20002_gastro.oesophageal.reflux..gord....gastric.reflux", 
                      "Model_Lower_fibre", "skin_colour_Black", "biomarker_IGF_one", 
                      "medication_diet_mineral_6719__Glucosamine", "comorbidity_E_Endocrine_thyroid", 
                      "comorbidity_I_Cerebrovascular", "Model_Greater_red_meat", "comorbidity_B_infectious", 
                      "comorbidity_D_benign_rectal_neoplasms_poly")



plot(enet_5yr_min[enet_5yr_min > 0.2], type = "h", col = "navy",
     lwd = 3, yaxt="n", xaxt = "n", xlab = "", ylab = expression(beta),
     ylim = c(0, 1.9), las = 1, main="Elastic net stability analysis under lambda min (5 year)")
axis(side = 2, at = c(0,0.5,1.0))

text(enet_5yr_min + 0.07, 
     labels = var_names_enet_min, pos = 4, offset=-0.35, srt = 90, cex = 0.7)



