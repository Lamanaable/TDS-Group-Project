library(tidyverse)


# Lasso stability analysis under lambda min -------------------------------



lasso_5yr_min <- readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/5_year/Outputs/b_5yr_lasso_stability_lambdamin.rds")

# Removing the intercept (although weird that doesn't appear 100% of the time??)
X_row <- which(names(lasso_5yr_min) == "X")
lasso_5yr_min <-lasso_5yr_min [-X_row]

# Looking at names of >0.2 proportion variables
dput(names(lasso_5yr_min [lasso_5yr_min  > 0.2]))



# Manually renaming
var_names_lasso_min<-c("Diet - beer", "Biomarker - Neu", "Pulse", "Operation - appendicectomy", 
                       "Medication - statins", "TV time", "Comorbidity - ear", "Smoking packs", 
                       "Comorbidity - atherosclerotic.heart.disease", "Smoking status - previous", 
                       "Occupation - home/family", "Medication - glucosamine", 
                       "Comorbidity - gastritis duodenitis", "Family history - sibling high bp", 
                       "Education - high school", "Comorbidity - other symptoms (R0)", 
                       "Comorbidity - gatric ulcer", "Medication. - betaBlocker", 
                       "Diet - milk semi-skimmed", "Diet - cereal", "Biomarker - HCT", 
                       "Comorbidity - Liver", "Biomarker - T protein", "Exercise - walking", 
                       "Diet - other coffee", "Comorbidity - other symptoms (R1)", 
                       "Comorbidity - family history drug allergies", "Comorbidity - eye adnexa", 
                       "Early life height - shorter", "Housing score", 
                       "Biomarker  - platelet", "No. cancers", "Comorbidity - C/D neoplasms", 
                       "Family history - father Alzheimer's", "Diet - non-oily fish", 
                       "Diet - full milk", "Ethnicity - Asian", "Family history - father diabetes", 
                       "Leisure - adult education class", "Family history - mother bowel cancer", 
                       "Family history - no. bowel cancers", "Comorbidity - gallbladder", 
                       "Family history -other specified neoplasms", "Education - college/uni", 
                       "Waist circumference", "Medication - omeprazole", 
                       "Smoking status - never", "Biomarker - TRG", "Comorbidity - renal failure", 
                       "Medication - CCB", "Diet - oily fish", "Family history - mother breast cancer", 
                       "Medication - metformin", "Diet - pork", "Comorbidity - respiratory", 
                       "Model - low folate", "Accommodation - mobile/temporary", 
                       "Diet - coffe ground", 
                       "Family history - mother heart disease", "BMI - underweight (0)", 
                       "Father dead", "Family history - sibling diabetes", "Mobile hours", 
                       "Comorbidity - other stomach", "Comorbidity - hypertension", 
                       "Medication - vitamin.D", "Family history - mother high bp")


#jpeg(file="b_5yr_lasso_stability_lambdamin.jpeg")
plot(lasso_5yr_min[lasso_5yr_min > 0.2], type = "h", col = "navy",
           lwd = 3, yaxt="n", xaxt = "n", xlab = "", ylab = expression(beta),
           ylim = c(0, 2), las = 1, main="Lasso stability analysis under lambda min (5 year)")
axis(side = 2, at = c(0,0.5,1.0))

text(lasso_5yr_min[lasso_5yr_min > 0.2] + 0.07, 
     labels = var_names_lasso_min, pos = 4,offset=-0.1, srt = 90, cex = 0.7)








# Lasso stability analysis under lambda 1se -------------------------------

lasso_5yr_1se <- readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/5_year/Outputs/b_5yr_lasso_stability_lambda1se.rds")

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
     ylim = c(0, 1.9), las = 1, main="Lasso stability analysis under lambda 1se (5 year)")
axis(side = 2, at = c(0,0.5,1.0))

text(lasso_5yr_1se[lasso_5yr_1se > 0.2] + 0.07, 
     labels = var_names_lasso_1se, pos = 4,offset=-0.1, srt = 90, cex = 0.7)





# Enet stability analysis under lambda 1se -------------------------------

enet_5yr_1se <- readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/5_year/Outputs/c_enet_5yr_stability_lambda1se.rds")

# Removing the intercept (although weird that doesn't appear 100% of the time??)
X_row <- which(names(enet_5yr_1se) == "X")
enet_5yr_1se<-enet_5yr_1se[-X_row]

# Looking at names of >0.2 proportion variables
dput(names(enet_5yr_1se[enet_5yr_1se > 0.2]))

# Manually renaming
var_names_enet_1se<-cc("Diet - beer", "Smoking packs", "Pulse", "Medication - statins", 
                       "TV time", "Biomarker - neu", "Smoking status - never")



plot(enet_5yr_1se[enet_5yr_1se > 0.2], type = "h", col = "navy",
     lwd = 3, yaxt="n", xaxt = "n", xlab = "", ylab = expression(beta),
     ylim = c(0, 1.2), las = 1, main="Elastic net stability analysis under lambda 1se (5 year)")
axis(side = 2, at = c(0,0.5,1.0))

text(enet_5yr_1se + 0.07, 
     labels = var_names_enet_1se, pos = 4, offset=-0.35, srt = 90, cex = 0.7)





# Enet stability analysis under lambda min -------------------------------

enet_5yr_min <- readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/5_year/Outputs/c_enet_5yr_stability_lambdamin.rds")

# Removing the intercept (although weird that doesn't appear 100% of the time??)
X_row <- which(names(enet_5yr_min) == "X")
enet_5yr_min<-enet_5yr_min[-X_row]

# Looking at names of >0.2 proportion variables
dput(names(enet_5yr_min[enet_5yr_min > 0.2]))

# Manually renaming
var_names_enet_min<-c("Diet - beer", "Biomarker - neu", "Pulse", "Operation - appendicectomy", 
                      "Medication - statins", "Smoking packs", "TV time", 
                      "Comorbidity - ear", "Occupation - home/family", 
                      "Comorbidity - atherosclerotic" ,"Comorbidity - gastritis_duodenitis", 
                      "Previous smoker", "Medication - glucosamine", "Family history - silbing high bp", 
                      "Education - high school", "Comorbidity - other symptoms (R0)", 
                      "Comorbidity - gastric ulcer", "Diet - semi-skimmed milk", 
                      "Medication - beta blocker", "Diet - cereal", "Biomarker - HCT", 
                      "Comorbidity - liver", "Biomarker - T protein", "Exercise - walking", 
                      "Diet - other coffee", "Comorbidity - other symptoms (R1)", 
                      "comorbidity_Z88_Personal_Family_history", "Comorbidity - eye adnexa", 
                      "Hieght in early life (shorter)", "Housing score", 
                      "Family history - father Alzheimer's", "Comorbidity - neoplasms (D)", 
                      "Biomarker - platelet", "No. cancers", "Diet - non-oily fisht", 
                      "Ethnicity - Asian", "Diet - full milk", "Family history - father diabetes", 
                      "Family history - bowel cancer no.", "Education - college/uni", 
                      "Smoking - never", "Leisure - adult education class", 
                      "Family history - mother bowel cancer", "Waist circumference", 
                      "Comorbidity - gallbladder", "Family history - Z9", 
                      "Biomarker - TRG", "Medication - omeprazole", 
                      "Medication - CCB", "Comorbidity - renal failure", "Family history - mother breast cancer", 
                      "Comorbidity - respiratory", "Diet - oily fish", "Diet - ground coffee", 
                      "Medication - metformin", "BMI (cat 0)", "Diet - pork", 
                      "Comorbidity - hypertension", "Accommodation - mobile/temporary", 
                      "Father alive", "Family history - mother heart disease", 
                      "Diet - low folate", "Family history - sibling diabetes", 
                      "Comorbidity - other_stomach", "weekly_mobile_len_hrs", "Biomarker - urine K", 
                      "Medication - vitamin D", "Accomodation - flat", 
                      "Diet - other milk", "Family history - mother high bp", 
                      "Comorbidity - Ischaemic_heart")

# Remove father alive category
# Merge family history categories (sibling/mother/father)
# Merge diet categories - e.g. coffee just yes/no not types of coffee


plot(enet_5yr_min[enet_5yr_min > 0.2], type = "h", col = "navy",
     lwd = 3, yaxt="n", xaxt = "n", xlab = "", ylab = expression(beta),
     ylim = c(0, 1.9), las = 1, main="Elastic net stability analysis under lambda min (5 year)")
axis(side = 2, at = c(0,0.5,1.0))

text(enet_5yr_min + 0.07, 
     labels = var_names_enet_min, pos = 4, offset=-0.35, srt = 90, cex = 0.7)



