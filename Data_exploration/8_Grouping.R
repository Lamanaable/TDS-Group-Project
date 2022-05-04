## Grouping

# Date: 25 Feb 2022
# Authors: TDS Group 2 (Wei)
# Description: 
# This script intend to group variables,suitable for LASSO/Trees/SVM, etc.

# Output: CRC_data_grouped.csv/rds 

# 1. Setup -------------------------------------------------------------------

# Package used for date conversion:
rm(list=ls())
library(tidyverse)

# 2. Reading the data --------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
CRC_path <- as.character(args[1])
women_path <- as.character(args[2])

CRC_data <- readRDS(CRC_path)
women_only <- readRDS(women_path)

CRC_data = readRDS("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_comorbidity_5_year.rds")
#CRC_covars_women_only_5_year <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_covars_women_only_5_year.rds")
#women_only <- readRDS("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_covars_women_only_5_year.rds")

# 3. Group variables --------------
CRC_data <- CRC_data%>% select(-'assessment_centre')
CRC_data <- CRC_data%>% select(-'date_recr')

# deal with keep --------
keep <- CRC_data[,1:199]

waist_hip_ratio <- (keep$waist_circumference)/(keep$hip_circumference)
keep <- keep %>% add_column(waist_hip_ratio,.after='hip_circumference')
keep <- keep %>% select(-'waist_circumference')
keep <- keep %>% select(-'hip_circumference')
  
remove_names <- c("number_treatments_medication","number_days_walk_ten",
                  "phys_act_walk_duration","birth_year","number_cancers","number_noncancers","number_operations",
                  "accommodation_A house or bungalow","accommodation_A flat, maisonette or apartment",
                  "accommodation_Mobile or temporary structure (i.e. caravan)",
                  "accommodation_Sheltered accommodation","accommodation_Care home",
                  "number_household","num_vehicles","time_outdoors_summer","time_outdoors_winter",
                  "chronotype_cat_evening_person","chronotype_cat_morning_person",
                  "diet_cooked_vegatable","diet_raw_vegatable","diet_fresh_fruit",
                  "diet_dried_fruit", "diet_processed_meat_cat","diet_poultry_cat", 
                  "diet_beef_cat","diet_lamb_cat","diet_pork_cat","diet_cheese_cat",
                  "diet_milk_type_Full cream","diet_milk_type_Semi-skimmed","diet_milk_type_Skimmed",
                  "diet_milk_type_Soya","diet_milk_type_Other type of milk",
                  "diet_milk_type_Never/rarely have milk","diet_bread", 
                  "diet_bread_type_Other type of bread","father_alive_Yes","father_alive_No",
                  "mother_alive_No","mother_alive_Yes")

keep <- keep%>% rename(diet_coffee_Decaffeinated = `diet_coffee_type_Decaffeinated coffee (any type)`)
diet_coffee <- rowSums(as.data.frame(apply(keep[,101:103],2,as.numeric)))
diet_coffee <- as.factor(diet_coffee)
keep <- keep %>% add_column(diet_coffee, .after='diet_coffee_Decaffeinated')
keep <- keep %>% select(-starts_with('diet_coffee_type'))

keep <- keep %>% select(-all_of(remove_names))

# deal with multi-categorical variables-----
choose <- CRC_data[,200:ncol(CRC_data)]
choose <- choose %>% mutate_if(is.factor,as.numeric)
choose <- choose-1

sum((colSums(choose)/nrow(choose))<0.03)
names <- names(choose[,(colSums(choose)/nrow(choose))<0.03])

# occupation
occupation = choose[,1:7]
occupation <- occupation[,-which(colnames(occupation) %in% names[1:2])]
occupation <- apply(occupation,2,as.factor)
keep <- cbind(keep,occupation)

choose <- choose[,8:ncol(choose)]
names <- names[3:length(names)]

# cardiovascular
dim(choose %>% select(starts_with('cardiovascular')))[2]
cardiovascular = choose[,1:4]
cardiovascular <- cardiovascular %>% select(-'cardiovascular_Stroke')
cardiovascular <- apply(cardiovascular,2,as.factor)

choose <- choose[,5:ncol(choose)]
names <- names[2:length(names)]

# comorbidity_field6152
dim(choose %>% select(starts_with('comorbidity_field6152')))[2]
comorbidity_field6152 = choose[,1:5]
comorbidity_field6152 <- comorbidity_field6152[,-which(colnames(comorbidity_field6152) %in% names[1:3])]

choose <- choose[,6:ncol(choose)]
names <- names[4:length(names)]

# medication_field6153_6154
dim(choose %>% select(starts_with('medication_field6153'),starts_with('medication_field6154')))[2]
medication_field6153_6154 = choose[,1:11]
medication_field6153_6154 <- medication_field6153_6154[,-which(colnames(medication_field6153_6154) %in% names[1:5])]

choose <- choose[,12:ncol(choose)]
names <- names[6:length(names)]

# medication_vitamin
dim(choose %>% select(starts_with('vitamin')))[2]
medication_vitamin_6155 = choose[,1:7]
medication_vitamin_6155 <- medication_vitamin_6155[,-which(colnames(medication_vitamin_6155) %in% names[1:3])]
colnames(medication_vitamin_6155) = paste0('medication_',colnames(medication_vitamin_6155))

choose <- choose[,8:ncol(choose)]
names <- names[4:length(names)]

# pain_type
dim(choose %>% select(starts_with('pain')))[2]
pain_type_6159 = choose[,1:8]

choose <- choose[,9:ncol(choose)]
names <- names[3:length(names)]

# leisure activity
dim(choose %>% select(starts_with('leisure')))[2]
leisure = choose[,1:5]
choose <- choose[,6:ncol(choose)]

# medication_6177
dim(choose %>% select(starts_with('medication_6177')))[2]
medication_6177 = choose[,1:3]
medication_6177 <- medication_6177[,-which(colnames(medication_6177) %in% names[1])]

choose <- choose[,4:ncol(choose)]
names <- names[2:length(names)]

# mineral
dim(choose %>% select(starts_with('diet_mineral')))[2]
mineral = choose[,1:6]
mineral <- mineral[,-which(colnames(mineral) %in% names[1:2])]
colnames(mineral) = paste0('medication_',colnames(mineral))

mineral <- apply(mineral,2,as.factor)

choose <- choose[,7:ncol(choose)]
names <- names[3:length(names)]

# cancer
dim(choose %>% select(starts_with('cancer')))[2]
cancer = choose[,1:60]

choose <- choose[,61:ncol(choose)]
names <- names[61:length(names)]

# comorbidity
dim(choose %>% select(starts_with('comorbidity_20002')))[2]
comorbidity_20002= choose[,1:408]
comorbidity_20002 <- comorbidity_20002[,-which(colnames(comorbidity_20002) %in% names[1:396])]

choose <- choose[,409:ncol(choose)]
names <- names[397:length(names)]

# medication
dim(choose %>% select(starts_with('medication_')))[2]
medication= choose[,1:1464]

medication_20003_Aspirin <- rowSums(medication %>% select(contains('aspirin')))
medication_20003_NSAIDS <- rowSums(medication %>% select(contains('ibuprofen'),contains('naproxen'),
                                                         contains('diclofenac'),contains('mefenamic acid'),
                                                         contains('etoricoxib'),contains('indomethacin'),
                                                         contains('paracetamol')))
medication_20003_Statins <- rowSums(medication %>% select(contains('statin')))
medication_20003_antioxidants <- rowSums(medication %>% select(contains('vitamin a'),contains('vitamin c'),
                                                               contains('vitamin e'),contains('beta-carotene'),
                                                               contains('lycopene'),contains('lutein'),
                                                               contains('selenium'),contains('multivitamin')))
medication_20003_antibiotics <- rowSums(medication %>% select(contains('cillin'),
                                                              contains('co-amoxiclav'),contains('cefalexin'),
                                                              contains('micin'), contains('mycin'),
                                                              contains('cycline'), 
                                                              contains('xacin'),contains('fusidic acid'),
                                                              contains('nitrofurantoin'),contains('trimethoprim')))
medication_20003_ACEI <- rowSums(medication %>% select(contains('pril')))
medication_20003_ARB <- rowSums(medication %>% select(contains('sartan')))
medication_20003_CCB <- rowSums(medication %>% select(contains('dipine')))
medication_20003_BetaBlocker <- rowSums(medication %>% select(contains('lol')))

medication_20003_diuretics <- rowSums(medication %>% select(contains('thiazide')))
medication_20003_glucosamine <- rowSums(medication %>% select(contains('glucosamine')))
medication_20003_H_Blocker <- rowSums(medication %>% select(contains('prazole')))
medication_20003_metformin <-  rowSums(medication %>% select(contains('metformin')))
medication_20003_omega_3 <- rowSums(medication %>% select(contains('omega-3')))

medication_field6153_6154 <- apply(medication_field6153_6154,2,as.factor)
medication_vitamin_6155 <- apply(medication_vitamin_6155,2,as.factor)
medication_6177 <- apply(medication_6177,2,as.factor)
keep <- cbind(keep,mineral,medication_vitamin_6155,medication_6177)
keep <- cbind(keep,medication_20003_Aspirin,medication_20003_NSAIDS,medication_20003_Statins,medication_20003_antioxidants,
              medication_20003_antibiotics,medication_20003_ACEI,medication_20003_ARB,medication_20003_CCB,
              medication_20003_BetaBlocker,medication_20003_diuretics,medication_20003_glucosamine,
              medication_20003_H_Blocker,medication_20003_metformin,medication_20003_omega_3)

choose <- choose[,1465:ncol(choose)]
names <- names[1447:length(names)]

# operation
dim(choose %>% select(starts_with('operation_')))[2]
operation = choose[,1:240]
operation <- operation %>% select(c('operation_20004_appendicectomy',
                                    'operation_20004_cholecystectomy/gall bladder removal',
                                    'operation_20004_sterilisation'))
keep <- cbind(keep,operation)

choose <- choose[,241:ncol(choose)]
names <- names[225:length(names)]

# family_history
dim(choose %>% select(contains('family_history')))[2]
family_history = choose[,1:34]

family_history_Alzheimer <- ifelse(rowSums(family_history  %>% select(contains('Alzheimer')))>0,1,0)
family_history_CRC <- ifelse(rowSums(family_history  %>% select(contains('Bowel')))>0,1,0)
family_history_BreastCancer <- ifelse(rowSums(family_history  %>% select(contains('Breast')))>0,1,0)
family_history_COPD <- ifelse(rowSums(family_history  %>% select(contains('Chronic')))>0,1,0)
family_history_Diabetes <- ifelse(rowSums(family_history  %>% select(contains('Diabetes')))>0,1,0)
family_history_Cardio <- ifelse(rowSums(family_history  %>% select(contains('_Heart')))>0,1,0)
family_history_HBP <- ifelse(rowSums(family_history  %>% select(contains('_High')))>0,1,0)
family_history_LungCancer <- ifelse(rowSums(family_history  %>% select(contains('_Lung')))>0,1,0)
family_history_Parkinson <- ifelse(rowSums(family_history  %>% select(contains('_Parkinson')))>0,1,0)
family_history_Prostate <- ifelse(rowSums(family_history  %>% select(contains('_Prostate')))>0,1,0)
family_history_Stroke <- ifelse(rowSums(family_history  %>% select(contains('_Stroke')))>0,1,0)

family_history_new <- cbind(family_history_Alzheimer,family_history_CRC,family_history_BreastCancer,family_history_COPD,
                        family_history_Diabetes,family_history_Cardio,family_history_HBP,family_history_LungCancer,
                        family_history_Parkinson,family_history_Prostate,family_history_Stroke)
family_history_new <- as.data.frame(apply(family_history_new,2,as.factor))

keep <- cbind(keep,family_history_new)
keep <- cbind(keep,cardiovascular)
keep <- cbind(keep,comorbidity_20002)

# comorbidity -----------
choose <- choose %>% select(contains('cormorbidity'))

comorbidity <- choose+1
colnames(comorbidity) <- paste0('co',substring(colnames(comorbidity),4))

names(comorbidity[,(colSums(comorbidity)/nrow(comorbidity))>=0.03])

name_keep <- names(comorbidity[,(colSums(comorbidity)/nrow(comorbidity))>=0.03])[1:3]
comorbidity_keep <- comorbidity[,colnames(comorbidity) %in% name_keep]

comorbidity_keep <- apply(comorbidity_keep,2,as.factor)
keep <- cbind(keep,comorbidity_keep)

comorbidity_colon_rectal <- (comorbidity %>% select(contains('colon'),contains('rectal')))[c(1:6,7,9,11)]
comorbidity_D_benign_rectal_neoplasms_poly <- rowSums(comorbidity_colon_rectal)
keep <- cbind(keep,comorbidity_D_benign_rectal_neoplasms_poly)

# A
comorbidity_A_Intestinal_infectious_diseases <- comorbidity %>% select(contains('comorbidity_ICD10_A0'))
comorbidity_A_Intestinal_infectious_diseases <- rowSums(comorbidity_A_Intestinal_infectious_diseases)
keep <- cbind(keep,comorbidity_A_Intestinal_infectious_diseases)

comorbidity_A10_A99_other_infectious <- comorbidity %>% select(contains('comorbidity_ICD10_A1'),
                                                               contains('comorbidity_ICD10_A2'),
                                                               contains('comorbidity_ICD10_A3'),
                                                               contains('comorbidity_ICD10_A4'),
                                                               contains('comorbidity_ICD10_A5'),
                                                               contains('comorbidity_ICD10_A6'),
                                                               contains('comorbidity_ICD10_A7'),
                                                               contains('comorbidity_ICD10_A8'),
                                                               contains('comorbidity_ICD10_A9'))
sum(rowSums(comorbidity_A10_A99_other_infectious))
comorbidity_A10_A99_other_infectious <- rowSums(comorbidity_A10_A99_other_infectious)
keep <- cbind(keep,comorbidity_A10_A99_other_infectious)

# B
comorbidity_B_infectious <- comorbidity %>% select(contains('comorbidity_ICD10_B'))
comorbidity_B_infectious <- rowSums(comorbidity_B_infectious)
keep <- cbind(keep,comorbidity_B_infectious)

# CD
comorbidity_C_D_neoplasms <- comorbidity %>% select(contains('comorbidity_ICD10_C'),
                                                          contains('comorbidity_ICD10_D0'),
                                                          contains('comorbidity_ICD10_D1'),
                                                          contains('comorbidity_ICD10_D2'),
                                                          contains('comorbidity_ICD10_D3'),
                                                          contains('comorbidity_ICD10_D4'))
comorbidity_C_D_neoplasms <- rowSums(comorbidity_C_D_neoplasms)
keep <- cbind(keep,comorbidity_C_D_neoplasms)

comorbidity_D_Blood_Immune <- comorbidity %>% select(contains('comorbidity_ICD10_D5'),
                                                          contains('comorbidity_ICD10_D6'),
                                                          contains('comorbidity_ICD10_D7'),
                                                          contains('comorbidity_ICD10_D8'))
comorbidity_D_Blood_Immune <- rowSums(comorbidity_D_Blood_Immune)
keep <- cbind(keep,comorbidity_D_Blood_Immune)

# E
comorbidity_E_Endocrine_thyroid <- comorbidity %>% select(contains('comorbidity_ICD10_E0'))
comorbidity_E_Endocrine_thyroid <- rowSums(comorbidity_E_Endocrine_thyroid)
keep <- cbind(keep,comorbidity_E_Endocrine_thyroid)

comorbidity_E_Diabetes <- comorbidity %>% select(contains('comorbidity_ICD10_E10'),
                                                 contains('comorbidity_ICD10_E11'),
                                                 contains('comorbidity_ICD10_E12'),
                                                 contains('comorbidity_ICD10_E13'),
                                                 contains('comorbidity_ICD10_E14'))
comorbidity_E_Diabetes <- rowSums(comorbidity_E_Diabetes)
keep <- cbind(keep,comorbidity_E_Diabetes)

comorbidity_E_Other_glucose_disorder <- comorbidity %>% select(contains('comorbidity_ICD10_E15'),
                                                 contains('comorbidity_ICD10_E16'))
comorbidity_E_Other_glucose_disorder <- rowSums(comorbidity_E_Other_glucose_disorder)
keep <- cbind(keep,comorbidity_E_Other_glucose_disorder)

comorbidity_E_other_endocrine_glands <- comorbidity %>% select(contains('comorbidity_ICD10_E2'),
                                                 contains('comorbidity_ICD10_E3'))
comorbidity_E_other_endocrine_glands <- rowSums(comorbidity_E_other_endocrine_glands)
keep <- cbind(keep,comorbidity_E_other_endocrine_glands)

comorbidity_E_Malnutrition <- comorbidity %>% select(contains('comorbidity_ICD10_E4'),
                                                     contains('comorbidity_ICD10_E5'),
                                                     contains('comorbidity_ICD10_E60'),
                                                     contains('comorbidity_ICD10_E61'),
                                                     contains('comorbidity_ICD10_E62'),
                                                     contains('comorbidity_ICD10_E63'),
                                                     contains('comorbidity_ICD10_E64'))
comorbidity_E_Malnutrition <- rowSums(comorbidity_E_Malnutrition)
keep <- cbind(keep,comorbidity_E_Malnutrition)

comorbidity_E_Obesity <- comorbidity %>% select(contains('comorbidity_ICD10_E65'),
                                                 contains('comorbidity_ICD10_E66'),
                                                 contains('comorbidity_ICD10_E67'),
                                                 contains('comorbidity_ICD10_E68'))
comorbidity_E_Obesity <- rowSums(comorbidity_E_Obesity)
keep <- cbind(keep,comorbidity_E_Obesity)

comorbidity_E_Metabolic_disorders <- comorbidity %>% select(contains('comorbidity_ICD10_E7'),
                                                 contains('comorbidity_ICD10_E8'),
                                                 contains('comorbidity_ICD10_E9'))
comorbidity_E_Metabolic_disorders <- rowSums(comorbidity_E_Metabolic_disorders)
keep <- cbind(keep,comorbidity_E_Metabolic_disorders)

# F
comorbidity_F_Mental_disorder <- comorbidity %>% select(contains('comorbidity_ICD10_F'))
comorbidity_F_Mental_disorder <- rowSums(comorbidity_F_Mental_disorder)
keep <- cbind(keep,comorbidity_F_Mental_disorder)

# G
comorbidity_G_Nervous_system <- comorbidity %>% select(contains('comorbidity_ICD10_G'))
comorbidity_G_Nervous_system <- rowSums(comorbidity_G_Nervous_system)
keep <- cbind(keep,comorbidity_G_Nervous_system)

# H
comorbidity_H_Eye_adnexa <- comorbidity %>% select(contains('comorbidity_ICD10_H0'),
                                                   contains('comorbidity_ICD10_H1'),
                                                   contains('comorbidity_ICD10_H2'),
                                                   contains('comorbidity_ICD10_H3'),
                                                   contains('comorbidity_ICD10_H4'),
                                                   contains('comorbidity_ICD10_H5'))
comorbidity_H_Eye_adnexa <- rowSums(comorbidity_H_Eye_adnexa)
keep <- cbind(keep,comorbidity_H_Eye_adnexa)

comorbidity_H_Ear <- comorbidity %>% select(contains('comorbidity_ICD10_H6'),
                                                   contains('comorbidity_ICD10_H7'),
                                                   contains('comorbidity_ICD10_H8'),
                                                   contains('comorbidity_ICD10_H9'))
comorbidity_H_Ear  <- rowSums(comorbidity_H_Ear)
keep <- cbind(keep,comorbidity_H_Ear)

# I

comorbidity_I_Circ_Chronic_rheumatic_heart <- comorbidity %>% select(contains('comorbidity_ICD10_I05'),
                                                                   contains('comorbidity_ICD10_I06'),
                                                                   contains('comorbidity_ICD10_I07'),
                                                                   contains('comorbidity_ICD10_I08'),
                                                                   contains('comorbidity_ICD10_I09'))
comorbidity_I_Circ_Chronic_rheumatic_heart <- rowSums(comorbidity_I_Circ_Chronic_rheumatic_heart)
keep <- cbind(keep,comorbidity_I_Circ_Chronic_rheumatic_heart)

comorbidity_I_Circ_Hypertensive <- comorbidity %>% select(contains('comorbidity_ICD10_I10'))
comorbidity_I_Circ_Hypertensive <- rowSums(comorbidity_I_Circ_Hypertensive)
keep <- cbind(keep,comorbidity_I_Circ_Hypertensive)

comorbidity_I_Circ_Ischaemic_heart <- comorbidity %>% select(contains('comorbidity_ICD10_I20'),
                                                                     contains('comorbidity_ICD10_I21'),
                                                                     contains('comorbidity_ICD10_I22'),
                                                                     contains('comorbidity_ICD10_I23'),
                                                                     contains('comorbidity_ICD10_I24'),
                                                                     contains('comorbidity_ICD10_I25'))
comorbidity_I_Circ_Ischaemic_heart <- rowSums(comorbidity_I_Circ_Ischaemic_heart)
keep <- cbind(keep,comorbidity_I_Circ_Ischaemic_heart)

comorbidity_I_Pulmonary_heart <- comorbidity %>% select(contains('comorbidity_ICD10_I26'),
                                                             contains('comorbidity_ICD10_I27'),
                                                             contains('comorbidity_ICD10_I28'))
comorbidity_I_Pulmonary_heart <- rowSums(comorbidity_I_Pulmonary_heart)
keep <- cbind(keep,comorbidity_I_Pulmonary_heart)

comorbidity_I_other_heart_diseases <- comorbidity %>% select(contains('comorbidity_ICD10_I3'),
                                                        contains('comorbidity_ICD10_I4'),
                                                        contains('comorbidity_ICD10_I5'))
comorbidity_I_other_heart_diseases <- rowSums(comorbidity_I_other_heart_diseases)
keep <- cbind(keep,comorbidity_I_other_heart_diseases)

comorbidity_I_Cerebrovascular <- comorbidity %>% select(contains('comorbidity_ICD10_I6'))
comorbidity_I_Cerebrovascular <- rowSums(comorbidity_I_Cerebrovascular)
keep <- cbind(keep,comorbidity_I_Cerebrovascular)

comorbidity_I_Arteries <- comorbidity %>% select(contains('comorbidity_ICD10_I7'))
comorbidity_I_Arteries <- rowSums(comorbidity_I_Arteries)
keep <- cbind(keep,comorbidity_I_Arteries)

comorbidity_I_Veins <- comorbidity %>% select(contains('comorbidity_ICD10_I8'))
comorbidity_I_Veins <- rowSums(comorbidity_I_Veins)
keep <- cbind(keep,comorbidity_I_Veins)

comorbidity_I_Other_Circ <- comorbidity %>% select(contains('comorbidity_ICD10_I9'))
comorbidity_I_Other_Circ <- rowSums(comorbidity_I_Other_Circ)
keep <- cbind(keep,comorbidity_I_Other_Circ)
              
# J
comorbidity_J_Respiratory <- comorbidity %>% select(contains('comorbidity_ICD10_J'))
comorbidity_J_Respiratory <- rowSums(comorbidity_J_Respiratory)
keep <- cbind(keep,comorbidity_J_Respiratory)

# K
comorbidity_K_oral <- comorbidity %>% select(contains('comorbidity_ICD10_K0'),
                                             contains('comorbidity_ICD10_K10'),
                                             contains('comorbidity_ICD10_K11'),
                                             contains('comorbidity_ICD10_K12'),
                                             contains('comorbidity_ICD10_K13'),
                                             contains('comorbidity_ICD10_K14'))
comorbidity_K_oral <- rowSums(comorbidity_K_oral)
keep <- cbind(keep,comorbidity_K_oral)

comorbidity_K_Gastrooesophageal_reflux_disease <- comorbidity %>% select(contains('comorbidity_ICD10_K21'))
comorbidity_K_Gastrooesophageal_reflux_disease <- rowSums(comorbidity_K_Gastrooesophageal_reflux_disease)
keep <- cbind(keep,comorbidity_K_Gastrooesophageal_reflux_disease)

comorbidity_K_Other_oesophagus <- comorbidity %>% select(contains('comorbidity_ICD10_K20'),
                                                         contains('comorbidity_ICD10_K22'),
                                                         contains('comorbidity_ICD10_K23'))
comorbidity_K_Other_oesophagus <- rowSums(comorbidity_K_Other_oesophagus)
keep <- cbind(keep,comorbidity_K_Other_oesophagus)

comorbidity_K_Gastric_ulcer <- comorbidity %>% select(contains('comorbidity_ICD10_K25'))
comorbidity_K_Gastric_ulcer <- rowSums(comorbidity_K_Gastric_ulcer)
keep <- cbind(keep,comorbidity_K_Gastric_ulcer)

comorbidity_K_Duodenal_ulcer <- comorbidity %>% select(contains('comorbidity_ICD10_K26'))
comorbidity_K_Duodenal_ulcer <- rowSums(comorbidity_K_Duodenal_ulcer)
keep <- cbind(keep,comorbidity_K_Duodenal_ulcer)

comorbidity_K_Peptic_ulcer <- comorbidity %>% select(contains('comorbidity_ICD10_K27'))
comorbidity_K_Peptic_ulcer <- rowSums(comorbidity_K_Peptic_ulcer)
keep <- cbind(keep,comorbidity_K_Peptic_ulcer)

comorbidity_K_Gastrojejunal_ulcer <- comorbidity %>% select(contains('comorbidity_ICD10_K28'))
comorbidity_K_Gastrojejunal_ulcer <- rowSums(comorbidity_K_Gastrojejunal_ulcer)
keep <- cbind(keep,comorbidity_K_Gastrojejunal_ulcer)

comorbidity_K_Gastritis_duodenitis <- comorbidity %>% select(contains('comorbidity_ICD10_K29'))
comorbidity_K_Gastritis_duodenitis <- rowSums(comorbidity_K_Gastritis_duodenitis)
keep <- cbind(keep,comorbidity_K_Gastritis_duodenitis)

comorbidity_K_Dyspepsia <- comorbidity %>% select(contains('comorbidity_ICD10_K30'))
comorbidity_K_Dyspepsia <- rowSums(comorbidity_K_Dyspepsia)
keep <- cbind(keep,comorbidity_K_Dyspepsia)

comorbidity_K_other_stomach <- comorbidity %>% select(contains('comorbidity_ICD10_K31'))
comorbidity_K_other_stomach <- rowSums(comorbidity_K_other_stomach)
keep <- cbind(keep,comorbidity_K_other_stomach)

comorbidity_K_Appendix <- comorbidity %>% select(contains('comorbidity_ICD10_K35'),
                                                 contains('comorbidity_ICD10_K36'),
                                                 contains('comorbidity_ICD10_K37'),
                                                 contains('comorbidity_ICD10_K38'))
comorbidity_K_Appendix <- rowSums(comorbidity_K_Appendix)
keep <- cbind(keep,comorbidity_K_Appendix)

comorbidity_K_Hernia <- comorbidity %>% select(contains('comorbidity_ICD10_K4'))
comorbidity_K_Hernia  <- rowSums(comorbidity_K_Hernia)
keep <- cbind(keep,comorbidity_K_Hernia)

comorbidity_K_51_52_IBD <- comorbidity %>% select(contains('comorbidity_ICD10_K50'),
                                            contains('comorbidity_ICD10_K51'))
comorbidity_K_51_52_IBD  <- rowSums(comorbidity_K_51_52_IBD)
keep <- cbind(keep,comorbidity_K_51_52_IBD)

comorbidity_K55_K64_Other_intestines <- comorbidity %>% select(contains('comorbidity_ICD10_K55'),
                                                               contains('comorbidity_ICD10_K56'),
                                                               contains('comorbidity_ICD10_K57'),
                                                               contains('comorbidity_ICD10_K58'),
                                                               contains('comorbidity_ICD10_K59'),
                                                               contains('comorbidity_ICD10_K60'),
                                                               contains('comorbidity_ICD10_K61'),
                                                               contains('comorbidity_ICD10_K62'),
                                                               contains('comorbidity_ICD10_K63'),
                                                               contains('comorbidity_ICD10_K64'))
comorbidity_K55_K64_Other_intestines  <- rowSums(comorbidity_K55_K64_Other_intestines)
keep <- cbind(keep,comorbidity_K55_K64_Other_intestines)

comorbidity_K6_peritoneum <- comorbidity %>% select(contains('comorbidity_ICD10_K65'),
                                                   contains('comorbidity_ICD10_K66'),
                                                   contains('comorbidity_ICD10_K67'))
comorbidity_K6_peritoneum  <- rowSums(comorbidity_K6_peritoneum)
keep <- cbind(keep,comorbidity_K6_peritoneum)

comorbidity_K7_Liver <- comorbidity %>% select(contains('comorbidity_ICD10_K7'))
comorbidity_K7_Liver  <- rowSums(comorbidity_K7_Liver)
keep <- cbind(keep,comorbidity_K7_Liver)

comorbidity_K8_Gallbladder <- comorbidity %>% select(contains('comorbidity_ICD10_K8'))
comorbidity_K8_Gallbladder  <- rowSums(comorbidity_K8_Gallbladder)
keep <- cbind(keep,comorbidity_K8_Gallbladder)

# L
comorbidity_L_skin <- comorbidity %>% select(contains('comorbidity_ICD10_L'))
comorbidity_L_skin  <- rowSums(comorbidity_L_skin)
keep <- cbind(keep,comorbidity_L_skin)

# M
comorbidity_M_Muscle <- comorbidity %>% select(contains('comorbidity_ICD10_M'))
comorbidity_M_Muscle  <- rowSums(comorbidity_M_Muscle)

# N
comorbidity_N_Glomerular_diseases <- comorbidity %>% select(contains('comorbidity_ICD10_N0'))
comorbidity_N_Glomerular_diseases  <- rowSums(comorbidity_N_Glomerular_diseases)
keep <- cbind(keep,comorbidity_N_Glomerular_diseases)

comorbidity_N_Renal_tubulo_interstitial <- comorbidity %>% select(contains('comorbidity_ICD10_N10'),
                                                                  contains('comorbidity_ICD10_N11'),
                                                                  contains('comorbidity_ICD10_N12'),
                                                                  contains('comorbidity_ICD10_N14'),
                                                                  contains('comorbidity_ICD10_N15'),
                                                                  contains('comorbidity_ICD10_N16'))
comorbidity_N_Renal_tubulo_interstitial  <- rowSums(comorbidity_N_Renal_tubulo_interstitial)
keep <- cbind(keep,comorbidity_N_Renal_tubulo_interstitial)


comorbidity_N_Renal_Failure <- comorbidity %>% select(contains('comorbidity_ICD10_N17'),
                                                                  contains('comorbidity_ICD10_N18'),
                                                                  contains('comorbidity_ICD10_N19'))
comorbidity_N_Renal_Failure  <- rowSums(comorbidity_N_Renal_Failure)
keep <- cbind(keep,comorbidity_N_Renal_Failure)

comorbidity_N_Urolithiasis <- comorbidity %>% select(contains('comorbidity_ICD10_N20'),
                                                      contains('comorbidity_ICD10_N21'),
                                                      contains('comorbidity_ICD10_N22'),
                                                     contains('comorbidity_ICD10_N23'))
comorbidity_N_Urolithiasis  <- rowSums(comorbidity_N_Urolithiasis)
keep <- cbind(keep,comorbidity_N_Urolithiasis)

comorbidity_N_Other_kidney <- comorbidity %>% select(contains('comorbidity_ICD10_N25'),
                                                     contains('comorbidity_ICD10_N26'),
                                                     contains('comorbidity_ICD10_N27'),
                                                     contains('comorbidity_ICD10_N28'),
                                                     contains('comorbidity_ICD10_N29'))
comorbidity_N_Other_kidney  <- rowSums(comorbidity_N_Other_kidney)
keep <- cbind(keep,comorbidity_N_Other_kidney)

comorbidity_N_Other_urinary <- comorbidity %>% select(contains('comorbidity_ICD10_N3'))
comorbidity_N_Other_urinary  <- rowSums(comorbidity_N_Other_urinary)
keep <- cbind(keep,comorbidity_N_Other_urinary)

male=NULL
for (i in c(40,41,42,43,45,47,48,50)){
  x <- NULL
  x <- comorbidity %>% select(contains(paste0('comorbidity_ICD10_N',i)))
  x <- rowSums(x)
  male <- cbind(male,x)
  colnames(male)[ncol(male)] = paste0('comorbidity_N',i,'_male')
}

male <- as.data.frame(male)
male$eid <- rownames(male)
CRC_male <- keep[keep$sex_male==1,]
male <- male[male$eid %in% CRC_male$eid,]

#write.csv(male,'Outputs/CRC_covars_men_only_5_year.csv')
#saveRDS(male,'Outputs/CRC_covars_men_only_5_year.rds')

covars_women = NULL

female=NULL
for (i in seq(70,98)){
  x <- NULL
  x <- comorbidity %>% select(contains(paste0('comorbidity_ICD10_N',i)))
  x <- rowSums(x)
  if(sum(x)>100) {
    female <- cbind(female,x)
    colnames(female)[ncol(female)] = paste0('comorbidity_N',i,'_female')
  }
}

female <- as.data.frame(female)
female$eid <- rownames(female)
CRC_female <- keep[keep$sex_male==0,]
female <- female[female$eid %in% CRC_female$eid,]
female <- female %>% select(-'eid')
  
women_only <- cbind(women_only,female)

# O

comorbidity_O_Pregnancy <- comorbidity %>% select(contains('comorbidity_ICD10_O'))
comorbidity_O_Pregnancy  <- rowSums(comorbidity_O_Pregnancy)

comorbidity_O_Pregnancy <- as.data.frame(comorbidity_O_Pregnancy)
comorbidity_O_Pregnancy$eid <- rownames(comorbidity_O_Pregnancy)
CRC_female <- keep[keep$sex_male==0,]
comorbidity_O_Pregnancy <- comorbidity_O_Pregnancy[comorbidity_O_Pregnancy$eid %in% CRC_female$eid,]
comorbidity_O_Pregnancy <- comorbidity_O_Pregnancy %>% select(-'eid')

women_only <- cbind(women_only,comorbidity_O_Pregnancy)

# Q
comorbidity_Q_Congenital <- comorbidity %>% select(contains('comorbidity_ICD10_Q'))
comorbidity_Q_Congenital  <- rowSums(comorbidity_Q_Congenital)
keep <- cbind(keep,comorbidity_Q_Congenital)

# R

for (i in seq(0,9)){
  x <- NULL
  x <- comorbidity %>% select(contains(paste0('comorbidity_ICD10_R',i)))
  x <- rowSums(x)
  if (sum(x) >100 ){
    keep <- cbind(keep,x)
    colnames(keep)[ncol(keep)] = paste0('comorbidity_R',i,'_Other_symptoms')
  }
}

for (i in seq(10,90)){
  x <- NULL
  x <- comorbidity %>% select(contains(paste0('comorbidity_ICD10_R',i)))
  x <- rowSums(x)
  if (sum(x) >100 ){
    keep <- cbind(keep,x)
    colnames(keep)[ncol(keep)] = paste0('comorbidity_R',i,'_Other_symptoms')
  }
}

# ST
for (i in seq(0,9)){
  x <- NULL
  x <- comorbidity %>% select(contains(paste0('comorbidity_ICD10_S',i)))
  x <- rowSums(x)
  if (sum(x) >100 ){
    keep <- cbind(keep,x)
    colnames(keep)[ncol(keep)] = paste0('comorbidity_S',i,'_Injuries')
  }
}

for (i in seq(10,99)){
  x <- NULL
  x <- comorbidity %>% select(contains(paste0('comorbidity_ICD10_S',i)))
  x <- rowSums(x)
  if (sum(x) >100 ){
    keep <- cbind(keep,x)
    colnames(keep)[ncol(keep)] = paste0('comorbidity_S',i,'_Injuries')
  }
}

for (i in seq(0,9)){
  x <- NULL
  x <- comorbidity %>% select(contains(paste0('comorbidity_ICD10_T',i)))
  x <- rowSums(x)
  if (sum(x) >100 ){
    keep <- cbind(keep,x)
    colnames(keep)[ncol(keep)] = paste0('comorbidity_T',i,'_Injuries')
  }
}

for (i in seq(10,99)){
  x <- NULL
  x <- comorbidity %>% select(contains(paste0('comorbidity_ICD10_T',i)))
  x <- rowSums(x)
  if (sum(x) >100 ){
    keep <- cbind(keep,x)
    colnames(keep)[ncol(keep)] = paste0('comorbidity_T',i,'_Injuries')
  }
}

# V,W,X,Y
for (i in seq(0,9)){
  x <- NULL
  x <- comorbidity %>% select(contains(paste0('comorbidity_ICD10_V',i)))
  x <- rowSums(x)
  if (sum(x) >100 ){
    keep <- cbind(keep,x)
    colnames(keep)[ncol(keep)] = paste0('comorbidity_V',i,'_External_causes')
  }
}

for (i in seq(0,9)){
  x <- NULL
  x <- comorbidity %>% select(contains(paste0('comorbidity_ICD10_W',i)))
  x <- rowSums(x)
  if (sum(x) >100 ){
    keep <- cbind(keep,x)
    colnames(keep)[ncol(keep)] = paste0('comorbidity_W',i,'_External_causes')
  }
}

for (i in seq(0,9)){
  x <- NULL
  x <- comorbidity %>% select(contains(paste0('comorbidity_ICD10_X',i)))
  x <- rowSums(x)
  if (sum(x) >100 ){
    keep <- cbind(keep,x)
    colnames(keep)[ncol(keep)] = paste0('comorbidity_X',i,'_External_causes')
  }
}

for (i in seq(0,9)){
  x <- NULL
  x <- comorbidity %>% select(contains(paste0('comorbidity_ICD10_Y',i)))
  x <- rowSums(x)
  if (sum(x) >100 ){
    keep <- cbind(keep,x)
    colnames(keep)[ncol(keep)] = paste0('comorbidity_Y',i,'_External_causes')
  }
}


# women_only

covars_women <- keep %>% select(contains('oophorectomy'),contains('caesarean'),
                              contains('hysterectomy'),contains('comorbidity_ICD10_C50.9'),
                              contains('comorbidity_ICD10_Z85.3'),
                              contains('lumpectomy'))

covars_women <- as.data.frame(covars_women)
covars_women$eid <- rownames(covars_women)
CRC_female <- keep[keep$sex_male==0,]
covars_women <- covars_women[covars_women$eid %in% CRC_female$eid,]
covars_women <- covars_women %>% select(-'eid')

keep <- keep %>% select(-names(covars_women))

women_only <- cbind(women_only,covars_women)


# bind

check <- keep %>% select_if(is.numeric) 

check <- check[,67:ncol(check)]

colSums(check)/nrow(check)

names = names(check[,(colSums(check)/nrow(check))<0.005])

keep <- keep[,!(names(keep) %in% names)]

CRC_data <- keep

CRC_data <- CRC_data %>% mutate_if(is.character,as.factor)

names(CRC_data) <- make.names(names(CRC_data), unique=TRUE)

# delete some
CRC_data <- CRC_data %>% select(-contains('smoking_current'))
CRC_data <- CRC_data %>% select(-contains('smoking_past'))
CRC_data <- CRC_data %>% select(-starts_with('neuro_'))
CRC_data <- CRC_data %>% select(-starts_with('broken_bones'))
CRC_data <- CRC_data %>% select(-'Idex_multiple_deprivation')
CRC_data <- CRC_data %>% select(-'comorbidity_20002_angina')
CRC_data <- CRC_data %>% select(-'comorbidity_20002_unclassifiable')
CRC_data <- CRC_data[,1:225]

# check variable types again
apply(CRC_data,2,class)
  
# 4. Export file ----------------------------------

write.csv(CRC_data, "Outputs/CRC_data_5_year_final.csv")
saveRDS(CRC_data, "Outputs/CRC_data_5_year_final.rds")

#write.csv(CRC_covars_women_only_5_year,'Outputs/CRC_covars_women_only_5_year.csv')
#saveRDS(CRC_covars_women_only_5_year,'Outputs/CRC_covars_women_only_5_year.rds')

