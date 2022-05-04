## Grouping

# Date: 04 Mar 2022
# Authors: TDS Group 2 (Wei)
# Description: 
# This script intend to group variables,suitable for LASSO/Trees/SVM, etc.

# Output: CRC_data_grouped.csv/rds 

# 1. Setup -------------------------------------------------------------------

# Package used for date conversion:
rm(list=ls())
library(tidyverse)

# 2. Reading the data --------------------------------------------------------

CRC_data = readRDS("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_final_5_year.rds")
CRC_data_ordered = readRDS("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_5_year_ordered_V2.rds")

# 3. Remove duplicated cols -------------------------------------------------------
CRC_data <- CRC_data[1:27]
CRC_data$Model_NSAID <- as.factor(ifelse(as.numeric(CRC_data_ordered$medication_20003_NSAIDS)>0,1,0))
CRC_data$Model_Asprin <- as.factor(ifelse(as.numeric(CRC_data_ordered$medication_20003_Aspirin)>0,1,0))
CRC_data <- CRC_data %>% rename(
  NSAIDS=Model_NSAID,
  Asprin=Model_Asprin,
  Lower_Ca=Model_Lower_Ca,
  Lower_fibre=Model_Lower_fibre,
  Greater_processed_meat=Model_Greater_processed_meat,
  Greater_red_meat=Model_Greater_red_meat,
  Low_veg=Model_Low_veg,
  Low_fruit=Model_Low_fruit,
  Low_folate=Model_Low_folate,
  Diabetes=Model_Diabetes,
  Education=Model_Education,
  Alcohol=Model_Alcohol_more,
  Sedentary=Model_Sedentary,
  Smoking_pack_year=Model_smoking_pack_year,
  Height=Model_height,
  BMI=Model_BMI_cat
)


CRC_data <- CRC_data %>% mutate(BMI = recode(BMI,'0'='healthy','1'='subhealthy','2'='obesity'),
                                Alcohol=recode(Alcohol,'0'='less than once a week',
                                               '1' = 'once in a week or more',
                                               '2' = 'daily or more'))

saveRDS(CRC_data,'Outputs/CRC_data_5_year_logistic.rds')
# 4. Remove duplicated vars
CRC_data_ordered$Model_NSAID <- as.factor(ifelse(as.numeric(CRC_data_ordered$medication_20003_NSAIDS)>0,1,0))
CRC_data_ordered$Model_Asprin <- as.factor(ifelse(as.numeric(CRC_data_ordered$medication_20003_Aspirin)>0,1,0))

CRC_data_ordered <- CRC_data_ordered %>% rename(
  NSAIDS=Model_NSAID,
  Asprin=Model_Asprin,
  Lower_Ca=Model_Lower_Ca,
  Lower_fibre=Model_Lower_fibre,
  Greater_processed_meat=Model_Greater_processed_meat,
  Greater_red_meat=Model_Greater_red_meat,
  Low_veg=Model_Low_veg,
  Low_fruit=Model_Low_fruit,
  Low_folate=Model_Low_folate,
  Diabetes=Model_Diabetes,
  Education_below_highschool=Model_Education_less.than.high.school,
  Education_highschool=Model_Education_high.school,
  Education_other_professions=Model_Education_other.professions,
  Education_college_uni=Model_Education_college.uni,
  Alcohol_less=Model_Alcohol_more_0,
  Alcohol_daily=Model_Alcohol_more_2,
  Alcohol_once_week_more=Model_Alcohol_more_1,
  Sedentary=Model_Sedentary,
  Smoking_pack_year=Model_smoking_pack_year,
  Height=Model_height,
  BMI_health=Model_BMI_cat_0,
  BMI_subhealthy=Model_BMI_cat_1,
  BMI_obesity=Model_BMI_cat_2,
  weight=Weight
)

skin_colour_fair <- as.numeric(CRC_data_ordered$skin_colour_Very.fair)+
  as.numeric(CRC_data_ordered$skin_colour_Fair)-2
CRC_data_ordered  <- CRC_data_ordered %>% add_column(skin_colour_fair,.after = 'skin_colour_Very.fair')
CRC_data_ordered$skin_colour_fair <- as.factor(CRC_data_ordered$skin_colour_fair)

skin_colour_olive <- as.numeric(CRC_data_ordered$skin_colour_Light.olive)+
  as.numeric(CRC_data_ordered$skin_colour_Dark.olive)-2
CRC_data_ordered  <- CRC_data_ordered %>% add_column(skin_colour_olive,.after = 'skin_colour_Light.olive')
CRC_data_ordered$skin_colour_olive <- as.factor(CRC_data_ordered$skin_colour_olive)

skin_colour_brown_black <- as.numeric(CRC_data_ordered$skin_colour_Brown)+
  as.numeric(CRC_data_ordered$skin_colour_Black)-2
CRC_data_ordered  <- CRC_data_ordered %>% add_column(skin_colour_brown_black,.after = 'skin_colour_Brown')
CRC_data_ordered$skin_colour_brown_black <- as.factor(CRC_data_ordered$skin_colour_brown_black)

CRC_data_ordered <- CRC_data_ordered %>% select(-c('skin_colour_Very.fair','skin_colour_Fair',
                                                   'skin_colour_Light.olive','skin_colour_Dark.olive',
                                                   'skin_colour_Brown','skin_colour_Black'))

CRC_data_ordered <- CRC_data_ordered %>% rename(
occupation_unpaid_voluntary=occupation_Doing.unpaid.or.voluntary.work,
occupation_employment_self_employed=occupation_In.paid.employment.or.self.employed,
occupation_looking_after_family=occupation_Looking.after.home.and.or.family,
occupation_retired=occupation_Retired,
occupation_uable_work=occupation_Unable.to.work.because.of.sickness.or.disability,
body_size_age_ten_Thinner=early_life_comparative_body_size_age_ten_Thinner,
body_size_age_ten_Plumper=early_life_comparative_body_size_age_ten_Plumper,
body_size_age_ten_Average=early_life_comparative_body_size_age_ten_About.average,
height_age_ten_Shorter=early_life_comparative_height_age_ten_Shorter,
height_age_ten_Taller=early_life_comparative_height_age_ten_Taller,
height_age_ten_Average=early_life_comparative_height_age_ten_About.average,
weekly_red_wine=weekly_red_wine_intake,
weekly_champagne_and_white_wine=weekly_champagne_and_white_wine_intake,
weekly_beer=weekly_beer_intake,
weekly_spirits=weekly_spirits_intake,
weekly_fortified_wine=weekly_fortified_wine_intake,
meds_regular_calcium=medication_diet_mineral_6719__Calcium,
meds_regular_fish_oil=medication_diet_mineral_6719_Fish.oil,
meds_regular_glucosamine=medication_diet_mineral_6719__Glucosamine,
meds_regular_zinc=medication_diet_mineral_6719__Zinc,
meds_regular_multivitamins=medication_vitamin_6155_Multivitamins,
meds_regular_vitaminB=medication_vitamin_6155_Vitamin.B,
meds_regular_vitaminC=medication_vitamin_6155_Vitamin.C,
meds_regular_vitaminD=medication_vitamin_6155_Vitamin.D,
meds_hypertension=medication_6177_Blood.pressure.medication,
meds_cholesterol=medication_6177_Cholesterol.lowering.medication,
meds_Statins=medication_20003_Statins,
meds_Antioxidants=medication_20003_antioxidants,
meds_Antibiotics=medication_20003_antibiotics,
meds_ACEI=medication_20003_ACEI,
meds_ARB=medication_20003_ARB,
meds_CCB=medication_20003_CCB,
meds_BetaBlocker=medication_20003_BetaBlocker,
meds_diuretics=medication_20003_diuretics,
meds_HBlocker=medication_20003_H_Blocker,
meds_metformin=medication_20003_metformin,
meds_omega_3=medication_20003_omega_3,
ops_appendicectomy=operation_20004_appendicectomy,
ops_cholecystectomy=operation_20004_cholecystectomy,
ops_sterilisation=operation_20004_sterilisation,
Comorb_Angina=cardiovascular_Angina,
Comorb_Heart_attack=cardiovascular_Heart.attack,
Comorb_Hypertension=cardiovascular_High.blood.pressure,
Comorb_asthma=comorbidity_20002_asthma,
Comorb_depression=comorbidity_20002_depression,
Comorb_Gastro_Oesophageal_Reflux=comorbidity_20002_Gastro_Oesophageal_Reflux,
Comorb_Allergic=comorbidity_20002_hayfever.allergic.rhinitis,
Comorb_hypothyroidism=comorbidity_20002_hypothyroidism.myxoedema,
Comorb_osteoarthritis=comorbidity_20002_osteoarthritis,
Comorb_hypercholesterolaemia=comorbidity_ICD10_E78.0.Pure.hypercholesterolaemia,
Comorb_primary_hypertension=comorbidity_ICD10_I10.Essential..primary..hypertension,
Comorb_AHD=comorbidity_ICD10_I25.1.Atherosclerotic.heart.disease,
Comorb_benign_rectal_neoplasms_polyps=comorbidity_D_benign_rectal_neoplasms_poly,
Comorb_Intestinal_infectious_diseases=comorbidity_A_Intestinal_infectious_diseases,
Comorb_infectious=comorbidity_B_infectious,
Comorb_any_neoplasms=comorbidity_C_D_neoplasms,
Comorb_Blood_Immune=comorbidity_D_Blood_Immune,
Comorb_Endocrine_thyroid=comorbidity_E_Endocrine_thyroid,
Comorb_Diabetes_Complication=comorbidity_E_Diabetes,
Comorb_Obesity=comorbidity_E_Obesity,
Comorb_Metabolic_disorders=comorbidity_E_Metabolic_disorders,
Comorb_Mental_disorder=comorbidity_F_Mental_disorder,
Comorb_Nervous_system=comorbidity_G_Nervous_system,
Comorb_Eye_adnexa=comorbidity_H_Eye_adnexa,
Comorb_Ear=comorbidity_H_Ear,
Comorb_Hypertensive_Heart=comorbidity_I_Circ_Hypertensive,
Comorb_Ischaemic_heart=comorbidity_I_Circ_Ischaemic_heart,
Comorb_other_heart_diseases=comorbidity_I_other_heart_diseases,
Comorb_Cerebrovascular=comorbidity_I_Cerebrovascular,
Comorb_Arteries=comorbidity_I_Arteries,
Comorb_Veins=comorbidity_I_Veins,
Comorb_Respiratory=comorbidity_J_Respiratory,
Comorb_oral=comorbidity_K_oral,
Comorb_Gastrooesophageal_reflux_disease=comorbidity_K_Gastrooesophageal_reflux_disease,
Comorb_Other_oesophagus=comorbidity_K_Other_oesophagus,
Comorb_Gastric_ulcer=comorbidity_K_Gastric_ulcer,
Comorb_Duodenal_ulcer=comorbidity_K_Duodenal_ulcer,
Comorb_Gastritis_duodenitis=comorbidity_K_Gastritis_duodenitis,
Comorb_Dyspepsia=comorbidity_K_Dyspepsia,
Comorb_other_stomach=comorbidity_K_other_stomach,
Comorb_Appendix=comorbidity_K_Appendix,
Comorb_Hernia=comorbidity_K_Hernia,
Comorb_IBD=comorbidity_K_51_52_IBD,
Comorb_Other_intestines=comorbidity_K55_K64_Other_intestines,
Comorb_Liver=comorbidity_K7_Liver,
Comorb_Gallbladder=comorbidity_K8_Gallbladder,
Comorb_skin=comorbidity_L_skin,
Comorb_Renal_Failure=comorbidity_N_Renal_Failure,
Comorb_Urolithiasis=comorbidity_N_Urolithiasis,
Comorb_Other_urinary=comorbidity_N_Other_urinary,
Comorb_Congenital=comorbidity_Q_Congenital,
Comorb_Other_Circ_symptoms=comorbidity_R0_Other_symptoms,
Comorb_Other_Digest_symptoms=comorbidity_R1_Other_symptoms,
Comorb_Other_Urinary_symptoms=comorbidity_R3_Other_symptoms
)

CRC_data_ordered <- CRC_data_ordered %>% select(-c('early_life_breastfed_as_baby_No',
                                                   'birth_maternal_smoking_No','medication_20003_Aspirin',
                                                   'medication_20003_NSAIDS','medication_20003_glucosamine',
                                                   'comorbidity_20002_diabetes',
                                                   'comorbidity_20002_heart.attack.myocardial.infarction',
                                                   'comorbidity_20002_high.cholesterol',
                                                   'comorbidity_20002_hypertension'
                                                   ))

diet_coffee <- as.factor(ifelse(as.numeric(CRC_data_ordered$diet_coffee)-1>0,1,0))
CRC_data_ordered <- CRC_data_ordered%>% select(-'diet_coffee')
CRC_data_ordered <- CRC_data_ordered%>% add_column(diet_coffee,.after='diet_coffee_Decaffeinated')

saveRDS(CRC_data_ordered,'Outputs/CRC_data_5_year_ordered_V3.rds')



