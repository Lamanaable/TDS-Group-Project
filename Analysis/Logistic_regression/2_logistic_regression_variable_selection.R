b_5yr_lasso_stability_lambdamin <- readRDS("/rds/general/user/syl416/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/5_year/Outputs/b_5yr_lasso_stability_lambdamin.rds")

lasso_5yr_variable_selection <- data.frame(b_5yr_lasso_stability_lambdamin)
lasso_5yr_variable_selection$variable <- rownames(lasso_5yr_variable_selection)
lasso_5yr_variable_selection_first20 <- c(lasso_5yr_variable_selection[2:21,2])

CRC_data <- readRDS("/rds/general/user/syl416/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_final_5_year.rds")

train <- CRC_data[CRC_data$tt_status=="train",]
test <- CRC_data[CRC_data$tt_status=="test",]

variable_selection <- glm(cc_status ~ 
                            weekly_beer_intake+
                            biomarker_Neu+
                            pulse+
                            operation_20004_appendicectomy+
                            medication_20003_Statins+
                            time_TV+
                            comorbidity_G_Ear+
                            Model_smoking_pack_year+
                            comorbidity_ICD10_I25.1.Atherosclerotic.heart.disease+
                            smoking_status_Previous+
                            occupation_Looking.after.home.and.or.family+
                            medication_20003_glucosamine+
                            comorbidity_K_Gastritis_duodenitis+
                            family_history_sibling_20111_High.blood.pressure+
                            Model_Education_high.school+
                            comorbidity_R0_Other_symptoms+
                            comorbidity_K_Gastric_ulcer+
                            medication_20003_BetaBlocker+
                            diet_milk_type_Semi.skimmed+
                            diet_cereal,
                              data = train,
                              family = "binomial")
summary(variable_selection)
