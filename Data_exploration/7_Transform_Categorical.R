## Transform Categorical

# Date: 22 Feb 2022
# Authors: TDS Group 2 (Wei)
# Description: 
# This script :
# 1.clean variable names
# 2.transform variables to hot encoding
# 3.correct variable type
# 4.rename CRC_case as cc_status
# 5.add tt_status: train/test 75%
# 6.add 16 variables starts with 'Model' refers to variable selected in baseline model
# Note: The first 9 columns are CRC-related, could drop them in data analysis

# Output: CRC_data_final.csv/rds 

# 1. Setup -------------------------------------------------------------------

# Package used for date conversion:
library(sjmisc)
library(tidyverse)
# install.packages('sjmisc')

# 2. Reading the data --------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
CRC_path <- as.character(args[1])

CRC_data <- readRDS(CRC_path)

# CRC_data <- CRC_case_data_QC_final[1:100,]

CRC_data <- CRC_data %>% add_column(eid = rownames(CRC_data),.before = 'group')

# clean names & sort types
sex_male <- as.factor(ifelse(CRC_data$sex=='Male',1,0))
CRC_data <- CRC_data %>% add_column(sex_male,.after = 'sex')
CRC_data <- CRC_data %>% select(-'sex')
CRC_data <- CRC_data %>% select(-'birth_month')

CRC_data <- CRC_data %>% rename(cc_status = CRC_case)

CRC_data$pace_maker <- as.factor(plyr::mapvalues(CRC_data$pace_maker,from=c('No','Yes'),to=c('0','1')))

# deal with -10, -7, -3, and -1 ------------------
CRC_data$number_days_walk_ten <- ifelse(CRC_data$number_days_walk_ten==-2,0,CRC_data$number_days_walk_ten)
CRC_data$number_days_walk_ten <- ifelse(CRC_data$number_days_walk_ten<0,
                                        median(CRC_data$number_days_walk_ten),CRC_data$number_days_walk_ten)

names = c("phys_act_walk_duration","sleep_duration","weekly_red_wine_intake",
          "weekly_spirits_intake","weekly_fortified_wine_intake","weekly_champagne_and_white_wine_intake",
          "weekly_beer_intake","father_death_age","mother_death_age")

for (name in names){
  CRC_data[,name] <- ifelse(CRC_data[,name]<0,median(CRC_data[,name]),CRC_data[,name])
}

names = c('time_outdoors_summer','time_outdoors_winter','time_TV','time_computer',
          'diet_cooked_vegatable',"diet_raw_vegatable","diet_fresh_fruit",
          "diet_dried_fruit","diet_bread","diet_cereal","diet_tea","diet_water") 

for (name in names){
  CRC_data[,name] <- ifelse(CRC_data[,name]==-10,0.5,CRC_data[,name])
  CRC_data[,name] <-ifelse(CRC_data[,name]<0,median(CRC_data[,name]),CRC_data[,name])
}

CRC_data <- CRC_data %>% select(-"smoking_second_hand_home")    
CRC_data <- CRC_data %>% select(-"smoking_second_hand_outside")

# 3. Add baseline model variables --------------

# BMI_cat

Model_BMI_cat <- ifelse( 18.5<= CRC_data$bmi & CRC_data$bmi <25,0,NA)
Model_BMI_cat <- ifelse( CRC_data$bmi >= 30,2,Model_BMI_cat)
Model_BMI_cat <- replace_na(Model_BMI_cat,1)
table(Model_BMI_cat)
Model_BMI_cat <- as.factor(Model_BMI_cat)

CRC_data <- CRC_data %>% add_column(Model_BMI_cat, .after='group')
CRC_data <- CRC_data %>% select(-'bmi')

# Height
# hist(CRC_data$height)
Model_height <- CRC_data$height
  
CRC_data <- CRC_data %>% add_column(Model_height, .after='group')
CRC_data <- CRC_data %>% select(-'height')

# Pack_years of smoking

# hist(CRC_data$smoking_pack_year)
Model_smoking_pack_year <- CRC_data$smoking_pack_year

CRC_data <- CRC_data %>% add_column(Model_smoking_pack_year, .after='group')
CRC_data <- CRC_data %>% select(-'smoking_pack_year')

# Sedentary

#hist(CRC_data$phys_act_mod_duration)
#hist(CRC_data$phys_act_vig_duration)
#CRC_data$phys_act_vig_duration=0
mod = (CRC_data$phys_act_mod_duration)*(CRC_data$phys_act_mod_days)
vig = (CRC_data$phys_act_vig_duration)*(CRC_data$phys_act_vig_days)

Model_Sedentary <- ifelse((mod >= 150 | vig >= 75),0,1)

Model_Sedentary <- as.factor(Model_Sedentary)

CRC_data <- CRC_data %>% add_column(Model_Sedentary, .after='group')
CRC_data <- CRC_data %>% select(-c('phys_act_mod_duration','phys_act_vig_duration',
                                   'phys_act_mod_days','phys_act_vig_days'))

# Alcohol_more
table(CRC_data$diet_alcohol_frequency)
Model_Alcohol_more <- plyr::mapvalues(CRC_data$diet_alcohol_frequency,
                            from=c("Prefer not to answer","Daily or almost daily",
                                   "Three or four times a week","Once or twice a week",
                                   "One to three times a month","Special occasions only","Never"),
                            to=c(0,2,1,1,0,0,0))

Model_Alcohol_more <- as.factor(Model_Alcohol_more)

CRC_data <- CRC_data %>% add_column(Model_Alcohol_more, .after='group')
CRC_data <- CRC_data %>% select(-c('diet_alcohol_frequency'))

# Education
# head(CRC_data %>% select(starts_with('education')))
zero <- ifelse(CRC_data$`education_Prefer not to answer`+CRC_data$`education_None of the above` >0,0,0)
               
one <- ifelse(CRC_data$`education_A levels/AS levels or equivalent` +CRC_data$`education_CSEs or equivalent`+
  CRC_data$`education_NVQ or HND or HNC or equivalent`+CRC_data$`education_O levels/GCSEs or equivalent` >0,1,0)
  
two <- ifelse(CRC_data$`education_Other professional qualifications eg: nursing, teaching`>0,2,0)

three <- ifelse(CRC_data$`education_College or University degree`>0,3,0)

Model_Education <- factor(apply(cbind(zero,one,two,three),1,max),levels=c(0,1,2,3),
                             labels=c('less than high school','high school','other professions','college/uni'))
Model_Education <- as.factor(Model_Education)

CRC_data <- CRC_data %>% add_column(Model_Education, .after='group')
CRC_data <- CRC_data %>% select(-starts_with('education'))

# Diabetes
table(CRC_data$diabetes_diagnosed_doctor)
Model_Diabetes <- plyr::mapvalues(CRC_data$diabetes_diagnosed_doctor,
                                      from=c("Prefer not to answer",'Do not know',
                                             'No','Yes'),
                                      to=c(0,0,0,1))
Model_Diabetes <- as.factor(Model_Diabetes)

CRC_data <- CRC_data %>% add_column(Model_Diabetes, .after='group')
CRC_data <- CRC_data %>% select(-'diabetes_diagnosed_doctor')

# Low_folate

vitamin <- ifelse(CRC_data$`vitamin_6155_Folic acid or Folate (Vit B9)`>0,1,0)
raw_veg <- ifelse(CRC_data$diet_raw_vegatable>=5,1,0)
poultry <- as.numeric(plyr::mapvalues(CRC_data$diet_poultry,
                           from=c("Prefer not to answer","Do not know",
                                  "Never", "Less than once a week","Once a week",
                                  "2-4 times a week","5-6 times a week","Once or more daily"),
                           to=c(0,0,0,0,0,1,1,1)))-1
pork <- as.numeric(plyr::mapvalues(CRC_data$diet_pork,
                           from=c("Prefer not to answer","Do not know",
                                  "Never", "Less than once a week","Once a week",
                                  "2-4 times a week","5-6 times a week","Once or more daily"),
                           to=c(0,0,0,0,0,1,1,1)))-1
beef <- as.numeric(plyr::mapvalues(CRC_data$diet_beef,
                        from=c("Prefer not to answer","Do not know",
                               "Never", "Less than once a week","Once a week",
                               "2-4 times a week","5-6 times a week","Once or more daily"),
                        to=c(0,0,0,0,0,1,1,1)))-1

Model_Low_folate <- ifelse(rowSums(cbind(vitamin,raw_veg,poultry,pork,beef))<=1,1,0)
Model_Low_folate <- as.factor(Model_Low_folate)
CRC_data <- CRC_data %>% add_column(Model_Low_folate, .after='group')

# Low_fruit
Model_Low_fruit <- ifelse((CRC_data$diet_fresh_fruit + CRC_data$diet_dried_fruit)<5,1,0)
Model_Low_fruit <- as.factor(Model_Low_fruit)
CRC_data <- CRC_data %>% add_column(Model_Low_fruit, .after='group')

# Low_veg
Model_Low_veg <- ifelse((CRC_data$diet_raw_vegatable + CRC_data$diet_cooked_vegatable)<5,1,0)
Model_Low_veg <- as.factor(Model_Low_veg)
CRC_data <- CRC_data %>% add_column(Model_Low_veg, .after='group')

# Greater_red_meat

lamb <- as.numeric(plyr::mapvalues(CRC_data$diet_lamb,
                               from=c("Prefer not to answer","Do not know",
                                      "Never", "Less than once a week","Once a week",
                                      "2-4 times a week","5-6 times a week","Once or more daily"),
                               to=c(0,0,0,0,0,1,1,1)))-1

Model_Greater_red_meat <- ifelse(rowSums(cbind(lamb,pork,beef))>=2,1,0)
Model_Greater_red_meat <- as.factor(Model_Greater_red_meat)
CRC_data <- CRC_data %>% add_column(Model_Greater_red_meat, .after='group')

# Greater_processed_meat
table(CRC_data$diet_processed_meat)

Model_Greater_processed_meat <- plyr::mapvalues(CRC_data$diet_lamb,
                                                       from=c("Prefer not to answer","Do not know",
                                                              "Never", "Less than once a week","Once a week",
                                                              "2-4 times a week","5-6 times a week","Once or more daily"),
                                                       to=c(0,0,0,0,0,1,1,1))
Model_Greater_processed_meat <- as.factor(Model_Greater_processed_meat)
CRC_data <- CRC_data %>% add_column(Model_Greater_processed_meat, .after='group')

# Lower_fibre
Model_Lower_fibre <- ifelse((CRC_data$diet_raw_vegatable + CRC_data$diet_fresh_fruit)<5,1,0)
Model_Lower_fibre <- as.factor(Model_Lower_fibre)
CRC_data <- CRC_data %>% add_column(Model_Lower_fibre, .after='group')

# Lower_Ca
Ca <- ifelse(CRC_data$diet_mineral_6719__Calcium==1,1,0)

milk <- as.numeric(plyr::mapvalues(CRC_data$diet_milk_type,from=levels(CRC_data$diet_milk_type),
                                     to=c(0,0,1,1,1,0,0,0)))-1

cheese <- as.numeric(plyr::mapvalues(CRC_data$diet_cheese,
                                     from=c("Prefer not to answer","Do not know",
                                            "Never", "Less than once a week","Once a week",
                                            "2-4 times a week","5-6 times a week","Once or more daily"),
                                     to=c(0,0,0,0,0,1,1,1)))-1

Model_Lower_Ca <- ifelse(rowSums(cbind(Ca,milk,cheese))<1,1,0)
Model_Lower_Ca <- as.factor(Model_Lower_Ca)
CRC_data <- CRC_data %>% add_column(Model_Lower_Ca, .after='group')

# Aspirin
Model_Asprin <- ifelse(CRC_data$medication_field6154_Aspirin==1,1,0)
Model_Asprin <- as.factor(Model_Asprin)
CRC_data <- CRC_data %>% add_column(Model_Asprin, .after='group')

# NSAIDS
Model_NSAID <- ifelse(CRC_data$`medication_field6154_Ibuprofen (e.g. Nurofen)`==1,1,0)
Model_NSAID <- as.factor(Model_NSAID)
CRC_data <- CRC_data %>% add_column(Model_NSAID, .after='group')

# 4. Deal with variables with multi-levels -----------

CRC_data <- CRC_data %>% select(-'birth_weight')

concat <- function(data,data_dummy) {
  dummy_sep <- rep(1:nrow(data),times=ncol(data))
  res = NULL
  for (i in 1:(nrow(data))) {
    res = rbind(res,colSums(data_dummy[dummy_sep==i,],na.rm=TRUE))
  }
  colnames(res) = colnames(data_dummy)
  return (res)
}

# accommodation
levels = levels(CRC_data[,'accommodation'])
select <- factor(CRC_data[,'accommodation'],levels=levels,labels=levels)

select_dummy <- sjmisc::to_dummy(select,var.name='accommodation',suffix = "label")
select_dummy <- as.data.frame(apply(select_dummy,2,as.factor))

CRC_data <- CRC_data %>% add_column(select_dummy, .after = "accommodation")
CRC_data <- CRC_data %>% select(-'accommodation')

# number_vehicles
levels = levels(CRC_data$number_vehicles)
CRC_data$number_vehicles <- as.character(CRC_data$number_vehicles)

num_vehicles <- plyr::mapvalues(CRC_data$number_vehicles,
                                from=c("Prefer not to answer","Do not know","None","One","Two","Three",
                                       "Four or more"),to=c(1,1,0,1,2,3,4))

num_vehicles <- as.numeric(num_vehicles)
CRC_data <- CRC_data %>% add_column(num_vehicles,.after = 'number_vehicles')
CRC_data <- CRC_data %>% select(-'number_vehicles')

# household_income
levels = levels(CRC_data$household_income)
CRC_data$household_income <- as.character(CRC_data$household_income)

household_income_GBP <- plyr::mapvalues(CRC_data$household_income,
                                from=c( "Prefer not to answer","Do not know","Less than 18,000","18,000 to 30,999",
                                "31,000 to 51,999","52,000 to 100,000","Greater than 100,000"),
                                to=c(25000,25000,9000,25000,41500,76000,100000))

household_income_GBP <- as.numeric(household_income_GBP)
CRC_data <- CRC_data %>% add_column(household_income_GBP,.after = 'household_income')
CRC_data <- CRC_data %>% select(-'household_income')

# length_mobile
CRC_data <- CRC_data %>% select(-'length_mobile')

# weekly_mobile
levels = levels(CRC_data$weekly_mobile)
CRC_data$weekly_mobile <- as.character(CRC_data$weekly_mobile)

weekly_mobile_len_hrs <- plyr::mapvalues(CRC_data$weekly_mobile,
                                        from=c("Prefer not to answer","Do not know","Less than 5mins",
                                               "5-29 mins","30-59 mins","1-3 hours","4-6 hours","More than 6 hours"),
                                        to=c(0.25,0.25,0.05,0.25,0.75,2,5,6))

weekly_mobile_len_hrs <- as.numeric(weekly_mobile_len_hrs)
CRC_data <- CRC_data %>% add_column(weekly_mobile_len_hrs,.after = 'weekly_mobile')
CRC_data <- CRC_data %>% select(-'weekly_mobile')

# smoking_household
levels = levels(CRC_data$smoking_household)
CRC_data$smoking_household <- as.character(CRC_data$smoking_household)

smoking_num_household <- plyr::mapvalues(CRC_data$smoking_household,
                                         from=c("Prefer not to answer","No",
                                                "Yes, one household member smokes",
                                                "Yes, more than one household member smokes"),
                                         to=c(0,0,1,2))

smoking_num_household <- as.numeric(smoking_num_household)
CRC_data <- CRC_data %>% add_column(smoking_num_household,.after = 'smoking_household')
CRC_data <- CRC_data %>% select(-'smoking_household')

# chronotype
levels = levels(CRC_data$chronotype)
CRC_data$chronotype <- as.numeric(CRC_data$chronotype)

chronotype_cat <- plyr::mapvalues(CRC_data$chronotype,from=c(1,2,3,4,5,6),
                                         to=c('Prefer not to answer','Do not know','morning_person',
                                         'morning_person','evening_person','evening_person'))

chronotype_cat <- as.factor(chronotype_cat)
CRC_data <- CRC_data %>% add_column(chronotype_cat,.after = 'chronotype')
CRC_data <- CRC_data %>% select(-'chronotype')

#
names = c('chronotype_cat','smoking_current','smoking_past')

for (name in names){
  levels = levels(CRC_data[,name])
  select <- factor(CRC_data[,name],levels = levels,labels=levels)
  select_dummy <- sjmisc::to_dummy(select,var.name = name,suffix = "label")
  select_dummy <- as.data.frame(apply(select_dummy,2,as.factor))
  CRC_data <- CRC_data %>% add_column(select_dummy, .after = name)
  CRC_data <- CRC_data %>% select(-name)
}

# diet
diet_salt_cat <- plyr::mapvalues(CRC_data$diet_salt,from=levels(CRC_data$diet_salt),to=c(0,0,0,1,1))
CRC_data <- CRC_data %>% add_column(diet_salt_cat,.after = 'diet_salt')
CRC_data <- CRC_data %>% select(-'diet_salt')

names = c('diet_oily_fish','diet_non_oily_fish','diet_processed_meat',
          'diet_poultry','diet_beef','diet_lamb', 'diet_pork','diet_cheese')

diet_oily_fish_cat <- plyr::mapvalues(CRC_data[,'diet_oily_fish'],
                          from=c("Prefer not to answer","Do not know",
                                 "Never", "Less than once a week","Once a week",
                                 "2-4 times a week","5-6 times a week","Once or more daily"),
                          to=c(0,0,0,0,0,1,1,1))
CRC_data <- CRC_data %>% add_column(diet_oily_fish_cat,.after = 'diet_oily_fish')
CRC_data <- CRC_data %>% select(-'diet_oily_fish')

diet_non_oily_fish_cat <- plyr::mapvalues(CRC_data[,'diet_non_oily_fish'],
                                      from=c("Prefer not to answer","Do not know",
                                             "Never", "Less than once a week","Once a week",
                                             "2-4 times a week","5-6 times a week","Once or more daily"),
                                      to=c(0,0,0,0,0,1,1,1))
CRC_data <- CRC_data %>% add_column(diet_non_oily_fish_cat,.after = 'diet_non_oily_fish')
CRC_data <- CRC_data %>% select(-'diet_non_oily_fish')

diet_processed_meat_cat <- plyr::mapvalues(CRC_data[,'diet_processed_meat'],
                                      from=c("Prefer not to answer","Do not know",
                                             "Never", "Less than once a week","Once a week",
                                             "2-4 times a week","5-6 times a week","Once or more daily"),
                                      to=c(0,0,0,0,0,1,1,1))
CRC_data <- CRC_data %>% add_column(diet_processed_meat_cat,.after = 'diet_processed_meat')
CRC_data <- CRC_data %>% select(-'diet_processed_meat')


diet_poultry_cat <- plyr::mapvalues(CRC_data[,'diet_poultry'],
                                      from=c("Prefer not to answer","Do not know",
                                             "Never", "Less than once a week","Once a week",
                                             "2-4 times a week","5-6 times a week","Once or more daily"),
                                      to=c(0,0,0,0,0,1,1,1))
CRC_data <- CRC_data %>% add_column(diet_poultry_cat,.after = 'diet_poultry')
CRC_data <- CRC_data %>% select(-'diet_poultry')


diet_beef_cat <- plyr::mapvalues(CRC_data[,'diet_beef'],
                                      from=c("Prefer not to answer","Do not know",
                                             "Never", "Less than once a week","Once a week",
                                             "2-4 times a week","5-6 times a week","Once or more daily"),
                                      to=c(0,0,0,0,0,1,1,1))
CRC_data <- CRC_data %>% add_column(diet_beef_cat,.after = 'diet_beef')
CRC_data <- CRC_data %>% select(-'diet_beef')


diet_lamb_cat <- plyr::mapvalues(CRC_data[,'diet_lamb'],
                                      from=c("Prefer not to answer","Do not know",
                                             "Never", "Less than once a week","Once a week",
                                             "2-4 times a week","5-6 times a week","Once or more daily"),
                                      to=c(0,0,0,0,0,1,1,1))
CRC_data <- CRC_data %>% add_column(diet_lamb_cat,.after = 'diet_lamb')
CRC_data <- CRC_data %>% select(-'diet_lamb')

diet_pork_cat <- plyr::mapvalues(CRC_data[,'diet_pork'],
                                      from=c("Prefer not to answer","Do not know",
                                             "Never", "Less than once a week","Once a week",
                                             "2-4 times a week","5-6 times a week","Once or more daily"),
                                      to=c(0,0,0,0,0,1,1,1))
CRC_data <- CRC_data %>% add_column(diet_pork_cat,.after = 'diet_pork')
CRC_data <- CRC_data %>% select(-'diet_pork')

diet_cheese_cat <- plyr::mapvalues(CRC_data[,'diet_cheese'],
                                      from=c("Prefer not to answer","Do not know",
                                             "Never", "Less than once a week","Once a week",
                                             "2-4 times a week","5-6 times a week","Once or more daily"),
                                      to=c(0,0,0,0,0,1,1,1))
CRC_data <- CRC_data %>% add_column(diet_cheese_cat,.after = 'diet_cheese')
CRC_data <- CRC_data %>% select(-'diet_cheese')

names = c('diet_milk_type','diet_bread_type', 'diet_coffee_type')

for (name in names){
  levels = levels(CRC_data[,name])
  select <- factor(CRC_data[,name],levels = levels,labels=levels)
  select_dummy <- sjmisc::to_dummy(select,var.name = name,suffix = "label")
  select_dummy <- as.data.frame(apply(select_dummy,2,as.factor))
  CRC_data <- CRC_data %>% add_column(select_dummy, .after = name)
  CRC_data <- CRC_data %>% select(-name)
}

# diet_hot_drink_temperature
levels = levels(CRC_data$diet_hot_drink_temperature)
CRC_data$diet_hot_drink_temperature <- as.character(CRC_data$diet_hot_drink_temperature)

diet_very_hot_drink <- plyr::mapvalues(CRC_data$diet_hot_drink_temperature,
                                         from=c("Prefer not to answer","Do not drink hot drinks","Very hot",
                                                "Hot","Warm"),
                                         to=c(0,0,1,0,0))
diet_very_hot_drink <- as.factor(diet_very_hot_drink)
CRC_data <- CRC_data %>% add_column(diet_very_hot_drink,.after = 'diet_hot_drink_temperature')
CRC_data <- CRC_data %>% select(-'diet_hot_drink_temperature')

# diet_major_changes
CRC_data <- CRC_data %>% select(-'diet_major_changes')

# neuro

names = c('neuro_insomia','neuro_worried',"neuro_mood_swings","neuro_miserableness","neuro_irritability",                                                                     
          "neuro_hurt_feelings","neuro_fed_up_feelings" ,"neuro_nervous_feelings",                                                                 
          "neuro_anxious_feelings",'early_life_breastfed_as_baby',
          'early_life_comparative_body_size_age_ten',
          'early_life_comparative_height_age_ten')

for (name in names){
  levels = levels(CRC_data[,name])
  select <- factor(CRC_data[,name],levels = levels,labels=levels)
  select_dummy <- sjmisc::to_dummy(select,var.name = name,suffix = "label")
  select_dummy <- as.data.frame(apply(select_dummy,2,as.factor))
  CRC_data <- CRC_data %>% add_column(select_dummy, .after = name)
  CRC_data <- CRC_data %>% select(-name)
}

# skin_colour 
  
names = c('skin_colour', 'birth_maternal_smoking','father_alive',
          'mother_alive' )

for (name in names){
  levels = levels(CRC_data[,name])
  select <- factor(CRC_data[,name],levels = levels,labels=levels)
  select_dummy <- sjmisc::to_dummy(select,var.name = name,suffix = "label")
  select_dummy <- as.data.frame(apply(select_dummy,2,as.factor))
  CRC_data <- CRC_data %>% add_column(select_dummy, .after = name)
  CRC_data <- CRC_data %>% select(-name)
}

# overall_health_rating
# 0-Poor,1-Fair,2-Good,3-Excellent
CRC_data <- CRC_data %>% select(-'wear_glasses')

levels = levels(CRC_data$overall_health_rating)
CRC_data$overall_health_rating <- as.character(CRC_data$overall_health_rating)

overall_health_rate <- plyr::mapvalues(CRC_data$overall_health_rating,
                                from=c("Prefer not to answer","Do not know","Excellent",
                                       "Good","Fair","Poor"),to=c(2,2,3,2,1,0))

overall_health_rate <- as.numeric(overall_health_rate)
CRC_data <- CRC_data %>% add_column(overall_health_rate,.after = 'overall_health_rating')
CRC_data <- CRC_data %>% select(-'overall_health_rating')

# remove some 
CRC_data <- CRC_data %>% select(-'chest_pain')
CRC_data <- CRC_data %>% select(-'smoking_ever')

# smoking_status, diet_alcohol, smoking_ever
names = c('smoking_status','diet_alcohol')

for (name in names){
  levels = levels(CRC_data[,name])
  select <- factor(CRC_data[,name],levels = levels,labels=levels)
  select_dummy <- sjmisc::to_dummy(select,var.name = name,suffix = "label")
  select_dummy <- as.data.frame(apply(select_dummy,2,as.factor))
  CRC_data <- CRC_data %>% add_column(select_dummy, .after = name)
  CRC_data <- CRC_data %>% select(-name)
}

# ethnicity
levels = levels(CRC_data$ethnicity)
CRC_data$ethnicity <- as.character(CRC_data$ethnicity)

ethnicity_cat <- plyr::mapvalues(CRC_data$ethnicity,
                                       from=c("Prefer not to answer","Do not know","White","Mixed",
                                              "Asian or Asian British","Black or Black British","Chinese","Other ethnic group",
                                              "British","Irish","Any other white background","White and Black Caribbean",
                                              "White and Black African","White and Asian","Any other mixed background",
                                              "Indian","Pakistani","Bangladeshi","Any other Asian background",
                                              "Caribbean","African","Any other Black background"),
                                       to=c("Prefer not to answer","Do not know","White","Mixed",
                                            "Asian","Black","Asian","Other",
                                            "White","White","White","Mixed",
                                            "Mixed","Mixed","Mixed",
                                            "Asian","Asian","Asian","Asian",
                                            "Black","Black","Black"))
ethnicity_cat <- as.factor(ethnicity_cat)
CRC_data <- CRC_data %>% add_column(ethnicity_cat,.after = 'ethnicity')
CRC_data <- CRC_data %>% select(-'ethnicity')

names = c('ethnicity_cat')

for (name in names){
  levels = levels(CRC_data[,name])
  select <- factor(CRC_data[,name],levels = levels,labels=levels)
  select_dummy <- sjmisc::to_dummy(select,var.name = name,suffix = "label")
  select_dummy <- as.data.frame(apply(select_dummy,2,as.factor))
  CRC_data <- CRC_data %>% add_column(select_dummy, .after = name)
  CRC_data <- CRC_data %>% select(-name)
}

# weight
Weight = CRC_data$weight
CRC_data <- CRC_data %>% add_column(Weight,.after='age_recr')
CRC_data <- CRC_data %>% select(-'weight')

# make variables factor
idx = which(colnames(CRC_data)=='occupation_Doing unpaid or voluntary work')
CRC_data[,idx:ncol(CRC_data)] <- as.data.frame(apply(CRC_data[,idx:ncol(CRC_data)],2,as.factor))

# delete nonsense variables
CRC_data <- CRC_data %>% select(-contains('Prefer not to answer'))
CRC_data <- CRC_data %>% select(-contains('Do not know'))
CRC_data <- CRC_data %>% select(-contains('None of the above'))

# transform char to factor
CRC_data <- CRC_data %>% mutate_if(is.character,as.factor)

# 5. Add tt_status ----------------------------------

case_id <- CRC_data[CRC_data$cc_status==1,"eid"]

set.seed(3141592)
train_idx <- sample(1:length(case_id), 0.75 * length(case_id))
train_id <- (case_id[train_idx])

train_cc_id <- (CRC_data$group %in% train_id) 

tt_status <- ifelse(train_cc_id,'train','test')
CRC_data <- CRC_data %>% add_column(tt_status,.after = 'cc_status')

print('test group table')
print(table(table(CRC_data[CRC_data$tt_status=='test','group'])))
print('train group table')
print(table(table(CRC_data[CRC_data$tt_status=='train','group'])))
print('all group table')
print(table(table(CRC_data[,'group'])))

# 6. Export file ----------------------------------

saveRDS(CRC_data, "Outputs/CRC_data_final.rds")

# minor changes
levels = levels(CRC_data[,'Model_Education'])
select <- factor(CRC_data[,'Model_Education'],levels=levels,labels=levels)

select_dummy <- sjmisc::to_dummy(select,var.name='Model_Education',suffix = "label")
select_dummy <- as.data.frame(apply(select_dummy,2,as.factor))

CRC_data <- CRC_data %>% add_column(select_dummy, .after = "Model_Education")
CRC_data <- CRC_data %>% select(-'Model_Education')

levels = levels(CRC_data[,'Model_Alcohol_more'])
select <- factor(CRC_data[,'Model_Alcohol_more'],levels=levels,labels=levels)

select_dummy <- sjmisc::to_dummy(select,var.name='Model_Alcohol_more',suffix = "label")
select_dummy <- as.data.frame(apply(select_dummy,2,as.factor))

CRC_data <- CRC_data %>% add_column(select_dummy, .after = "Model_Alcohol_more")
CRC_data <- CRC_data %>% select(-'Model_Alcohol_more')

levels = levels(CRC_data[,'Model_BMI_cat'])
select <- factor(CRC_data[,'Model_BMI_cat'],levels=levels,labels=levels)

select_dummy <- sjmisc::to_dummy(select,var.name='Model_BMI_cat',suffix = "label")
select_dummy <- as.data.frame(apply(select_dummy,2,as.factor))

CRC_data <- CRC_data %>% add_column(select_dummy, .after = "Model_BMI_cat")
CRC_data <- CRC_data %>% select(-'Model_BMI_cat')

CRC_data <- CRC_data %>% mutate_if(is.character,as.factor)

write.csv(CRC_data, "Outputs/CRC_data_final_V2.csv")
saveRDS(CRC_data, "Outputs/CRC_data_final_V2.rds")


