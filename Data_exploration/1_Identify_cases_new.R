## USING BARBARA'S IDENTIFIED CASES AND ADDING VARIABLES AND SCREENING INFORMATION

# Authors: Wei and Ellie
# Date: 21 Feb 2022



# 1. Setup -------------------------------------------------------------------

# Package used for date conversion:
rm(list=ls())
library(zoo)
library(tidyverse)


# 2. Reading the data --------------------------------------------------------

# Change this to the main UKB dataset for the main job!!!!!
print("Read data")
args <- commandArgs(trailingOnly = TRUE)
data_path <- as.character(args[1])
ukb <- readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/extraction_and_recoding/outputs/ukb_extracted.rds")
ukb_barbara<-readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/outcome_definition/Outputs/output_final.rds")

print("change vars")
ukb_barbara$sex<-ukb$sex.0.0
ukb_barbara$age_recr<-ukb$age.0.0
ukb_barbara$DOB_year<-ukb$birth_year.0.0
ukb_barbara$DOB_month<-ukb$birth_month.0.0
ukb_barbara$assessment_centre<-ukb[,'assessment_centre.0.0']
ukb_barbara$CRC_screening_0<-ukb$CRC_screening.0.0
ukb_barbara$CRC_screening_last<-ukb$CRC_screening_last.0.0
ukb_barbara$ethnicity<-ukb$ethnicity.0.0

ukb_barbara <- ukb_barbara %>% rename(death_date_0 = date_death)

#ukb_barbara$death_date_0<-ukb$death_date.0.0
#ukb_barbara$date_recr<-ukb$date_recr.0.0

#rename(ukb_barbara$date_death,)

# add column CRC_dvlp_year calculating the year participants developed CRC after recruitment 
#CRC_data_final$CRC_dvlp_year <- as.numeric(difftime(CRC_data_final$CRC_diag_date,CRC_data_final$date_recr))/365.25
# CRC_data$CRC_dvlp_year[!is.na(CRC_data$CRC_dvlp_year)] check what's in 

ukb_barbara$CRC_dvlp_year<-ukb_barbara$time_to_diagnosis/365.25


print("Defining age_first_screened_CRC function")
# add column first_screened_CRC_instance and age_first_screened_CRC
first_screened <-function(x){
  first_screened_CRC_instance =NA
  age_first_screened_CRC =NA
  if (!is.na(x['CRC_screening.0.0']) & x['CRC_screening.0.0'] >0) {
    first_screened_CRC_instance=0
    age_first_screened_CRC = x['age.0.0']
  } else if (!is.na(x['CRC_screening.1.0']) & x['CRC_screening.1.0'] >0) {
    first_screened_CRC_instance=1
    age_first_screened_CRC = 2012.5-x['birth_year.0.0'] #instance 1 from 2012 to 2013, use mean 2012.5
  } else if (!is.na(x['CRC_screening.2.0']) & x['CRC_screening.2.0'] >0) {
    first_screened_CRC_instance=2
    age_first_screened_CRC = 2017-x['birth_year.0.0'] #instance 2 from 2014 to 2020, use mean 2017
  }
  return (c(as.numeric(first_screened_CRC_instance),as.numeric(age_first_screened_CRC)))
}
print("running first screened loop")
res=NULL
for (i in 1:nrow(ukb)) {
  res = rbind(res,first_screened(ukb[i,]))
}
print("loop done!!!!!")

colnames(res) <- c('first_screened_CRC_instance','age_first_screened_CRC')

CRC_data_final <- cbind(ukb_barbara,res) 

print("saving data")
saveRDS(CRC_data_final, "Outputs/CRC_case_data_new1.rds")


