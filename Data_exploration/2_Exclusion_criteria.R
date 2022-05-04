# INCLUSION AND EXCLUSION CRITERIA

# Date: 21 Feb 2022
# Authors: TDS Group 2 (Wei)
# Description : Apply Inclusion/Exclusion criteria

# 1. Setup -------------------------------------------------------------------

# Package used for date conversion:
rm(list=ls())
library(tidyverse)

# 2. Reading the data --------------------------------------------------------
print("Data paths")
args <- commandArgs(trailingOnly = TRUE)
data_path <- as.character(args[1])
CRC_path <- as.character(args[2])
 
# CRC_data <- CRC_case_data_new_1 
print("Reading data")
ukb<- readRDS(data_path)
CRC_data <- readRDS(CRC_path)

# 3. Inclusion-Exclusion Criteria------
print("Assessment centre exclusion")
# Inclusion: include participants who have baseline records(essentially include all those with instance=0)
# table(ukb[,'assessment_centre.0.0'])
idx_baseline <- !is.na(ukb[,'assessment_centre.0.0'])
CRC_data <- CRC_data[idx_baseline,]

# Include people diagnosed within 10 years
print("10 year exclusion")
idx_CRC_diag_within_10yr <- replace_na(CRC_data$CRC_dvlp_year<=10,TRUE)
sum(CRC_data$CRC_dvlp_year<=10,na.rm=NA)
CRC_data <- CRC_data[idx_CRC_diag_within_10yr,]

# Exclusion:exclude participants diagnosed CRC at baseline
print("Baseline CRC exclusion")
idx_baseline <- replace_na(CRC_data[,'date_recr'] < CRC_data[,'date_diagnosis'],TRUE)
sum(CRC_data$CRC_dvlp_year<=0,na.rm=NA)
CRC_data <- CRC_data[idx_baseline,]

# Exclusion:exclude participants diagnose CRC within 12 months(=1year)
print("<1 year diagnosis")
idx_CRC_diag_after_1yr <- replace_na(CRC_data$CRC_dvlp_year>=1,TRUE)
sum(CRC_data$CRC_dvlp_year<1,na.rm=NA)
CRC_data <- CRC_data[idx_CRC_diag_after_1yr,]

# exclude age <55 yr and have been screened
print("<55 screened")
idx_screen_55 <- replace_na(CRC_data$age_first_screened_CRC<55,FALSE)
sum(CRC_data$age_first_screened_CRC>=55,na.rm=NA)
CRC_data <- CRC_data[!idx_screen_55,]

# Renaming the case variable so that it matches old scripts
print("renaming case status")
CRC_data <- CRC_data %>% rename(CRC_case = case)

# Exporting as RDS ----------------------------------------------------
print("Saving data")
saveRDS(CRC_data, "Outputs/CRC_case_data_exclusion.rds")


# count of cases after exclusion
print("Total cases")
sum(CRC_data$CRC_case)

print("incident cases")
table(CRC_data$incident_case)
print("prevalent cases")
table(CRC_data$prevalent_case)
print("total cases")
table(CRC_data$CRC_case)
