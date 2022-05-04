# COVARS WOMEN

# Date: 15 Feb 2022
# Authors: TDS Group 2 (Wei)
# Description : impute on covars_women

# 1. Setup -------------------------------------------------------------------
# Package used 
library(tidyverse)

# 2. Reading the data --------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
CRC_path <- as.character(args[1])
women_path <- as.character(args[2])
ukb_path <- as.character(args[3])

CRC_data <- readRDS(CRC_path)
women_data <- readRDS(women_path)
ukb <- readRDS(ukb_path)

# testing
# CRC_data <- CRC_case_data_QC
# women_data <- covars_women
# ukb <- ukb_extracted_new_2
CRC_data$eid <- rownames(CRC_data)
CRC_women <- CRC_data[CRC_data$sex=='Female',]
women_data <- women_data[women_data$eid %in% CRC_women$eid,]
# women_data <- women_data[1:100,]

ukb$eid <- rownames(ukb)
ukb <- ukb[ukb$eid %in% women_data$eid,]

women_data <- women_data %>% rename(
  screening_breast = women_screening_breast,
  screening_breast_years = women_screening_breast_years,
  screening_smear = women_screening_smear,
  age_HRT_start = women_age_HRT_start,
  age_HRT_stop = women_age_HRT_stop)

colnames(women_data) <- paste0(colnames(women_data),'.0.0')
colnames(women_data)[19] <- 'eid'

# 3. missingness check------
## count initial NaNs--------
missing_table <- colSums(is.na(women_data))/nrow(women_data)

# quality check of some variables, impute some from ukb_extracted_new--------
name <- colnames(women_data)
remove_name <- names(which(missing_table > 0.40))

missing_ukb <- colSums(is.na(ukb[,remove_name]))/nrow(ukb)

keep_name <- names(missing_ukb[missing_ukb<=0.40])
print('these columns are checked and kept')
print(keep_name)

women_data[,keep_name] <- ukb[,keep_name]

# clean names
names_clean <- names(women_data  %>% select(contains('.0.0')))
colnames(women_data)[which(names(women_data ) %in% names_clean)] <- 
  substr(names_clean,0,nchar(names_clean)-4)

women_data <- women_data %>% rename(
  women_screening_breast = screening_breast,
  women_screening_breast_years = screening_breast_years,
  women_screening_smear = screening_smear,
  women_age_HRT_start = age_HRT_start,
  women_age_HRT_stop = age_HRT_stop)

# new missing table

missing_all <- colSums(is.na(women_data))/nrow(women_data)

remove_name <- names(which(missing_all > 0.40))

# 4. remove missing > 40% ------------

print('columns removed - missing > 40%')
print(remove_name)

# remove women_pregnant--not discriminative

women_data <- select(women_data,-all_of(remove_name))
women_data <- select(women_data,-'women_pregnant')
women_data$women_screening_smear <- 
  as.factor(plyr::mapvalues(women_data$women_screening_smear,
                            from=c('Prefer not to answer','Do not know','No','Yes'),
                            to=c(1,1,0,1)))
women_data$women_screening_breast <- 
  as.factor(plyr::mapvalues(women_data$women_screening_breast,
                            from=c('Prefer not to answer','Do not know','No','Yes'),
                            to=c(1,1,0,1)))

women_data <- women_data %>% rename(screening_breast = women_screening_breast,
                                    screening_smear = women_screening_smear)

women_data <- women_data %>% select(-'women_screening_breast_years')

## final missing_table
missing_all <- colSums(is.na(women_data))/nrow(women_data)
print('Impute cols')
print(names(women_data))

women_data <- women_data[,1:(ncol(women_data)-1)]

# 5. Imputation---------
imputed_women <- mice::mice(women_data, method = 'pmm', m = 5, maxit = 10, seed = 1)
  
completeData <- mice::complete(imputed_women)

missing_all <- colSums(is.na(completeData))/nrow(completeData)

print(missing_all)

print('number of columns with missing > 0% in imputed_women table')
print(sum((colSums(is.na(completeData)/nrow(completeData))>0)))

completeData$eid <- rownames(completeData)

# Exporting as RDS ----------------------------------------------------

saveRDS(completeData, "Outputs/CRC_covars_women_only.rds")

