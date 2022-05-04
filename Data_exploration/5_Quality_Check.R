# QUALITY CHECK

# Date: 15 Feb 2022
# Authors: TDS Group 2 (Wei)
# Description : Apply quality control, identify missingness, apply imputation 
# output: CRC_QC_final

# 1. Setup -------------------------------------------------------------------
# Package used 
library(tidyverse)

# 2. Reading the data --------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
ukb_path <- as.character(args[1])
CRC_path <- as.character(args[2])
ukb <- readRDS(ukb_path)
CRC_data <- readRDS(CRC_path)

x = 15 # = ncol of CRC_data first few important columns

# testing
# CRC_data <- CRC_case_data_linked
# CRC_data <- CRC_data[1:100, ]
# ukb <- ukb_extracted_new_2
ukb$eid <- rownames(ukb)
ukb <- ukb[ukb$eid %in% CRC_data$eid,]

# 3. quality check ------
# replace NA with 3000-01-01

# quality checks
CRC_data$date_diagnosis <- CRC_data$date_diagnosis %>% replace_na(.,'3000-01-01')
CRC_data$death_date_0 <- CRC_data$death_date_0 %>% replace_na(.,'3000-01-01')

CRC_data_checked <- CRC_data

## 4. missingness check------
# # count initial NaNs--------

split <- split(CRC_data_checked,CRC_data_checked$CRC_case)
CRC_control <- split$`0`
CRC_case <- split$`1`

missing_case <- colSums(is.na(CRC_case))/nrow(CRC_case)
missing_control <- colSums(is.na(CRC_control))/nrow(CRC_control)

missing_all <- colSums(is.na(CRC_data_checked))/nrow(CRC_data_checked)

missing_table <- cbind(missing_case,missing_control,missing_all)

# quality check of some variables, impute some from ukb_extracted_new--------
keep <- c(1:x)

name <- colnames(CRC_data_checked)[-keep]

remove_name <- names(which(missing_table[name,3] > 0.40))

missing_ukb <- colSums(is.na(ukb[,remove_name]))/nrow(ukb)

keep_name <- c(names(missing_ukb[missing_ukb<=0.40]),'smoking_pack_year.0.0',
               'phys_act_vig_duration.0.0')

print('these columns are checked and kept')
print(keep_name)

CRC_data_checked[,keep_name] <- ukb[,keep_name]
CRC_data_checked$smoking_pack_year.0.0 <- replace_na(CRC_data_checked$smoking_pack_year.0.0,0)
CRC_data_checked$phys_act_vig_duration.0.0 <- replace_na(CRC_data_checked$phys_act_vig_duration.0.0,0)

# clean names
names_clean <- names(CRC_data_checked  %>% select(contains('.0.0')))

colnames(CRC_data_checked)[which(names(CRC_data_checked ) %in% names_clean)] <- 
  substr(names_clean,0,nchar(names_clean)-4)

name <- c('biomarker_IGF-1')
idx <- which(colnames(CRC_data_checked) %in% name)
colnames(CRC_data_checked)[idx] <- 'biomarker_IGF_one'

name <- c('die_non-oily_fish')
idx <- which(colnames(CRC_data_checked) %in% name)
colnames(CRC_data_checked)[idx] <- 'diet_non_oily_fish'

CRC_data_checked <- CRC_data_checked %>% rename(diet_raw_vegatable = die_raw_vegatable,
                            diet_beef = die_beef,
                            diet_lamb = die_lamb)

## deal with women_only variables ----------

CRC_data_checked <- CRC_data_checked %>% rename(
  women_screening_breast = screening_breast,
  women_screening_breast_years = screening_breast_years,
  women_screening_smear = screening_smear,
  women_age_HRT_start = age_HRT_start,
  women_age_HRT_stop = age_HRT_stop)

women_only <- names(CRC_data_checked %>% select(starts_with('women')))

women_only_idx <- which(names(CRC_data_checked) %in% women_only)

covars_women <- CRC_data_checked[,women_only]
covars_women$eid <- CRC_data_checked$eid
print('women_only covariates')
print(names(covars_women))

# saveRDS(covars_women, "Outputs/covars_women.rds")

# remove women_only from CRC_data
CRC_data_checked <- CRC_data_checked %>% select(-names(covars_women))

# sort variable order
order <- CRC_data_checked %>% select(c("CRC_screening_0","age_first_screened_CRC" ,
                                       "date_diagnosis","death_date_0",  "prevalent_case","incident_case", 
                                       "CRC_dvlp_year" ,"CRC_case","group","age_recr"))

CRC_data_checked <- cbind(order,CRC_data_checked[,15:ncol(CRC_data_checked)])

# new missing table
CRC_data_checked <- CRC_data_checked %>% select(-'cormorbidity_ICD10')
CRC_data_checked <- CRC_data_checked %>% select(-'CRC_screening')
CRC_data_checked <- CRC_data_checked %>% select(-'missingness')
CRC_data_checked <- CRC_data_checked %>% select(-'ethnic_genetic')
CRC_data_checked <- CRC_data_checked %>% select(-'genotype_batch')
CRC_data_checked <- CRC_data_checked %>% select(-'sex_genetic')
CRC_data_checked <- CRC_data_checked %>% select(-'genotyep_plate')
CRC_data_checked <- CRC_data_checked %>% select(-'genotype_well')
CRC_data_checked <- CRC_data_checked %>% select(-'death_cause_primary')
CRC_data_checked <- CRC_data_checked %>% select(-'cancer_type_ICD10')
CRC_data_checked <- CRC_data_checked %>% select(-'cormorbidity_ICD10_date')

split <- split(CRC_data_checked,CRC_data_checked$CRC_case)
CRC_control <- split$`0`
CRC_case <- split$`1`

missing_case <- colSums(is.na(CRC_case))/nrow(CRC_case)
missing_control <- colSums(is.na(CRC_control))/nrow(CRC_control)
missing_all <- colSums(is.na(CRC_data_checked))/nrow(CRC_data_checked)

missing_table <- cbind(missing_case,missing_control,missing_all)

keep <- c(1:9)

name <- colnames(CRC_data_checked)[-keep]
remove_name <- names(which(missing_table[name,3] > 0.40))

# remove missing > 40% ------------

print('columns removed - missing > 40%')
print(remove_name)

idx <- which(missing_table[remove_name,3] > 0.40)
CRC_data_selected <- select(CRC_data_checked,-all_of(remove_name))

## final missing_table
split <- split(CRC_data_selected,CRC_data_selected$CRC_case)
CRC_control <- split$`0`
CRC_case <- split$`1`

missing_case <- colSums(is.na(CRC_case))/nrow(CRC_case)
missing_control <- colSums(is.na(CRC_control))/nrow(CRC_control)
missing_all <- colSums(is.na(CRC_data_selected))/nrow(CRC_data_selected)

missing_table <- cbind(missing_case,missing_control,missing_all)

saveRDS(missing_table, "Outputs/missing_table.rds")
saveRDS(CRC_data_selected, "Outputs/CRC_case_data_QC.rds")

# 5. Imputation---------

# impute all missing
Impute_cols <- rownames(missing_table)[which(missing_table[,3]>0)]
Impute_cols <- Impute_cols[6:length(Impute_cols)]

print('columns need to be imputed')
print(Impute_cols)

Impute_cols_idx <- which(names(CRC_data_selected) %in% Impute_cols)

Impute_tool_names <- c('CRC_case','age_recr')

Impute_tool_id <- which(names(CRC_data_selected) %in% Impute_tool_names)

Impute_table <- CRC_data_selected[,union(Impute_cols_idx,Impute_tool_id)]

Impute_where <- as.data.frame(matrix(rep(FALSE,dim(Impute_table)[1]*dim(Impute_table)[2]),
                           ncol=ncol(Impute_table)))

colnames(Impute_where) <- colnames(Impute_table)
rownames(Impute_where) <- rownames(Impute_table)
Impute_where[,Impute_cols] <- is.na(Impute_table[,Impute_cols])

# m should ~ % the average percentage rate of missingness

m = mean(colSums(is.na(Impute_table))/nrow(Impute_table))*100
m =floor(m) +1
print(m)

imputed_Data <- mice::mice(Impute_table, where=Impute_where, method = 'pmm', m = m, maxit = 5, seed = 8)
  
completeData <- mice::complete(imputed_Data)

# replace original data with imputed data
CRC_data_selected[,Impute_cols] <- completeData[,names(completeData) %in% Impute_cols]

print('number of columns with missing > 0% in imputed table')
print(sum((colSums(is.na(CRC_data_selected)/nrow(CRC_data_selected))>0)))

print('columns with missing > 0% in imputed table')
print(names(CRC_data_selected[,(colSums(is.na(CRC_data_selected)/nrow(CRC_data_selected))>0)]))

# Exporting as RDS ----------------------------------------------------

saveRDS(CRC_data_selected, "Outputs/CRC_case_data_QC_final.rds")

