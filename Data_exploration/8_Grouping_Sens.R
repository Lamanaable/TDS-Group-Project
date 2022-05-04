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

CRC_data = readRDS("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_5_year_ordered_V3.rds")

# 3. Split data -------------------------------------------------------

age_diag <- CRC_data$age_recr + CRC_data$CRC_dvlp_year
CRC_data <- CRC_data%>% add_column(age_diag,.before='CRC_dvlp_year')

idx_early <- CRC_data[replace_na(CRC_data$age_diag < 50,FALSE),]
idx_early <- rownames(idx_early)

idx_late <- CRC_data[replace_na(CRC_data$age_diag >= 50,FALSE),]
idx_late <- rownames(idx_late)

CRC_Early <- CRC_data[CRC_data$group %in% idx_early,]
CRC_Late <- CRC_data[CRC_data$group %in% idx_late,]

saveRDS(CRC_Early,'Sensitivity_Data/CRC_Early.rds')
saveRDS(CRC_Late,'Sensitivity_Data/CRC_Late.rds')

# 4. Time to diagnosis Data ------------------------------------------------

# cencored version
group_names <- names(table(CRC_data$group))

CRC_data_time = rep(NA,nrow(CRC_data))

for (group in group_names) {
  CRC_group = CRC_data[which(CRC_data$group == group),]
  time = CRC_group$CRC_dvlp_year[!is.na(CRC_group$CRC_dvlp_year)]
  CRC_data_time[which(CRC_data$group == group)] = time
}

CRC_data <- CRC_data %>% add_column(CRC_data_time,.before = 'cc_status')

CRC_data <- CRC_data %>% rename(
  time = CRC_data_time,
  status = cc_status
)

CRC_data <- CRC_data[,7:ncol(CRC_data)]

saveRDS(CRC_data,'Sensitivity_Data/CRC_data_time_status_test.rds')

 
# control 5 year 0 version
CRC_data = readRDS("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_5_year_ordered_V3.rds")

CRC_data_time = ifelse(CRC_data$cc_status=='0',5,CRC_data$CRC_dvlp_year)

CRC_data <- CRC_data %>% add_column(CRC_data_time,.before = 'cc_status')

CRC_data <- CRC_data %>% rename(
  time = CRC_data_time,
  status = cc_status
)

CRC_data <- CRC_data[,7:ncol(CRC_data)]

saveRDS(CRC_data,'Sensitivity_Data/CRC_data_time_status.rds')
