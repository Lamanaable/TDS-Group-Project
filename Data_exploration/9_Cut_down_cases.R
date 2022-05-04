# cutting down cases to only within 5 years
CRC_data = readRDS("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_case_data_QC_final.rds")

CRC_data$group <- as.character(CRC_data$group)

length(which(CRC_data$CRC_dvlp_year <=5))

keep = CRC_data[which(CRC_data$CRC_dvlp_year <=5),]

group = keep$group

CRC_data <- CRC_data[CRC_data$group %in% group,]

saveRDS(CRC_data,'Outputs/CRC_case_data_QC_final_5_year.rds')

# cutting down cases to only within 2 years
CRC_data = readRDS("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_case_data_Matched.rds")

CRC_data$group <- as.character(CRC_data$group)

length(which(CRC_data$CRC_dvlp_year <=2))

keep = CRC_data[which(CRC_data$CRC_dvlp_year <=2),]

group = keep$group

CRC_data <- CRC_data[CRC_data$group %in% group,]

dim(CRC_data)

saveRDS(CRC_data,'Outputs/CRC_case_data_Matched_2_year.rds')

# cutting down cases to only within 2 years
CRC_data_5 = readRDS("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_case_data_Matched_5_year.rds")
CRC_women = readRDS("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_covars_women_only.rds")

idx = as.character(CRC_data_5$eid)

keep = CRC_women[CRC_women$eid %in% idx,]

CRC_women <- keep

dim(CRC_women)

saveRDS(CRC_women,'Outputs/CRC_covars_women_only_5_year.rds')
