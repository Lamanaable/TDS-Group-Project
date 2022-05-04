##### Identifying cases of the 3xCRC locations


# Main data ---------------------------------------------------------------

CRC_ordered<-readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_5_year_ordered_V3.rds")

table(CRC_ordered$cc_status)


# Site: colon -------------------------------------------------------------

CRC_colon<-readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/outcome_definition/Outputs_colon/output_final.rds")

# Keep only incident cases

colon_incident_cases<-CRC_colon %>% filter(incident_case==1)
nrow(colon_incident_cases)

# List of eids of incident cases

colon_eids<-dput(colon_incident_cases$eid)

# Find the matching groups of these cases

colon_cases_only <- CRC_ordered %>% filter(rownames(CRC_ordered) %in% colon_eids)
colon_matching_groups<-dput(colon_cases_only$group)

# Create new dataset with cases and controls of correct groups for colon cases

CRC_colon_ordered <- CRC_ordered %>% filter(group %in% colon_matching_groups)
table(CRC_colon_ordered$cc_status) #2868 controls, 1454 cases

colon_controls<-table(CRC_colon_ordered$cc_status)[1]
colon_cases<-table(CRC_colon_ordered$cc_status)[2]



# Site: junction ----------------------------------------------------------

CRC_junction<-readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/outcome_definition/Outputs_junction/output_final.rds")

# Keep only incident cases

junction_incident_cases<-CRC_junction %>% filter(incident_case==1)
nrow(junction_incident_cases)

# List of eids of incident cases

junction_eids<-dput(junction_incident_cases)

# Find the matching groups of these cases

junction_cases_only <- CRC_ordered %>% filter(rownames(CRC_ordered) %in% junction_eids) # We have no junction cases?
junction_matching_groups<-dput(junction_cases_only$group) # Why does this have an output?

# Create new dataset with cases and controls of correct groups for colon cases

CRC_junction_ordered <- CRC_ordered %>% filter(group %in% junction_matching_groups)
table(CRC_junction_ordered$cc_status) 

junction_controls<-table(CRC_junction_ordered$cc_status)[1]
junction_cases<-table(CRC_junction_ordered$cc_status)[2]



# Site: rectum -------------------------------------------------------------

CRC_rectum<-readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/outcome_definition/Outputs_rectum/output_final.rds")

# Keep only incident cases

rectum_incident_cases<-CRC_rectum %>% filter(incident_case==1)
nrow(rectum_incident_cases)

# List of eids of incident cases

rectum_eids<-dput(rectum_incident_cases$eid)

# Find the matching groups of these cases

rectum_cases_only <- CRC_ordered %>% filter(rownames(CRC_ordered) %in% rectum_eids)
rectum_matching_groups<-dput(rectum_cases_only$group)

# Create new dataset with cases and controls of correct groups for colon cases

CRC_rectum_ordered <- CRC_ordered %>% filter(group %in% rectum_matching_groups)
table(CRC_rectum_ordered$cc_status) # 1196 controls and 604 cases

rectum_controls<-table(CRC_rectum_ordered$cc_status)[1]
rectum_cases<-table(CRC_rectum_ordered$cc_status)[2]




# Check on case numbers ---------------------------------------------------

total_controls<-table(CRC_ordered$cc_status)[1]
total_cases<-table(CRC_ordered$cc_status)[2]

colon_cases+junction_cases+rectum_cases==total_cases
colon_cases+junction_cases+rectum_cases - total_cases # 60 excess cases




# Investigating overlap ---------------------------------------------------

# # Ignoring junction as there are no junction cases after exclusion criteria applied
# 
# CRC_colon_rectum<-inner_join(CRC_colon_ordered, CRC_rectum_ordered)
# nrow(CRC_colon_rectum)
# 
# CRC_colon_rectum <- CRC_colon_rectum %>% filter(cc_status==1)
# nrow(CRC_colon_rectum) #Overlap of 138
# 
# View(CRC_colon_rectum)

# Looking at the common cases

common <- dput(intersect(rownames(colon_cases), rownames(rectum_cases)))  

CRC_colon_ordered <- CRC_colon_ordered %>% filter(rownames(CRC_colon_ordered) %in% common)
nrow(CRC_colon_ordered)

CRC_rectum_ordered <- CRC_rectum_ordered %>% filter(rownames(CRC_rectum_ordered) %in% common)
nrow(CRC_rectum_ordered)

common_data<-data.frame(matrix(NA,nrow=nrow(CRC_colon_ordered),ncol=4))
colnames(common_data) <- c("eid","colon_date_diag","rectum_date_diag","case_type")

common_data$eid<-rownames(CRC_colon_ordered)
common_data$colon_date_diag<-CRC_colon_ordered$date_diagnosis
common_data$rectum_date_diag<-CRC_rectum_ordered$date_diagnosis

common_data$case_type<-ifelse(common_data$colon_date_diag<common_data$rectum_date_diag,"colon","rectum")
View(common_data) # all have the same data of diagnosis, so maybe can be classed as multiple sites upon diagnosis


# Therefore it looks like they can have multiple sites...


# Export data (no junction cases) -----------------------------------------


saveRDS(CRC_colon_ordered, "CRC_colon_only.rds")

saveRDS(CRC_rectum_ordered, "CRC_rectum_only.rds")


