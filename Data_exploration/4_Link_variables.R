## LINK VARIABLES

# Date: 13 Feb 2022
# Authors: TDS Group 2 (Wei)
# Description: This script is designed to link all predictor variables of interest to the case_control data 
# Output: csv/rds file that includes all variables for all cases and controls 

# 1. Setup -------------------------------------------------------------------

# Package used for date conversion:
library(tidyverse)
library(data.table)
# install.packages('sjmisc')

# 2. Reading the data --------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
ukb_new_path <- as.character(args[1])
ukb_recoded_path <- as.character(args[2])
CRC_path <- as.character(args[3])
CRC_old <- as.character(args[4])

ukb_extracted <- readRDS(ukb_new_path)
ukb_code <- readRDS(ukb_recoded_path)
CRC_data <- readRDS(CRC_path)
CRC_old <- readRDS(CRC_old)

t0=Sys.time()

# CRC_data <- CRC_case_data_Matched
# ukb_extracted <- ukb_extracted_new

ukb_extracted$eid <- rownames(ukb_extracted)
CRC_old$eid <- rownames(CRC_old)
ukb_code$eid <- rownames(ukb_code)

ukb <- ukb_extracted[ukb_extracted$eid %in% CRC_data$eid,]
rownames(ukb) <- ukb$eid

# ukb_code <- ukb_final
ukb_code <- ukb_code[ukb_code$eid %in% CRC_data$eid,]
rownames(ukb_code) <- ukb_code$eid

# set.seed(1)
# ukb <- ukb[1:100,] 
# ukb_code <- ukb_code[1:100,]
# CRC_data <- CRC_data[1:100,]

print('start')

# 3. Link variable ----------------------------------

# ukb <- ukb %>% select(eid,ends_with(paste0('.0.',seq(0,50))))

# extract all with only one instance
choose = NULL
choose <- ukb_code %>% select(ends_with('.0.0'))
# deal with variables have more than one arrays

# pulse ------------------
#choose <- choose %>% add_column(pulse = rep(NA,nrow(choose)))
# print(head(choose))
choose$pulse <- apply(ukb %>% select(starts_with('pulse')),1,mean,na.rm=TRUE)
choose <- choose %>% select(-starts_with('pulse.0.'))

# sysbp------------------
#choose <- choose %>% add_column(sysBP = rep(NA,nrow(choose)))
choose$sysBP <- apply(ukb %>% select(starts_with('sysBP')),1,mean,na.rm=TRUE)
choose <- choose %>% select(-starts_with('sysBP.0.'))

# diabp------------------
#choose <- choose %>% add_column(diaBP = rep(NA,nrow(choose)))
choose$diaBP <- apply(ukb %>% select(starts_with('diaBP')),1,mean,na.rm=TRUE)
choose <- choose %>% select(-starts_with('diaBP.0.'))

print('done line 77')

# education------------------
levels <- levels(ukb_code[,'education.0.0'])
#levels

select <- apply(ukb %>% select(starts_with('education.0')),2,
                function(x) factor(x,levels=c(-7,-3,1,2,3,4,5,6),
                                   labels=levels))

select_dummy <- sjmisc::to_dummy(select,var.name='education',suffix = "label")

concat <- function(data,data_dummy) {
  dummy_sep <- rep(1:nrow(data),times=ncol(data))
  res = NULL
  for (i in 1:(nrow(data))) {
    res = rbind(res,colSums(data_dummy[dummy_sep==i,],na.rm=TRUE))
  }
  colnames(res) = colnames(data_dummy)
  return (res)
}

choose <- cbind(choose,concat(data=select, data_dummy=select_dummy))
choose <- choose %>% select(-starts_with('education.0.'))

print('done line 98')

# occupation------------------
levels <- levels(ukb_code[,'occupation.0.0'])
#levels

select <- apply(ukb %>% select(starts_with('occupation.0')),2,
                function(x) factor(x,levels=c(-7,-3,1,2,3,4,5,6,7),
                                   labels=levels))

select_dummy <- sjmisc::to_dummy(select,var.name='occupation',suffix = "label")

choose <- cbind(choose,concat(data=select, data_dummy=select_dummy))
choose <- choose %>% select(-starts_with('occupation.0.'))

print('done line 113')

# cardiovascular.0.0-0.3------------------
levels <- levels(ukb_code[,'cardiovascular.0.0'])
#levels

select <- apply(ukb %>% select(starts_with('cardiovascular.0')),2,
                function(x) factor(x,levels=c(-7,-3,1,2,3,4),
                                   labels=levels))

select_dummy <- sjmisc::to_dummy(select,var.name='cardiovascular',suffix = "label")


choose <- cbind(choose,concat(data=select, data_dummy=select_dummy))
choose <- choose %>% select(-starts_with('cardiovascular.0.'))

print('done line 129')

# clot_DVT_bronchitis_emphysema_asthma_rhinitis_eczema_allergy.0.0-0.4------------------
levels <- levels(ukb_code[,'clot_DVT_bronchitis_emphysema_asthma_rhinitis_eczema_allergy.0.0'])
#levels

select <- apply(ukb %>% select(starts_with('clot_DVT_bronchitis_emphysema_asthma_rhinitis_eczema_allergy')),2,
                function(x) factor(x,levels=c(-7,-3,5,6,7,8,9),
                                   labels=levels))

select_dummy <- sjmisc::to_dummy(select,var.name='comorbidity_field6152',suffix = "label")

choose <- cbind(choose,concat(data=select, data_dummy=select_dummy))
choose <- choose %>% select(-starts_with('clot_DVT_bronchitis_emphysema_asthma_rhinitis_eczema_allergy.0.'))

print('done line 144')

# medication_CHL_BP_DM_HOR.0.0-0.3------------------
levels <- levels(ukb_code[,'medication_CHL_BP_DM_HOR.0.0'])
#levels

select <- apply(ukb %>% select(starts_with('medication_CHL_BP_DM_HOR')),2,
                function(x) factor(x,levels=c(-7,-3,-1,1,2,3,4,5),
                                   labels=levels))

# select_code <- data.frame(lapply(ukb_code %>% select(starts_with('medication_CHL_BP_DM_HOR.0')), 
#                                 as.character), stringsAsFactors=FALSE)

select_dummy <- sjmisc::to_dummy(select,var.name='medication_field6153',suffix = "label")

choose <- cbind(choose,concat(data=select, data_dummy=select_dummy))

choose <- choose %>% select(-starts_with('medication_CHL_BP_DM_HOR.0.'))

print('done line 163')

# medication_pain_constipation_heartburn.0.0-0.5------------------
levels <- levels(ukb_code[,'medication_pain_constipation_heartburn.0.0'])
#levels

select <- apply(ukb %>% select(starts_with('medication_pain_constipation_heartburn')),2,
                function(x) factor(x,levels=c(-7,-3,-1,1,2,3,4,5,6),
                                   labels=levels))

# select_code <- data.frame(lapply(ukb_code %>% select(starts_with('medication_pain_constipation_heartburn.0')), 
#                                 as.character), stringsAsFactors=FALSE)

select_dummy <- sjmisc::to_dummy(select,var.name='medication_field6154',suffix = "label")

choose <- cbind(choose,concat(data=select, data_dummy=select_dummy))

choose <- choose %>% select(-starts_with('medication_pain_constipation_heartburn'))

print('done line 182')

# diet_vitamin_mineral_supplements.0.0-0.6------------------
levels <- levels(ukb_code[,'diet_vitamin_mineral_supplements.0.0'])
#levels

select <- apply(ukb %>% select(starts_with('diet_vitamin_mineral_supplements')),2,
                function(x) factor(x,levels=c(-7,-3,seq(1,7)),
                                   labels=levels))

#select_code <- data.frame(lapply(ukb_code %>% select(starts_with('diet_vitamin_mineral_supplements.0')), 
#                                 as.character), stringsAsFactors=FALSE)

select_dummy <- sjmisc::to_dummy(select,var.name='vitamin_6155',suffix = "label")

choose <- cbind(choose,concat(data=select, data_dummy=select_dummy))

choose <- choose %>% select(-starts_with('diet_vitamin_mineral_supplements'))

print('done line 201')

# pain_type_lastmonth.0.0-0.6------------------
levels <- levels(ukb_code[,'pain_type_lastmonth.0.0'])
#levels

select <- apply(ukb %>% select(starts_with('pain_type_lastmonth')),2,
                function(x) factor(x,levels=c(-7,-3,seq(1,8)),
                                   labels=levels))

#select_code <- data.frame(lapply(ukb_code %>% select(starts_with('pain_type_lastmonth.0')), 
#                                 as.character), stringsAsFactors=FALSE)

select_dummy <- sjmisc::to_dummy(select,var.name='pain_6159',suffix = "label")

choose <- cbind(choose,concat(data=select, data_dummy=select_dummy))

choose <- choose %>% select(-starts_with('pain_type_lastmonth'))

print('done line 220')

# leisure_activities.0.0-0.4------------------
levels <- levels(ukb_code[,'leisure_activities.0.0'])
#levels

select <- apply(ukb %>% select(starts_with('leisure_activities')),2,
                function(x) factor(x,levels=c(-7,-3,seq(1,5)),
                                   labels=levels))

#select_code <- data.frame(lapply(ukb_code %>% select(starts_with('leisure_activities.0')), 
#                                 as.character), stringsAsFactors=FALSE)

select_dummy <- sjmisc::to_dummy(select,var.name='leisure_6160',suffix = "label")

choose <- cbind(choose,concat(data=select, data_dummy=select_dummy))

choose <- choose %>% select(-starts_with('leisure_activities'))

print('done line 239')

# medication_CHL_BP_DM.0.0-0.2------------------
levels <- levels(ukb_code[,'medication_CHL_BP_DM.0.0'])
#levels

select <- apply(ukb %>% select(starts_with('medication_CHL_BP_DM.0')),2,
                function(x) factor(x,levels=c(-7,-3,-1,seq(1,3)),
                                   labels=levels))

#select_code <- data.frame(lapply(ukb_code %>% select(starts_with('medication_CHL_BP_DM.0')), 
#                                 as.character), stringsAsFactors=FALSE)

select_dummy <- sjmisc::to_dummy(select,var.name='medication_6177',suffix = "label")

choose <- cbind(choose,concat(data=select, data_dummy=select_dummy))

choose <- choose %>% select(-starts_with('medication_CHL_BP_DM.0'))

print('done line 258')

# diet_mineral_supplements.0.0-0.5------------------
levels <- levels(ukb_code[,'diet_mineral_supplements.0.0'])
#levels

select <- apply(ukb %>% select(starts_with('diet_mineral_supplements')),2,
                function(x) factor(x,levels=c(-7,-3,seq(1,6)),
                                   labels=levels))

#select_code <- data.frame(lapply(ukb_code %>% select(starts_with('diet_mineral_supplements')), 
#                                 as.character), stringsAsFactors=FALSE)

select_dummy <- sjmisc::to_dummy(select,var.name='diet_mineral_6719_',suffix = "label")

choose <- cbind(choose,concat(data=select, data_dummy=select_dummy))

choose <- choose %>% select(-starts_with('diet_mineral_supplements'))

print('done line 277')

# cancer_code_selfreport.0.0------------------
levels <- levels(ukb_code[,'cancer_code_selfreport.0.0'])
#levels

select <- apply(ukb %>% select(starts_with('cancer_code_selfreport')),2,
                function(x) factor(x,levels=c(rep(-1,times=7),1001:1012,1015:1048,1050:1053,1055,1056,
                                              1058:1068,1070:1082,1084:1088,99999),
                                   labels=levels))

#select_code <- data.frame(lapply(ukb_code %>% select(starts_with('cancer_code_selfreport')), 
#                                 as.character), stringsAsFactors=FALSE)

select_dummy <- sjmisc::to_dummy(select,var.name='cancer_20001',suffix = "label")

choose <- cbind(choose,concat(data=select, data_dummy=select_dummy))

choose <- choose %>% select(-starts_with('cancer_code_selfreport'))

print('done line 297')

# comorbidity_non_cancer_code.0.0-0.33------------------
levels <- levels(ukb_code[,'comorbidity_non_cancer_code.0.0'])
#levels

df <- as.data.frame(fread("Coding_tables/coding6.tsv"))

select <- apply(ukb %>% select(starts_with('comorbidity_non_cancer_code')),2,
                function(x) factor(x,levels=c(df[,1][1:237],df[,1][239:474]),
                                   labels=levels))

#select_code <- data.frame(lapply(ukb_code %>% select(starts_with('comorbidity_non_cancer_code')), 
#                                 as.character), stringsAsFactors=FALSE)

select_dummy <- sjmisc::to_dummy(select,var.name='comorbidity_20002',suffix = "label")

choose <- cbind(choose,concat(data=select, data_dummy=select_dummy))

choose <- choose %>% select(-starts_with('comorbidity_non_cancer_code'))

print('done line 318')

# treatment_code.0.0-0.47------------------

levels <- levels(ukb_code[,'treatment_code.0.0'])
#levels

df <- as.data.frame(fread("Coding_tables/coding4.tsv"))

select <- apply(ukb %>% select(starts_with('treatment_code')),2,
                function(x) factor(x,levels=c(df[,1][1:4393],df[,1][4395:length(df[,1])]),
                                   labels=levels))

#select_code <- data.frame(lapply(ukb_code %>% select(starts_with('treatment_code')), 
#                                 as.character), stringsAsFactors=FALSE)

select_dummy <- sjmisc::to_dummy(select,var.name='medication_20003',suffix = "label")

choose <- cbind(choose,concat(data=select, data_dummy=select_dummy))
choose <- choose %>% select(-starts_with('treatment_code'))

print('done line 342')

# operation_code.0.0-0.31------------------
# interest in 
# 1455	cholecystectomy/gall bladder removal
# 1195	renal/kidney transplant
# 1463	rectal or colon polypectomy
# 1519	colonoscopy/sigmoidoscopy
# 1613	ct colonoscopy
# 1463	rectal or colon polypectomy
# 1465	rectal/sigmoid resection

levels <- levels(ukb_code[,'operation_code.0.0'])
#levels

df <- as.data.frame(fread("Coding_tables/coding5.tsv"))

# interest <- c(1455,1195,1463,1519,1613,1463,1465)
  
select <- apply(ukb %>% select(starts_with('operation_code')),2,
                function(x) factor(x,levels=c(df[,1][1:268],df[,1][269]),
                                   labels=levels))

#select_code <- data.frame(lapply(ukb_code %>% select(starts_with('operation_code')), 
#                                as.character), stringsAsFactors=FALSE)

select_dummy <- sjmisc::to_dummy(select,var.name='operation_20004',suffix = "label")

choose <- cbind(choose,concat(data=select, data_dummy=select_dummy))
choose <- choose %>% select(-starts_with('operation_code'))

print('done line 373')

# cancer_diag_year.0.0-0.5------------------
# not interest now

choose <- choose %>% select(-starts_with('cancer_diag_year'))

# cancer_diag_age.0.0-0.5------------------
# not interest now

choose <- choose %>% select(-starts_with('cancer_diag_age'))

# illness_noncancer_year.0.0-0.33------------------
# not interest now

choose <- choose %>% select(-starts_with('illness_noncancer_year'))

# illness_noncancer_age.0.0-0.33------------------
# not interest now

choose <- choose %>% select(-starts_with('illness_noncancer_age'))

# operation_year.0.0-0.31------------------
# not interest now
choose <- choose %>% select(-starts_with('operation_year'))

# operation_age.0.0-0.31------------------
# not interest now

choose <- choose %>% select(-starts_with('operation_age'))

# father_ill.0.0------------------
# only interest in Bowel cancer , code = 4
levels <- levels(ukb_code[,'father_ill.0.0'])
#levels

df <- as.data.frame(fread("Coding_tables/coding1010.tsv"))

select <- apply(ukb %>% select(starts_with('father_ill')),2,
                function(x) factor(x,levels=df[,1],labels=levels))

# select_code <- data.frame(lapply(ukb_code %>% select(starts_with('father_ill')), 
#                                 as.character), stringsAsFactors=FALSE)

select_dummy <- sjmisc::to_dummy(select,var.name='family_history_father_20107',suffix = "label")

choose <- cbind(choose,concat(data=select, data_dummy=select_dummy))

choose <- choose %>% select(-starts_with('father_ill'))

# mother_ill.0.0------------------
# only interest in Bowel cancer , code = 4

# interest = c(seq(1,13))

select <- apply(ukb %>% select(starts_with('mother_ill')),2,
                function(x) factor(x,levels=df[,1],labels=levels))

# select_code <- data.frame(lapply(ukb_code %>% select(starts_with('mother_ill')), 
#                                                         as.character), stringsAsFactors=FALSE)
                                 
select_dummy <- sjmisc::to_dummy(select,var.name='family_history_mother_20110',suffix = "label")

choose <- cbind(choose,concat(data=select, data_dummy=select_dummy))

choose <- choose %>% select(-starts_with('mother_ill'))

# sibling_illness.0.0  ------------------
# only interest in Bowel cancer , code = 4

# interest = c(seq(1,13))

select <- apply(ukb %>% select(starts_with('sibling_illness')),2,
                function(x) factor(x,levels=df[,1],labels=levels))

# select_code <- data.frame(lapply(ukb_code %>% select(starts_with('sibling_illness')), 
#                                 as.character), stringsAsFactors=FALSE)

select_dummy <- sjmisc::to_dummy(select,var.name='family_history_sibling_20111',suffix = "label")

choose <- cbind(choose,concat(data=select, data_dummy=select_dummy))

choose <- choose %>% select(-starts_with('sibling_illness'))

print('done line 457')

# genotype_PCA.0.0   ------------------                                        
# not interested, drop all 
choose <- choose %>% select(-starts_with('genotype_PCA'))

# cormorbidity_ICD10.0.1 ------------------
# too much !!! leave !!!!

print('done line 466')

# cancer behaviour--------
#CRC_case <- CRC_data[CRC_data$CRC_case==1,]
#case_id <- CRC_case$eid 

#CRC_old_case <- CRC_old[CRC_old$CRC_case==1,]
#CRC_old_case <- CRC_old_case[CRC_old_case$eid %in% case_id,]

#CRC_instance <- CRC_old[case_id,'CRC_instance']
#CRC_data$CRC_instance = NA

#case_idx <- which(CRC_case$eid %in% CRC_data$eid)
#CRC_data[case_id,'CRC_instance'] <- CRC_instance

#cancer_behaviour <- ukb_code %>% select(starts_with('cancer_behaviour'))

#CRC_data$CRC_behaviour = NA
#for (i in case_idx) {
#  for (j in (1:17)) {
#    if (replace_na(CRC_data[i,'CRC_instance'],-1)==(j-1)) {CRC_data[i,'CRC_behaviour']=as.character(cancer_behaviour[i,j])}
#  }
#}

# cancer histology------

#cancer_histology <- ukb_code %>% select(starts_with('cancer_histology'))

#CRC_data$CRC_histology = NA
#for (i in case_idx) {
#  for (j in (1:17)) {
#    if (replace_na(CRC_data[i,'CRC_instance'],-1)==(j-1)) {CRC_data[i,'CRC_histology']=as.character(cancer_histology[i,j])}
#  }
#}

print('done line 501')

# 4. Combine variables ----------------------------------

choose <- as.data.frame(choose)

CRC_data <- cbind(CRC_data,choose)
  
t1=Sys.time()
print(t1-t0)

# rename wrong variable name

CRC_data <- CRC_data %>% select(-c('sex'))
CRC_data <- CRC_data %>% select(-c('date_recr'))
CRC_data <- CRC_data %>% select(-c('assessment_centre'))
CRC_data <- CRC_data %>% select(-c('CRC_screening_last'))
CRC_data <- CRC_data %>% select(-c('ethnicity'))
CRC_data <- CRC_data %>% select(-c('age.0.0'))

# 5. Export file ----------------------------------

saveRDS(CRC_data, "Outputs/CRC_case_data_linked.rds")


