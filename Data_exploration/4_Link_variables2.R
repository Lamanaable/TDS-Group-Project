## LINK VARIABLES

# Date: 20 Feb 2022
# Authors: TDS Group 2 (Wei)
# Description: Link  
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
CRC_final <- as.character(args[3])

ukb_extracted <- readRDS(ukb_new_path)
ukb_code <- readRDS(ukb_recoded_path)
CRC_final <- readRDS(CRC_final)

t0=Sys.time()

# CRC_final <- CRC_data_final_V2
# ukb_extracted <- ukb_extracted_new_2
# ukb_code <- ukb_final

ukb_extracted$eid <- rownames(ukb_extracted)
ukb_code$eid <- rownames(ukb_code)

ukb <- ukb_extracted[ukb_extracted$eid %in% CRC_final$eid,]
rownames(ukb) <- ukb$eid

ukb_code <- ukb_code[ukb_code$eid %in% CRC_final$eid,]
rownames(ukb_code) <- ukb_code$eid

# ukb <- ukb[1:100,] 
# ukb_code <- ukb_code[1:100,]
# CRC_final <- CRC_final[1:100,]

print('start')
t1=Sys.time()
# 3. Link variable ----------------------------------

# cormorbidity_ICD10.0.1
concat <- function(data,data_dummy) {
  dummy_sep <- rep(1:nrow(data),times=ncol(data))
  res = NULL
  for (i in 1:(nrow(data))) {
    res = rbind(res,colSums(data_dummy[dummy_sep==i,],na.rm=TRUE))
  }
  colnames(res) = colnames(data_dummy)
  return (res)
}

levels <- levels(ukb_code[,'cormorbidity_ICD10.0.0'])
#levels

df <- as.data.frame(fread("Coding_tables/coding1910.tsv"))

# select = apply(ukb %>% select(starts_with('cormorbidity_ICD10')),2,function(x) as.factor(x))

select <- apply(ukb %>% dplyr::select(starts_with('cormorbidity_ICD10.')),2,
                function(x) factor(x,levels=c(df[,1][1:1053],df[,1][1055:length(df[,1])]),
                                  labels=levels))

select_date <- ukb %>% dplyr::select(starts_with('cormorbidity_ICD10_date'))

compare_date =NULL
for (i in (1:ncol(select_date))){
  x=as.Date(select_date[,i]) - as.Date(CRC_final$date_recr)
  compare_date = cbind(compare_date,x)
  colnames(compare_date)[i] = colnames(select)[i]
}
rownames(compare_date) = rownames(select)

date <- replace_na(compare_date <= 0,FALSE)

compare_select = replace(select, !date, NA)

compare_select <- compare_select[,colSums(!is.na(compare_select)) >0]
#keep_idx = colSums((!is.na(compare_select))/nrow(compare_select)) > 0.01
#names(which(keep_idx))
#compare_select <- compare_select[,names(which(keep_idx))]

select_dummy <- sjmisc::to_dummy(compare_select,var.name='cormorbidity_ICD10',suffix = "label")

choose = NULL
choose <- concat(data=compare_select, data_dummy=select_dummy)

# 4. Combine variables ----------------------------------

choose <- as.data.frame(choose)

CRC_final <- cbind(CRC_final,choose)
  
t1=Sys.time()
print(t1-t0)


# 5. Export file ----------------------------------

saveRDS(CRC_final, "Outputs/CRC_data_comorbidity_5_year.rds")


