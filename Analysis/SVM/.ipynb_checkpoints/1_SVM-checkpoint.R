# SVM MODLE

# Date:  21 Feb 2022
# Authors: TDS Group 2 (Wei)
# Description : SVM preprocessing

# 1. Setup -------------------------------------------------------------------

# Package used 
# path = '/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/SVM'
# setwd(path)
library(tidyverse)

# 2. Reading the data --------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
CRC_path <- as.character(args[1])
CRC_data <- readRDS(CRC_path)

# testing
# CRC_data <- CRC_data_grouped
train = CRC_data[CRC_data$tt_status=='train',]
test = CRC_data[CRC_data$tt_status=='test',]

Train_mask <- train[,14:ncol(train)] %>% select_if(is.numeric)
Train_means <- data.frame(as.list(Train_mask %>% apply(2, mean))) 
Train_stddevs <- data.frame(as.list(Train_mask %>% apply(2,sd)))

col_names <- names(Train_mask)
for(i in 1:ncol(Train_mask)){ 
  train[,col_names[i]] <- (train[,col_names[i]]-Train_means[,col_names[i]])/Train_stddevs[,col_names[i]]
  test[,col_names[i]] <- (test[,col_names[i]]-Train_means[,col_names[i]])/Train_stddevs[,col_names[i]]
}

saveRDS(train, "train.rds")
write.csv(train, "train.csv")

saveRDS(test, "test.rds")
write.csv(test, "test.csv")
