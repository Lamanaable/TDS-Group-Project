# SVM MODLE

# Date:  21 Feb 2022
# Authors: TDS Group 2 (Wei)
# Description : SVM preprocessing

# 1. Setup -------------------------------------------------------------------

# Package used 
rm(list=ls())
library(tidyverse)

# 2. Reading the data --------------------------------------------------------
setwd("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/SVM/Sens_colon")
data <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Sensitivity_Data/CRC_colon_only.rds')

# 2. Reading the data --------------------------------------------------------

# View(data)

table(data$tt_status)
table(data$cc_status,data$tt_status)

cc_status<-data$cc_status
tt_status<-data$tt_status

# Confirmation of columns to remove
colnames(data)[c(1:7,10)]

# Removing CRC related variables
data_prep<-data[,-c(1:7,10)]
#View(data_prep)

# Test and train split
train<-data_prep %>% filter(tt_status=="train")
test<-data_prep %>% filter(tt_status=="test")

# Remove test and train now variable now

train<-subset(train,select=-c(tt_status))
test<-subset(test,select=-c(tt_status))

# Scaling numeric variables
train_mask <- train %>% select_if(is.numeric) 
train_means <- data.frame(as.list(train_mask %>% apply(2, mean)))
train_stddevs <- data.frame(as.list(train_mask %>% apply(2, sd)))

col_names <- names(train_mask)

for (i in 1:ncol(train_mask)){
  print(i)
  train[,col_names[i]] <- (train[,col_names[i]]-train_means[,col_names[i]])/train_stddevs[,col_names[i]]
  print(i)
  test[,col_names[i]] <- (test[,col_names[i]]-train_means[,col_names[i]])/train_stddevs[,col_names[i]]
}

# Converting everything else to numeric

train<-apply(train, FUN = as.numeric, MARGIN = 2)
test<-apply(test, MARGIN=2, FUN=as.numeric)

complete_index <- as.vector(complete.cases(train))
table(complete_index) # Nothing missing!

complete_index_test <- as.vector(complete.cases(test))
table(complete_index_test) # Nothing missing!

#Checking which columns are causing issues
#names(which(colSums(is.na(train)) > 0))


# Check everything is numeric
table(sapply(train, class))
table(sapply(test, class))

# Exporting test and train data as csv and RDS

write.csv(train, "train_5_year.csv")
saveRDS(train, "train_5_year.rds")

write.csv(test, "test_5_year.csv")
saveRDS(test, "test_5_year.rds")

