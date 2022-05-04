# Load Data
vars = read.csv('/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_explorationVariable_definition/vars_lookup.csv')
vars$Oldname <- as.character(vars$Oldname)
vars$Newname <- as.character(vars$Newname)

# Rename columns of the dataset
df = CRC_ordered # can change dataset to which you want to change the names

library(tidyverse)
names(df) <- vars[vars$Oldname %in% names(df),]$Newname

# remove underscores
names(df) <- gsub(x = names(df), pattern = "_", replacement = " ")

# see what has been changed
names(df)
