## DESCRIPTIVE ANALYSIS

# Date: 
# Authors: TDS Group 2 
# Description: descriptive analysis and table 1
# output:

# 1. Setup -------------------------------------------------------------------

# Package used for date conversion:
library(tidyverse)

# 2. Reading the data --------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
CRC_path <- as.character(args[1])

CRC_data <- readRDS(CRC_path)

