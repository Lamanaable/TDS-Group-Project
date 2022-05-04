# PLS ANALYSIS-SENSITIVITY

# 1. Setup -------------------------------------------------------------------

# Package used for date conversion:
rm(list=ls())
library(tidyverse)

# 2. Reading the data --------------------------------------------------------
setwd("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/PLS/Sensitivity_Analysis/Rectal_only")
CRC_ordered <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration//Sensitivity_Data/CRC_rectum_only.rds')

# 3. Analysis ------------------------------------------

# Loading packages
suppressPackageStartupMessages(library(sgPLS))
library(pheatmap)
library(utils)
library(doParallel)
library(parallel)
library(tidyverse)
print("Number of cores")
number_cores<-detectCores()
number_cores
registerDoParallel(number_cores-1)

# train/test data loading
train = CRC_ordered[CRC_ordered$tt_status=='train',]
test = CRC_ordered[CRC_ordered$tt_status=='test',]

X_train = as.data.frame(apply(train[,11:ncol(train)],2,as.numeric))
Y_train = train$cc_status
qr(X_train)$rank

X_test = as.data.frame(apply(test[,11:ncol(train)],2,as.numeric))
Y_test = test$cc_status

print(all(rownames(X_train)==rownames(Y_train)))

# stability selection --------
library(focus)
library(igraph)

Xgroups = c(23, 5, 23, 41, 14, 18, 25, 54)

stab_sgPLS = BiSelection(xdata = X_train, ydata = Y_train, verbose = FALSE,
                         implementation= SparseGroupPLS,
                         group_x = Xgroups, 
                         LambdaX = 1:8,
                         AlphaX = seq(0.1, 0.9, by = 0.1),
                         n_cores = number_cores-1,
                         K=1000, tau=0.8,
                         group_penalisation = TRUE,
                         resampling = "subsampling",
                         family = "binomial")

saveRDS(stab_sgPLS,'../Outputs/stab_selection_rectum.rds')

pdf('../Outputs/stab_selection_rectum.pdf')
CalibrationPlot(stab_sgPLS)
dev.off()

summary(stab_sgPLS)

selprop = SelectionProportions(stab_sgPLS)
saveRDS(selprop,'../Outputs/stab_selection_rectum_selprop.rds')

hat_params=Argmax(stab_sgPLS)
hat_params

pdf('../Outputs/stab_selection_rectum_selprop_plot.pdf')
plot(stab_sgPLS)
dev.off()
