# STABILITY SELECTION

rm(list=ls())
# Loading Data
setwd("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/PLS")
CRC_ordered <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_5_year_ordered_V3.rds')

# Loading packages
suppressPackageStartupMessages(library(sgPLS))
library(pheatmap)
library(utils)
library(pROC)
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

X_test = as.data.frame(apply(test[,11:ncol(train)],2,as.numeric))
Y_test = test$cc_status

print(all(rownames(X_train)==rownames(Y_train)))

# stability selection --------
library(focus)
library(igraph)
library(glmnet)
library(pheatmap)

Xgroups = c(23, 5, 23, 41, 14, 18, 25, 54)

# VariableSelection(xdata = X_train, ydata = Y_train, verbose = FALSE,
# implementation= SparseGroupPLS,
# group_x = Xgroups, 
# LambdaX = 1:8,
# alpha.x = 0.9,
# n_cores = number_cores-1,
# K=1000, tau=0.5,
# group_penalisation = TRUE,
# resampling = "subsampling",
# family = "binomial")

stab_sgPLS = BiSelection(xdata = X_train, ydata = Y_train, verbose = FALSE,
                         implementation= SparseGroupPLS,
                         group_x = Xgroups, 
                         LambdaX = 1:8,
                         AlphaX = seq(0.1, 0.9, by = 0.1),
                         n_cores = number_cores-1,
                         K=1000, tau=0.5,
                         group_penalisation = TRUE,
                         resampling = "subsampling",
                         family = "binomial")

saveRDS(stab_sgPLS,'Outputs/stab_selection_sgPLS.rds')

pdf('Outputs/stab_selection_sgPLS.pdf')
CalibrationPlot(stab_sgPLS)
dev.off()

summary(stab_sgPLS)

selprop = SelectionProportions(stab_sgPLS)
saveRDS(selprop,'Outputs/stab_selection_sgPLS_selprop.rds')

hat_params=Argmax(stab_sgPLS)
hat_params

pdf('Outputs/stab_selection_sgPLS_selprop_plot.pdf')
par(mar=c(14,5,1,1))
plot(selprop, type="h", lwd=3, las=1, xlab="", ylab="Selection Proportion", xaxt="n",
     col=ifelse(selprop>=hat_params[2], yes="red", no="grey"), cex.lab=1.5)
abline(h=hat_params[2], lty=2, col="darkred")
for (i in 1:length(selprop)){
  axis(side=1, at=i, labels=names(selprop)[i], cex.axis=0.6,las=2, 
       col=ifelse(selprop[i]>=hat_params[2], yes="red", no="grey"),
       col.axis=ifelse(selprop[i]>=hat_params[2], yes="red", no="grey"))
}
dev.off()
