# PLS ANALYSIS

# 1. Setup -------------------------------------------------------------------

# Package used for date conversion:
rm(list=ls())
library(tidyverse)

# 2. Reading the data --------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
CRC_path <- as.character(args[1])
CRC_ordered <- readRDS(CRC_path)
setwd("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/PLS")
CRC_ordered <- readRDS('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_5_year_ordered_V3.rds')

# 3. Analysis ------------------------------------------
suppressPackageStartupMessages(library(sgPLS))
suppressPackageStartupMessages(library(pheatmap))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(utils))
source("Scripts/pls_functions.R")

train = CRC_ordered[CRC_ordered$tt_status=='train',]

X_pooled=apply(train[,11:ncol(train)],2,as.numeric)
Y_pooled=train$cc_status

# Non-penalised PLS-DA models can be run using the plsda()

MyPLSDA_pooled <- plsda(X_pooled, Y_pooled, scale = FALSE,ncomp = 1)

MyPLSDA_pooled$loadings$X
MyPLSDA_pooled$loadings$Y
MyPLSDA_pooled$explained_variance

# sPLS ---------------

MysPLSDA_pooled <- splsda(X_pooled, Y_pooled, ncomp=1)

MysPLSDA_pooled$loadings$X

# selected variables
print(MysPLSDA_pooled$loadings$X[MysPLSDA_pooled$loadings$X!=0,])
betas = MysPLSDA_pooled$loadings$X[MysPLSDA_pooled$loadings$X!=0,]
names = names(betas)
pdf("Outputs/sPLS.pdf")
par(mar = c(10, 4.5, 6, 3))
plot(betas, type = 'h', col='navy', lwd=3, xaxt='n', xlab='', ylab=expression(beta))
#axis(side = 1, at = 1:length(betas), labels = names, las=2,cex.axis=0.8, tck=-.01)
text(1:length(betas), par("usr")[3]-0.05, 
     srt = 60, adj = 1, xpd = TRUE,
     labels = names, cex = 0.8)
abline(h=0, lty=2)
dev.off()


# gPLS ----------
### Loading and preparing the data

print(all(rownames(X_pooled)==rownames(Y_pooled)))

Xgroups=c(23, 28, 51, 92, 106, 124, 149)

MygPLSDA_pooled <- gPLSda(X_pooled, Y_pooled, ncomp = 1,
                          ind.block.x = Xgroups, keepX = 3)

MygPLSDA_pooled$loadings$X

# Claibration -------
# sPLS -----------

set.seed(1)
res_splsda = CalibratesPLSDA(dataX = X_pooled, dataY = Y_pooled,
                             ncomp = 1, Nrepeat = 10)
pdf("Outputs/sPLS_calib.pdf")
PlotCalib(res = res_splsda)
dev.off()

# sgPLS ----------

set.seed(1)
res_sgplsda = CalibratesgPLSDA(dataX = X_pooled, dataY = Y_pooled,
                               ncomp = 1, Nrepeat = 10, Xgroups = Xgroups)
pdf("Outputs/sgPLS_calib.pdf")
PlotCalib(res = res_sgplsda, type = "sgPLSDA")
dev.off()
