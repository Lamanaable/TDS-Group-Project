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

# Fitting PLS models---------------------
suppressPackageStartupMessages(library(sgPLS))
suppressPackageStartupMessages(library(pheatmap))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(utils))
source("Scripts/pls_functions.R")
library(doParallel)
library(parallel)

print("Number of cores")
number_cores<-detectCores()
number_cores
registerDoParallel(number_cores-1)
source("Scripts/pls_functions.R")

Xgroups=c(23, 28, 51, 92, 106, 124, 149)

train = CRC_ordered[CRC_ordered$tt_status=='train',]
test = CRC_ordered[CRC_ordered$tt_status=='test',]

X_pooled = apply(train[,11:ncol(train)],2,as.numeric)
Y_pooled = train$cc_status

X_test = apply(test[,11:ncol(train)],2,as.numeric)
Y_test = test$cc_status

print(all(rownames(X_pooled)==rownames(Y_pooled)))

Y_diagnostic = train$CRC_dvlp_year[train$cc_status == 1] ## time to diagnostic in days
X_diagnostic = X_pooled[train$cc_status == 1,]
names(Y_diagnostic) = rownames(X_diagnostic)

### Pooled cases 

MyPLSDA_pooled <- plsda(X_pooled, Y_pooled, ncomp=1)
MysPLSDA_pooled <- splsda(X_pooled, Y_pooled, keepX = 7, ncomp=1) 
MysgPLSDA_pooled <- sgPLSda(X_pooled, Y_pooled, ncomp = 1, ind.block.x = Xgroups, keepX = 2, alpha.x = 0.9)

# Visualising the loadings coefficients----------------
### Loadings plot

MysPLSDA_pooled$explained_variance

Loadings=cbind(-MysPLSDA_pooled$loadings$X, -MysgPLSDA_pooled$loadings$X, rep(NA, 203), rep(NA, 203))
Loadings=as.vector(t(Loadings))
Loadings=Loadings[-c(length(Loadings)-1, length(Loadings))]

MyPal=brewer.pal("Paired", n = 10)

pdf("Outputs/sPLS_sgPLS.pdf", height=6, width=10)
par(mar=c(14,5,3,3))
plot(Loadings, col=alpha(c(MyPal[10], MyPal[2], NA, NA),0.7), xaxt="n", ylab="Loadings Coefficients",  type="h", lwd=3, xlab="")
axis(1, at=seq(1.5, 203*4, by=4), labels = colnames(MyPLSDA_pooled$X), las=2, cex.axis=0.5)
axis(1, at=c(0, Xgroups, 203)*4, line = 12, labels = NA)
axis(1, at=c(12, 25.5, 39.5, 71.5, 99, 115, 136.5, 176)*4, 
     labels = c("Base","Phys","SocioDemo","Lifestyle","FamilyHx","Biomarker","Meds/Ops","Comorb"), 
     line=11.5, cex.axis=0.6,tick = FALSE)
abline(v=c(0, Xgroups, 203)*4, lty=3, col="black")
abline(h = 0, lty = 2)
legend("bottomright", legend = c("sPLS-DA", "sgPLS-DA"), lty=1, lwd=3,
       col=c(MyPal[10], MyPal[2]), cex=0.75)
dev.off()

# simplified version
Loadings=cbind(-MysPLSDA_pooled$loadings$X, -MysgPLSDA_pooled$loadings$X)
df = as.data.frame(replace(Loadings,!(Loadings!=0), NA))
colnames(df) = c('sPLSDA','sgPLSDA')
df$na = rowSums(!is.na(df))
Loadings_ = df[df$na >0 ,]
saveRDS(Loadings_,'Outputs/sPLSDA_sgPLSDA_results.rds')
Loadings=as.matrix(cbind(Loadings_[,1:2], rep(NA, nrow(Loadings_ )), rep(NA, nrow(Loadings_ ))))
Loadings=as.vector(t(Loadings))
Loadings=Loadings[-c(length(Loadings)-1, length(Loadings))]

Xgroups_ = c(1,4,9)

pdf("Outputs/sPLS_sgPLS_simple.pdf", height=6, width=10)
par(mar=c(14,5,3,3))
plot(Loadings, col=c(MyPal[10], MyPal[2], NA, NA), xaxt="n", ylab="Loadings Coefficients",  type="h", lwd=3, xlab="")
axis(1, at=seq(1.5, 10*4, by=4), labels = rownames(Loadings_), las=2, cex.axis=0.7)
axis(1, at=c(0, Xgroups_,9.5)*4, line = 10, labels = NA)
axis(1, at=c(0.5, 2.5, 6.5, 9.5)*4, 
     labels = c("Baseline","Physical Measurement","Lifestyle","Meds/Ops"), 
     line=10, cex.axis=0.6,tick = FALSE)
abline(v=c(0, Xgroups_,9.5)*4, lty=3, col="black")
abline(h = 0, lty = 2)
legend("topright", legend = c("sPLS-DA", "sgPLS-DA"), lty=1, lwd=3,
       col=c(MyPal[10], MyPal[2]), cex=0.75)
dev.off()


### Compute scores from loadings:

S_X_pooled=X_pooled%*%MyPLSDA_pooled$loadings$X

### Misclassification rates plot

MyPredict=predict(MysPLSDA_pooled, newdata = X_pooled)
fitted=MyPredict$class$max.dist
table(fitted)

MSEP_sPLSDA=NULL
for(subtype in c(0,1)){
  idx=which(Y_pooled==subtype)
  type = ifelse(subtype==1,'case','control')
  MSEP_sPLSDA[[type]]=1-sum(diag(table(Y_pooled[idx], MyPredict$class$max.dist[idx])))/length(idx)
}

MyPredict=predict(MysgPLSDA_pooled, newdata = X_pooled)

MSEP_sgPLSDA=NULL
for(subtype in c(0,1)){
  idx=which(Y_pooled==subtype)
  type = ifelse(subtype==1,'case','control')
  MSEP_sgPLSDA[[type]]=1-sum(diag(table(Y_pooled[idx], MyPredict$class$max.dist[idx])))/length(idx)
}

MSEP=cbind(MSEP_sPLSDA, MSEP_sgPLSDA)

rownames(MSEP)=c("Controls", "Cases")

MSEP=cbind(MSEP, rep(NA, 2), rep(NA, 2))
MSEP=unlist(t(MSEP)[1:(nrow(t(MSEP))*ncol(t(MSEP)))])
#MSEP=as.vector(t(MSEP))
MSEP=MSEP[-c(length(MSEP)-1, length(MSEP))]

pdf("Outputs/Misclassificaiton_Rate.pdf", height = 5, width=9)
par(mar=c(6,12,5,12))
plot(MSEP, type = "h", lwd=3, xaxt="n", xlab="", ylab="Misclassification Rates", 
     col=c(MyPal[2], MyPal[10], NA, NA),
     ylim=c(0, max(MSEP[!is.na(MSEP)])), las=1)
axis(1, at=c(1.5, 5.5), labels = c("Controls", "Cases"))
dev.off()


### Stability analysis sPLS ----------

Stability_sPLS=StabilityPlot(X = X_pooled, Y = Y_pooled, NIter = 100)
saveRDS(Stability_sPLS, "Outputs/stability_analysis_sPLS.rds")

# visual
stab = sort(colSums(Stability_sPLS),decreasing =TRUE)

# cut-off top 80%
stab = stab/(dim(Stability_sPLS)[1])
saveRDS(stab,'Outputs/stability_analysis_sPLS_sum.rds')
stab = stab *100
stab80 = stab[stab>80]

pdf("Outputs/Stability_Analysis_sPLS.pdf", height = 6, width=8)
par(mar=c(13.5,5,2,4))
plot(stab80, type = "h", col = "navy",
     lwd = 3, xaxt = "n", xlab = "", ylab = 'percentage selected (%)',
     ylim = c(0, 100), las = 1)
title(main='sPLS Stability Analysis')
axis(1, at=seq(1,length(stab80)), labels = names(stab80), las=2, cex.axis=0.7)
dev.off()

### Stability analysis sgPLS ----------

Stability_sgPLS=StabilityPlot_sgPLS(X = X_pooled, Y = Y_pooled, Xgroups=Xgroups, NGroup=2, NIter = 100)
saveRDS(Stability_sgPLS, "Outputs/stability_analysis_sgPLS.rds")

# visual
stab = sort(colSums(Stability_sgPLS),decreasing =TRUE)

# cut-off 50%
stab = stab/(dim(Stability_sgPLS)[1])
saveRDS(stab,'Outputs/stability_analysis_sgPLS_sum.rds')
stab = stab *100
stab50 = stab[stab>50]

pdf("Outputs/Stability_Analysis_sgPLS.pdf", height = 6, width=8)
par(mar=c(12,5,2,4))
plot(stab50, type = "h", col = "navy",
     lwd = 3, xaxt = "n", xlab = "", ylab = 'percentage selected (%)',
     ylim = c(0, 100), las = 1)
title(main='sgPLS Stability Analysis')
axis(1, at=seq(1,length(stab50)), labels = names(stab50), las=2, cex.axis=0.7)
dev.off()




