### LASSO USING STABILITY SELECTION (FOCUS PACKAGE)

# Note that the focus tar file is required in the same folder for this to run

# Author: Ellie


rm(list=ls())


# Loading data ------------------------------------------------------------

print("Reading data")
train <- readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/Stability_selection/5_year/Data/CRC_rectum_train_standardised.rds")
test <- readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/Penalised_regression/Stability_selection/5_year/Data/CRC_rectum_test_standardised.rds")

# Using comp epi example

# covars = readRDS("Data/Covariates_selected_proteins.rds")
# proteins = readRDS("Data/Proteins_selected_denoised_re.rds")
# transcripts = readRDS("Data/Transcripts_log_transformed.rds")
# ids = intersect(rownames(proteins), rownames(transcripts))
# covars = covars[ids, ]
# transcripts = transcripts[ids, ]
# proteins = proteins[ids, ]
# print(all(rownames(proteins) == rownames(transcripts)))
# print(all(rownames(proteins) == rownames(covars)))
# 
# ids_to_exclude = unique(c(which(is.na(covars$packyears)),
#                           which(is.na(covars$age.sample)), which(is.na(covars$bmi))))
# if (length(ids_to_exclude) > 0) {
#   covars = covars[-ids_to_exclude, ]
# }
# proteins = proteins[rownames(covars), ]
# y = covars$packyears
# x = cbind(proteins, age = covars$age.sample, bmi = covars$bmi)

# Loading packages --------------------------------------------------------

# print("Installing focus")
#library(devtools)
# untar("focus_1.0.1.tar.gz")
#install("focus", upgrade = "always")

print("Loading focus package")
library(focus)

print("Loading packages")
library(pheatmap)
#library(utils)
#library(pROC)
library(doParallel)
library(parallel)
library(tidyverse)
#library(focus)
#library(igraph)
library(glmnet)




# Setting up number of cores ----------------------------------------------

print("Number of cores")
number_cores<-detectCores()
number_cores
registerDoParallel(number_cores-1)


# X and Y / Train and test split ----------------------------------------------
print("Test train split")
Y_train = train$cc_status
X_train = as.data.frame(train[-1])

Y_test = test$cc_status
X_test = as.data.frame(test[-1])

table(sapply(X_train, class)) # there is one factor which is cc_status
table(sapply(Y_train, class))

print(all(rownames(X_train)==rownames(Y_train)))

X_train<-as.matrix(X_train)
#Y_train<-as.matrix(Y_train)

table(sapply(X_train, class)) # there is one factor which is cc_status
table(sapply(Y_train, class))

# Stability selection -----------------------------------------------------


# Trying stability selection under lasso ----------------------------------

# Changing tau to 0.5 to reduce the number of variables selected

print("Stability selection under lasso")
t0=Sys.time()
stab_lasso = VariableSelection(xdata = X_train, ydata = Y_train, verbose = TRUE,
                               implementation= PenalisedRegression,
                               n_cores = number_cores-1,
                               k=1000,
                               tau=0.5,
                               resampling = "subsampling",
                               family = "binomial")
t1=Sys.time()
print(t1-t0)


# Saving lasso plots and outputs ------------------------------------------


print("Saving stability selection")
saveRDS(stab_lasso,'Outputs_rectum/rectum_stab_selection_lasso.rds')

print("Calibration plot")
pdf('Outputs_rectum/rectum_stab_selection_lasso.pdf')
CalibrationPlot(stab_lasso)
dev.off()


print("Stability selection summary")
summary(stab_lasso)
selprop = SelectionProportions(stab_lasso)
saveRDS(selprop,'Outputs_rectum/rectum_stab_selection_lasso_selprop.rds')
hat_params=Argmax(stab_lasso)
hat_params

print("Stability selection plot")
pdf('Outputs_rectum/rectum_stab_selection_lasso_selprop_plot.pdf')
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

print("done")




