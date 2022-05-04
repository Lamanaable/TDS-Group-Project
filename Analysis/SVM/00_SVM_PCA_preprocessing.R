# SVM PCA preprocessing

# Date:  12 March 2022
# Authors: TDS Group 2 (Wei)

# 1. Setup -------------------------------------------------------------------

# Package used 
path = '/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/SVM'
setwd(path)
data = readRDS("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_5_year_ordered_V3.rds")
library(tidyverse)

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

train<-data_prep %>% filter(tt_status=='train')
test<-data_prep %>% filter(tt_status=='test')

train <- train %>% mutate_if(is.factor,function(x) (as.numeric(x)-1))
test <- test %>% mutate_if(is.factor,function(x) (as.numeric(x)-1))

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

train[2:ncol(train)] <- apply(train[2:ncol(train)], FUN = as.numeric, MARGIN = 2)
test[2:ncol(test)] <- apply(test[2:ncol(test)], MARGIN=2, FUN=as.numeric)

Baseline <- train[,2:24]
Physical <- train[,25:29]
Sociodemo <- train[,30:52]
Lifestyle <- train[,53:93]
FamilyHx <- train[,94:107]
Biomarker <- train[,108:125]
Meds <- train[,126:150]
Comorb <- train[,151:204]

Baseline_test <- test[,2:24]
Physical_test <- test[,25:29]
Sociodemo_test <- test[,30:52]
Lifestyle_test <- test[,53:93]
FamilyHx_test <- test[,94:107]
Biomarker_test <- test[,108:125]
Meds_test <- test[,126:150]
Comorb_test <- test[,151:204]

group = c('Baseline','Physical','Sociodemo','Lifestyle','FamilyHx',
          'Biomarker','Meds','Comorb')

# PCA ----

res = data[data$tt_status=='train',]$cc_status
res_test = data[data$tt_status=='test',]$cc_status

ev_list = NULL
cum_ev_list = NULL
library(abind)
for (name in group) {
mypca <- prcomp(eval(parse(text=name)),scale=TRUE)
out=summary(mypca)
ev=out$importance[2,]
names(ev) <- paste0(name,'_',names(ev))
ev_list <- abind(ev_list,ev,along=1)
cum_ev <- out$importance[3,]
names(cum_ev) <- paste0(name,'_',names(cum_ev))
cum_ev_list <- abind(cum_ev_list,cum_ev,along=1)

axes <- predict(mypca, newdata = eval(parse(text=name)))
axes <- axes[,1:(sum(cum_ev<0.80)+1)]
axes = as.data.frame(axes)

axes_test <- predict(mypca, newdata = eval(parse(text=paste0(name,'_test'))))
axes_test <- axes_test[,1:(sum(cum_ev<0.80)+1)]
axes_test = as.data.frame(axes_test)
names(axes) = names(axes_test) = paste0(name,'_',names(axes))

res = cbind(res,axes)
res_test = cbind(res_test,axes_test)
}

res = as.data.frame(res)
res <- res %>% rename(cc_status=res)
res_test = as.data.frame(res_test)
res_test  <- res_test  %>% rename(cc_status=res_test)

cum_ev_list <- as.data.frame(cum_ev_list)
cum_ev_list$feature <- rownames(cum_ev_list)

ev_list <- as.data.frame(ev_list)
ev_list$feature <- rownames(ev_list)

saveRDS(ev_list,'Results_PCA_SVM/ev_list.rds')
saveRDS(cum_ev_list,'Results_PCA_SVM/cum_ev_list.rds')

# Test and train split --------
train_pca <- res 
test_pca <- res_test

# Check everything is numeric
table(sapply(train_pca, class))
table(sapply(test_pca, class))

write.csv(train_pca, "pca_train_5_year.csv")
saveRDS(train_pca, "pca_train_5_year.rds")

write.csv(test_pca, "pca_test_5_year.csv")
saveRDS(test_pca, "pca_test_5_year.rds")

# # Correlation circle / variable plot

head(Biomarker)

pdf('Results_PCA_SVM/SVM_PCA_Biomarker.pdf',height=10,width=10)
par(mfrow=c(1,1),mar=c(5,5,5,5))
mycor=cor(as.matrix(Biomarker), as.matrix(res[,64:65]))
include <- ifelse((abs(mycor[,1])-0.4)<0 & (abs(mycor[,2])-0.4)<0,FALSE,TRUE)
name <- colnames(Biomarker)
name[!include] <-' '

plot(mycor[,1:2], xlim=c(-1.2,1.2), ylim=c(-1.2,1.2), cex=0.1, pch=19, las=1, cex.lab=1.5,
     xlab=substitute(PC[1]*" ("*a*"% e.v.)", list(a=13.8)),
     ylab=substitute(PC[2]*" ("*a*"% e.v.)", list(a=11.7)),
     main = 'PCA Biomarker on train set')
arrows(x0=rep(0, nrow(mycor)), y0=rep(0, nrow(mycor)),
       x1=mycor[,1], y1=mycor[,2], length=0.1, col="navy")
abline(h=0, lty=2)
abline(v=0, lty=2)
xseq=seq(-1,1,length.out=10000)
lines(xseq, sqrt(1-xseq^2))
lines(xseq, -sqrt(1-xseq^2))
text(mycor[,1]+sign(mycor[,1])*0.25, mycor[,2]+0.1, labels=name, cex=0.8, col="navy")
dev.off()

pdf('Results_PCA_SVM/SVM_PCA_Comorbidity.pdf',height=10,width=10)
par(mfrow=c(1,1),mar=c(5,5,5,5))
mycor=cor(as.matrix(Comorb), as.matrix(res[,92:93]))
include <- ifelse((abs(mycor[,1])-0.4)<0 & (abs(mycor[,2])-0.4)<0,FALSE,TRUE)
name <- colnames(Comorb)
name[!include] <-' '

plot(mycor[,1:2], xlim=c(-1.2,1.2), ylim=c(-1.2,1.2), cex=0.1, pch=19, las=1, cex.lab=1.5,
     xlab=substitute(PC[1]*" ("*a*"% e.v.)", list(a=9.6)),
     ylab=substitute(PC[2]*" ("*a*"% e.v.)", list(a=4.5)),
     main = 'PCA Comorbidity on train set')
arrows(x0=rep(0, nrow(mycor)), y0=rep(0, nrow(mycor)),
       x1=mycor[,1], y1=mycor[,2], length=0.1, col="navy")
abline(h=0, lty=2)
abline(v=0, lty=2)
xseq=seq(-1,1,length.out=10000)
lines(xseq, sqrt(1-xseq^2))
lines(xseq, -sqrt(1-xseq^2))
text(mycor[,1]+sign(mycor[,1])*0.25, mycor[,2]+0.1, labels=name, cex=0.8, col="navy")
dev.off()

# score plot for Comorb
mypca <- prcomp(Comorb,scale=TRUE)
pdf('Results_PCA_SVM/SVM_PCA_Comorbidity_score_plot.pdf',height=10,width=10)
par(mfrow=c(1,1),mar=c(5,5,5,5))
mypal= c('olivedrab4','lightcoral')
mycolors=mypal[as.numeric(train_pca$cc_status)]

plot(mypca$x[,1:2],pch=19, las=1, col=mycolors, cex.lab=1.5,
     xlab=substitute(PC[1]*" ("*a*"% e.v.)", list(a=9.6)),
     ylab=substitute(PC[2]*" ("*a*"% e.v.)", list(a=4.5)))
legend("topright", pch=19, col=mypal, legend=c('controls','cases'), pt.cex=1, cex=1.2)
dev.off()

# score plot for Biomarker
mypca <- prcomp(Biomarker,scale=TRUE)

pdf('Results_PCA_SVM/SVM_PCA_Biomarker_score_plot.pdf',height=10,width=10)
par(mfrow=c(1,1),mar=c(5,5,5,5))
mypal= c('olivedrab4','lightcoral')
mycolors=mypal[as.numeric(train_pca$cc_status)]

plot(mypca$x[,1:2],pch=19, las=1, col=mycolors, cex.lab=1.5,
     xlab=substitute(PC[1]*" ("*a*"% e.v.)", list(a=13.8)),
     ylab=substitute(PC[2]*" ("*a*"% e.v.)", list(a=11.7)))
legend("topright", pch=19, col=mypal, legend=c('controls','cases'), pt.cex=1, cex=1.2)
dev.off()
