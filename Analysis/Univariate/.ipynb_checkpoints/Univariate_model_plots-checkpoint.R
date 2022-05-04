##### Univariate analysis

library(tidyverse)
library(ggrepel)

# Reading data (NOTE THIS IS THE ONE-HOT ENCODED DATA)
data <- readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_5_year_final.rds")

# Confirmation of columns to remove
colnames(data)[c(1:7,9:10)]

# Removing CRC related variables
data_prep<-data[,-c(1:7,9:10)]
ncol(data_prep)

# Converting to X and Y

X <-as.data.frame(subset(data_prep, select=-cc_status))
Y <-as.matrix(subset(data_prep, select=cc_status))
Y<-as.numeric(Y)

# Running model

# Empty matrix - row column for each variable, one column for p values, 
uni_model<-matrix(NA,nrow=ncol(X),ncol=6)
uni_model<-as.data.frame(uni_model)
uni_model[,1]<-colnames(X)
colnames(uni_model)<-c("Variable","beta","p","p<0.05","BH","Bonferroni")

for (i in 1:ncol(X)){
  var=X[,i]
  model <- glm(Y~var, family="binomial")
  uni_model[i,2] <- exp(summary(model)$coef[2,1])
  uni_model[i,3] <- summary(model)$coef[2,4]
}


uni_model[,4]<-ifelse(uni_model[,3]<0.05,1,0)

uni_model[,5]<-ifelse(p.adjust(uni_model[,3], method = "bonf") < 0.05,1,0)

uni_model[,6]<-ifelse(uni_model[,3]<0.05/ncol(X),1,0)

which(as.vector(uni_model$BH)==1)
uni_model$p[which(as.vector(uni_model$BH)==1)]
BH_p_val<-min(uni_model$p[-which(as.vector(uni_model$BH)==1)])

# Creating a variable type category ---------------------------------------


uni_model$var_type<-NA
# Rows 1 to 27 -> Baseline model
uni_model$var_type[1:27]<-"Baseline"
# Rows 24 to 34 -> General/behavioural
uni_model$var_type[24:34]<-"General"
# Rows 35 to 51 -> Diet
uni_model$var_type[35:51]<-"Diet"
# Rows 52 to 65 - > General
uni_model$var_type[52:65]<-"General"
# Rows 66 to 68 -> Family
uni_model$var_type[66:68]<-"Family history"
# Rows 69 to 70 -> General/behavioural
uni_model$var_type[69:70]<-"General"
# Rows 71 -> Family
uni_model$var_type[71]<-"Family history"
# Rows 72 to 74 -> General
uni_model$var_type[72:74]<-"General"
# Rows 75 to 77 -> Diet
uni_model$var_type[75:77]<-"Diet"
# Rows 78 to 87 -> General (incl pollution, income, education)
uni_model$var_type[78:93]<-"General"
# Rows 94 to 111 - > Biomarkers
uni_model$var_type[94:111]<-"Biomarkers"
# 112 to 119 -> General/behavioural/income
uni_model$var_type[112:119]<-"General"
# 120 to 143 -> Medications
uni_model$var_type[120:143]<-"Medications"
# 144 to 146 -> Operations
uni_model$var_type[144:146]<-"Operations"
# 147 to 157 -> Family
uni_model$var_type[147:157]<-"Family history"
# 158 to 215 -> comorbidity
uni_model$var_type[158:215]<-"Comorbidities"



# Plotting ----------------------------------------------------------------


# Struggling to remove the ticks on the x axis

ggplot(data=uni_model,aes(x=1:nrow(uni_model) , y=-log(p,10),color=var_type)) +
  geom_point() +
  labs(color="Variable group")  +
  geom_hline(yintercept = -log(0.05,10), linetype="dashed", col="grey60") +
  annotate("text",x=0,y=1.45,color="grey60",label="p = 0.05",size=3) +
  geom_hline(yintercept = -log(0.05/ncol(X),10), linetype="dashed", col="gray48") +
  annotate("text",x=0,y=3.85,color="gray48",label="Bonferroni",size=3) +
  geom_text(aes(label=ifelse(p<0.05,Variable,"")),hjust=0,vjust=0, size=3) +
  labs(title="Manhattan plot of univariate association with 5-year CRC",
       x="Variable", y="log10(p)") +
  theme_classic() + 
  theme(axis.text.x=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))
  

  geom_text(aes(0,-log(0.05,10),label="p=0.05", vjust=-1),size=2,color="blue") +

BH_p_val
0.05/ncol(X)

# uni_model$p<-as.numeric(uni_model$p)
# 
# uni_model$fold_change<-as.numeric(uni_model$beta)-1
# 
# uni_model %>%
#   ggplot(aes(x=log(fold_change,2),y=-log10(p)))+
#   geom_point(colour=)+
#   geom_hline(yintercept = -log10(0.05),
#              linetype = "dashed") + 
#   geom_vline(xintercept = c(log2(0.5), log2(2)),
#              linetype = "dashed")+




