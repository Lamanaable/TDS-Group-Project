##### Univariate analysis

library(tidyverse)
library(ggrepel)

# Reading data (NOTE THIS IS THE ONE-HOT ENCODED DATA)
data <- readRDS("/rds/general/user/esb21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_5_year_ordered_V3.rds")

# Load Data
vars = read.csv('/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Variable_definition/vars_lookup.csv')
vars$Oldname <- as.character(vars$Oldname)
vars$Newname <- as.character(vars$Newname)

# Rename columns of the dataset
df = data # can change dataset to which you want to change the names

library(tidyverse)
names(df) <- vars[vars$Oldname %in% names(df),]$Newname

# remove underscores
names(df) <- gsub(x = names(df), pattern = "_", replacement = " ")

# see what has been changed
names(df)

data=df

# Confirmation of columns to remove
colnames(data)[c(1:7,9:10)]

# Removing CRC related variables
data_prep<-data[,-c(1:7,9:10)]
ncol(data_prep)

# Converting to X and Y

X <-as.data.frame(subset(data_prep, select=-1))
Y <-as.matrix(subset(data_prep, select=+1))
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

length(uni_model$p[uni_model$`p<0.05`==1])
which(as.vector(uni_model$BH)==1) # 8 significant variables significant at Bongerroni level
uni_model[which(as.vector(uni_model$BH)==1),] # Details of these variables
BH_p_val<-min(uni_model$p[-which(as.vector(uni_model$BH)==1)])

# Creating a variable type category ---------------------------------------

uni_model$var_type<-NA

uni_model$var_type[1:23]<-"Baseline"
uni_model$var_type[24:28]<-"Physical"
uni_model$var_type[29:51]<-"Sociodemographic"
uni_model$var_type[52:92]<-"Lifestyle"
uni_model$var_type[93:106]<-"Family history"
uni_model$var_type[107:124]<-"Biomarker"
uni_model$var_type[125:159]<-"Medication/Operation"
uni_model$var_type[160:203]<-"Comorbidity"

# Reordering clumns to make labels fit a bit better
 #


# Plotting ----------------------------------------------------------------

# Ordering data by var_type?

# Struggling to remove the ticks on the x axis

# ggplot(data=uni_model,aes(x=1:nrow(uni_model) , y=-log(p,10),color=var_type)) +
#   geom_point() +
#   labs(color="Variable group")  +
#   geom_hline(yintercept = -log(0.05,10), linetype="dashed", col="grey60") +
#   annotate("text",x=0,y=1.45,color="grey60",label="p = 0.05",size=3) +
#   geom_hline(yintercept = -log(0.05/ncol(X),10), linetype="dashed", col="gray48") +
#   annotate("text",x=0,y=3.75,color="gray48",label="Bonferroni",size=3) +
#   geom_text(aes(label=ifelse(p<0.05,Variable,"")),hjust=0,vjust=-0.5, size=3) +
#   labs(title="Manhattan plot of univariate association with 5-year CRC (vars=203)",
#        x="Variable", y=expression(paste("lo", g[10], "(p)", sep = ""))) +
#   theme_classic() + 
#   theme(axis.text.x=element_blank()) +
#   theme(plot.title = element_text(hjust = 0.5))
  

ggplot(data=uni_model,aes(x=1:nrow(uni_model) , y=-log(p,10),color=var_type)) +
  geom_point() +
  labs(color="Variable group")  +
  geom_hline(yintercept = -log(0.05,10), linetype="dashed", col="grey60") +
  annotate("text",x=0,y=1.45,color="grey60",label="p = 0.05",size=3) +
  geom_hline(yintercept = -log(0.05/ncol(X),10), linetype="dashed", col="gray48") +
  annotate("text",x=0,y=3.75,color="gray48",label="Bonferroni",size=3) +
  geom_text_repel(aes(label=ifelse(p<0.05,Variable,"")),size=2.5) +
  labs(title="Manhattan plot of univariate association with 5-year CRC (vars=203)",
       x="Variable", y=expression(paste("lo", g[10], "(p)", sep = ""))) +
  theme_classic() + 
  theme(axis.text.x=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="bottom")

# Number of variables significant at each level
