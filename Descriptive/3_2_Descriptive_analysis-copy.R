## DESCRIPTIVE ANALYSIS

# Date: 
# Authors: TDS Group 2 
# Description: descriptive analysis and table 1
# output:

# 1. Setup -------------------------------------------------------------------
rm(list=ls())

# Packages
library(tidyverse)
library(RColorBrewer)

# 2. Reading the data --------------------------------------------------------

#args <- commandArgs(trailingOnly = TRUE)
#CRC_path <- as.character(args[1])

#CRC_data <- readRDS(CRC_path)

setwd("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Descriptive")

#10 year data
#CRC_Matched_data <- readRDS("/rds/general/user/syl416/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_case_data_Matched.rds")

#5 year data
CRC_Matched_data <- readRDS("/rds/general/user/syl416/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_case_data_Matched_5_year.rds")

#brewer.pal(n=9, name = "Blues")

# 3. Age distribution -----------------------------------------------------
CRC_Matched_data$CRC_case <- factor(CRC_Matched_data$CRC_case, labels=c("Control", "Case"))

#overall age distribution
ggplot(data=CRC_Matched_data, aes(x=age_recr)) +
  geom_density(alpha=1, fill="#003399") +
  theme_bw() +
  xlab("Age Recruited (years)") +
  ylab("Density") +
  ggtitle("Overall Age Distribution") +
  ggsave(path = "Results", filename = "overall_age_densityplot.png")

#overlapping case control age distribution - density plot
ggplot(data=CRC_Matched_data, aes(x=age_recr, fill=CRC_case)) +
  geom_density(alpha=0.5) +
  theme_bw() +
  xlab("Age Recruited (years)") +
  ylab("Density") +
  ggtitle("Age Distribution by Case Control Status") +
  scale_fill_manual(values=c("#6699CC", "#08519C")) +
  labs(fill='Case Control Status') +
  ggsave(path = "Results", filename = "casecontrol_age_densityplot.png")
  
#overlapping case control age distribution - boxplot
ggplot(data=CRC_Matched_data, aes(x=age_recr, y=CRC_case, fill=CRC_case)) +
  geom_boxplot(alpha=1, fill=c("#6699CC", "#08519C")) +
  theme_bw() +
  xlab("Age Recruited (years)") +
  ylab("Case/Control status") +
  ggtitle("Age Distribution by Case/Control status") +
  coord_flip() +
  stat_summary(fun=mean, geom="point", shape=4, size=4) +
  theme(legend.position="none") +
  ggsave(path = "Results", filename = "casecontrol_age_boxplot.png")


# 4. sex distribution --------------------------------------------------------
CRC_Matched_data$sex <- factor(CRC_Matched_data$sex, labels=c("Female", "Male"))

#overall sex distribution
ggplot(data=CRC_Matched_data, aes(sex, fill=sex)) + 
  geom_bar(fill=c("#6699CC", "#08519C")) +
  theme_bw() +
  xlab("Sex") +
  ylab("Count") +
  ggtitle("Sex Distribution") +
  ggsave(path = "Results", filename = "overall_sex_barplot.png")

#sex distribution by case/control status
ggplot(data=CRC_Matched_data, aes(CRC_case)) + 
  geom_bar(stat="count", position="dodge", aes(fill=sex)) +
  theme_bw() +
  xlab("Case Control status") +
  ylab("Count") +
  ggtitle("Sex Distribution by Case Control Status") +
  scale_fill_manual(values=c("#6699CC", "#08519C")) +
  labs(fill='Sex') #+
  #ggsave(path = "Results", filename = "casecontrol_sex_barplot.png")


# 5. assessment centre ----------------------------------------------------
#adding labels to assessment centre
centre_codes<-c("10003", "11001", 
                "11002", "11003", "11004", "11005", "11006", "11007", "11008", 
                "11009", "11010", "11011", "11012", "11013", "11014", "11016", 
                "11017", "11018", "11020", "11021", "11022", "11023")

centre_towns<-c("Stockport (pilot)", "Manchester", 
                "Oxford", "Cardiff","Glasgow","Edinburgh","Stoke","Reading","Bury",
                "Newcastle", "Leeds",  "Bristol", "Barts","Nottingham","Sheffield", "Liverpool",
                "Middlesborough", "Hounslow", "Croydon", "Birmingham", "Swansea", "Wrexham")


CRC_Matched_data$assessment_centre_town<-centre_towns[match(CRC_Matched_data$assessment_centre,centre_codes)]

#overall assessment centre distribution
ggplot(data=CRC_Matched_data, aes(assessment_centre_town)) + 
  geom_bar(fill="#003399") +
  theme_bw() +
  xlab("Assessment Centre") +
  ylab("Count") +
  ggtitle("Assessment Centre Distribution") +
  coord_flip() +
  ggsave(path = "Results", filename = "overall_centre_barplot.png")

#overall assessment centre distribution by case/control status
ggplot(data=CRC_Matched_data, aes(assessment_centre_town)) + 
  geom_bar(stat="count", position="dodge", aes(fill=CRC_case)) +
  theme_bw() +
  xlab("Assessment Centre") +
  ylab("Count") +
  ggtitle("Assessment Centre Distribution by Case Control Status") +
  coord_flip() +
  scale_fill_manual(values=c("#6699CC", "#08519C")) +
  labs(fill='Case Control Status') +
  ggsave(path = "Results", filename = "casecontrol_centre_barplot.png")


# 6. CRC year developed ---------------------------------------------------
#CRC_Matched_data$CRC_dvlp_year_rounded <- CRC_Matched_data$CRC_dvlp_year

# CRC_Matched_data$CRC_dvlp_year_rounded <- mutate(CRC_Matched_data,
#        CRC_dvlp_year_rounded = case_when(
#          CRC_dvlp_year <= 2 ~ "1<x<=2 years",
#          CRC_dvlp_year <= 3 ~ "2<x<=3 years",
#          CRC_dvlp_year <= 4 ~ "3<x<=4 years",
#          CRC_dvlp_year <= 5 ~ "4<x<=5 years",
#          TRUE ~ NA_character_)
# )
# CRC_Matched_data$CRC_dvlp_year_rounded <- as.factor(CRC_Matched_data$CRC_dvlp_year_rounded)
# 
#   
# ggplot(data=CRC_Matched_data, aes(CRC_dvlp_year_rounded)) +
#   geom_bar(fill="#003399", colour="black") +
#   theme_bw() +
#   #xlab("Time from recruitment until CRC diagnosis (years)") +
#   xlim(c(1,5)) +
#   ylab("Count") +
#   ggtitle("Time from recruitment until CRC diagnosis") #+
#   #scale_x_continuous(breaks = seq(0, 20, by = 1)) #+
#   #ggsave(path = "Results", filename = "CRC_dvlp_year_histogram.png")


#CRC_time_labels <- c("1<x<=2", "2<x<=3", "3<x<=4", "4<x<=5")
CRC_time_labels <- c("1-2", "2-3", "3-4", "4-5")

ggplot(data=CRC_Matched_data[CRC_Matched_data$CRC_case==1,],
       aes(x=CRC_dvlp_year_new))+
  geom_bar(fill="#003399", colour="black") +
  theme_bw() +
  xlab("Time from recruitment until CRC diagnosis (years)") +
  ylab("Count") +
  ggtitle("Time from Recruitment until CRC Diagnosis") +
  scale_x_discrete(labels= CRC_time_labels) +
  ggsave(path = "Results", filename = "CRC_dvlp_year_histogram.png")

table(CRC_Matched_data$CRC_dvlp_year_new)

