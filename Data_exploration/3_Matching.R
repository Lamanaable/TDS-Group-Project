### Matching Cases and controls

#Loading the data and packages
args <- commandArgs(trailingOnly = TRUE)
CRC_path <- as.character(args[1])
CRC <- readRDS(CRC_path)

#install.packages("MatchIt", repos = "http://cran.us.r-project.org") #Add back in if needed
library(MatchIt)
#library(knitr)
#library(pacman)
library(tidyverse)
library(zoo)
library(dplyr)

#Removing NAs
print("removing NAs")
CRC <- CRC[complete.cases(CRC[ , c("sex", "age_recr", "assessment_centre")]),]

#Checking for NAs
print("checking for NAs")
sum(is.na(CRC$sex))
sum(is.na(CRC$age_recr))
sum(is.na(CRC$assessment_centre))

#Converting variables to factors
print("factorising")
CRC$assessment_centre <- as.factor(CRC$assessment_centre)
CRC$CRC_case <- as.factor(CRC$CRC_case)
CRC$sex <- as.factor(CRC$sex)

#Matching function, could use a different method, exact, full?
print("matching")
set.seed(1)
match.it <- matchit(CRC_case ~ age_recr + assessment_centre + sex + date_recr, data = CRC, method= "nearest", ratio = 2)

# a <- summary(match.it)
# 
# kable(a$nn, digits = 2, align = 'c', 
#       caption = 'Table 2: Sample sizes', "simple")

#New dataset of only matched cases and controls
print("new matching dataset of cases and controls")
df.match <- match.data(match.it)[1:ncol(CRC)]


# pacman::p_load(tableone)
# table4 <- CreateTableOne(vars = c('age_recr', 'sex', 'assessment_centre'), 
#                          data = df.match, 
#                          factorVars = c('sex', 'assessment_centre'),
#                          strata = "CRC_case")
# table4 <- print(table4, 
#                 printToggle = FALSE, 
#                 noSpaces = TRUE)
# 
# View(table4)

#Finding which controls are matched to each case
print("Finding which controls are matched to each case")
matches <- data.frame(match.it$match.matrix)

#Code to group cases and matched controls, added as a group column using the case ID.
print("Group ID")
df.match_new <- rownames_to_column(df.match, var = "ID")

matches <- rownames_to_column(matches, var = "ID")

df.match$group <- df.match_new$ID

print("loop")
for (j in 1:(ncol(matches)-1)){
  for(i in 1:nrow(matches)){
    df.match$group <- ifelse(matches[i, j+1]==df.match$group, matches[i,1],df.match$group)
  }
}

table(table(df.match$group))
#Code to remove controls who died before diagnosis date of the case
print("removing controls who died before diagnosis date")
diag_dates <- data.frame(df.match$group, df.match$date_diagnosis, df.match$CRC_case)
diag_dates <- diag_dates[complete.cases(diag_dates[ , c("df.match.group", "df.match.date_diagnosis")]),]

row_sub = apply(diag_dates, 1, function(row) all(row !=0 ))

diag_dates <- diag_dates[row_sub,]

CRC_diagnosis_dates <- df.match$date_diagnosis
today <- Sys.Date()
df.match$death_date_0 <- ifelse(is.na(df.match$death_date_0), today, df.match$death_date_0)

df.match$date_diagnosis <- as.numeric(df.match$date_diagnosis)

print("another loop")
for (j in which(is.na(df.match[, "date_diagnosis"]))) {
  df.match[j, "date_diagnosis"] <- mean(df.match[df.match[, "group"] == df.match[j, "group"], "date_diagnosis"],  na.rm = TRUE)
}

df.match$group <- ifelse(df.match$death_date_0 < df.match$date_diagnosis, NA, df.match$group)
df.match$date_diagnosis <- CRC_diagnosis_dates
df.match <- df.match[complete.cases(df.match[, c("group")]),]

df.match$death_date_0 <- ifelse(df.match$death_date_0 == today, NA, df.match$death_date_0)

df.match$death_date_0 <- as.Date(df.match$death_date_0, origin = "1970-01-01")

table(table(df.match$group))

# WEI AND ELLIE HAVE COMMENTED THIS MANUAL CHANGE OUT...CHECK LATER
#df.match <- df.match[!(df.match$group == 5804726 | df.match$group == 4929129),]


#Saving matched dataset as an rds
print("Saving interim data")
saveRDS(df.match, "Outputs/CRC_case_data_Matched_interim.rds")


print("Checking individuals with no controls and removing")
table(table(df.match$group))

# Removing those without controls

name <- names(table(df.match$group)[table(df.match$group)<2])
df.match <- df.match %>% filter(!(df.match$group %in% name))


#Saving matched dataset as an rds
print("Final data save")
saveRDS(df.match, "Outputs/CRC_case_data_Matched.rds")




