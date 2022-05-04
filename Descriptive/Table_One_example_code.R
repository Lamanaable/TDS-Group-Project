

#Specify data to use
CRC_Matched_data <- readRDS("/rds/general/user/syl416/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_case_data_Matched_5_year.rds")


#Reformatting assessment centres to names of towns instead of codes -> creating new column assessment_centre_town
# https://biobank.ndph.ox.ac.uk/ukb/coding.cgi?id=10

centre_codes<-c("10003", "11001", 
"11002", "11003", "11004", "11005", "11006", "11007", "11008", 
"11009", "11010", "11011", "11012", "11013", "11014", "11016", 
"11017", "11018", "11020", "11021", "11022", "11023")

centre_towns<-c("Stockport (pilot)", "Manchester", 
"Oxford", "Cardiff","Glasgow","Edinburgh","Stoke","Reading","Bury",
"Newcastle", "Leeds",  "Bristol", "Barts","Nottingham","Sheffield", "Liverpool",
"Middlesborough", "Hounslow", "Croydon", "Birmingham", "Swansea", "Wrexham")

# centre_conversion<-c("10003", "11001", 
#                 "11002", "11003", "11004", "11005", "11006", "11007", "11008", 
#                 "11009", "11010", "11011", "11012", "11013", "11014", "11016", 
#                 "11017", "11018", "11020", "11021", "11022", "11023")


CRC_Matched_data$assessment_centre_town<-centre_towns[match(CRC_Matched_data$assessment_centre,centre_codes)]

# Change screening from -3, -1, 0 and 1
# https://biobank.ndph.ox.ac.uk/ukb/coding.cgi?id=100349

CRC_Matched_data$CRC_screening_0 <- replace(CRC_Matched_data$CRC_screening_0,CRC_Matched_data$CRC_screening_0==-3,"Prefer not to answer")
CRC_Matched_data$CRC_screening_0 <- replace(CRC_Matched_data$CRC_screening_0,CRC_Matched_data$CRC_screening_0==-1,"Do not know")
CRC_Matched_data$CRC_screening_0 <- replace(CRC_Matched_data$CRC_screening_0,CRC_Matched_data$CRC_screening_0==0,"No")
CRC_Matched_data$CRC_screening_0 <- replace(CRC_Matched_data$CRC_screening_0,CRC_Matched_data$CRC_screening_0==1,"Yes")
table(CRC_Matched_data$CRC_screening_0)

# Changing sex from 0 and 1 to female and male
# https://biobank.ctsu.ox.ac.uk/crystal/coding.cgi?id=9

table(CRC_Matched_data$sex, useNA = "always")

# This doesn't work and I'm not sure why...
# CRC_Matched_data$sex <- replace(CRC_Matched_data$sex, CRC_Matched_data$sex==0,"Female")
# CRC_Matched_data$sex <- replace(CRC_Matched_data$sex, CRC_Matched_data$sex==1,"Male")

#See available variables
var_list <- dput(names(CRC_Matched_data)) # Creates a list of variable names!
var_list

# Select the variables we want in the table
myVars<-c("sex", "age_recr", "assessment_centre_town","CRC_screening_0")

# Define the categorical variable
catVars<-c("sex","assessment_centre_town","CRC_screening_0","ethnicity")

# Create and print tableOne
tableOne<-CreateTableOne(vars=myVars, data = CRC_Matched_data, factorVars=catVars, strata="CRC_case",addOverall = TRUE)
print(tableOne, showAllLevels = TRUE)

# Prints table in latex/markdown form (can then easily be knit/edited elsewhere)
kableone(print(tableOne, showAllLevels = TRUE))











# new version of table1 ---------------------------------------------------
#the above package (CreateTableOne) no longer works on the version of R on login node.
#trying the below:

#install.packages("table1")
library(table1)

#load data
CRC_data_ordered <- readRDS("/rds/general/user/syl416/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_5_year_ordered_V3.rds")
CRC_data_matched <- readRDS("/rds/general/user/syl416/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_case_data_Matched_5_year.rds")

#before imputation
# CRC_case_data_linked_5_year <- readRDS("/rds/general/user/syl416/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_case_data_linked_5_year.rds")
#after imputation
# CRC_case_data_QC_final_5_year <- readRDS("/rds/general/user/syl416/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_case_data_QC_final_5_year.rds")


CRC_data_ordered$cc_status <- factor(CRC_data_ordered$cc_status, labels=c("Control", "Case"))

label(CRC_data_ordered$sex) <- "Sex"

CRC_data_ordered$sex <- factor(CRC_data_ordered$sex, labels=c("Female", "Male"))

label(CRC_data_ordered$age_recr) <- "Age at Recruitment"

#ethnicity
table(CRC_data_ordered$ethnicity_cat_Asian) #99
table(CRC_data_ordered$ethnicity_cat_Black) #49
table(CRC_data_ordered$ethnicity_cat_Mixed) #25
table(CRC_data_ordered$ethnicity_cat_Other) #34
table(CRC_data_ordered$ethnicity_cat_White) #5707
CRC_data_ordered$ethnicity_cat <- CRC_data_matched$ethnicity
CRC_data_ordered$ethnicity_cat <- replace(CRC_data_ordered$ethnicity_cat,CRC_data_ordered$ethnicity_cat_Asian==1,"Asian")
CRC_data_ordered$ethnicity_cat <- replace(CRC_data_ordered$ethnicity_cat,CRC_data_ordered$ethnicity_cat_Black==1,"Black")
CRC_data_ordered$ethnicity_cat <- replace(CRC_data_ordered$ethnicity_cat,CRC_data_ordered$ethnicity_cat_Mixed==1,"Mixed")
CRC_data_ordered$ethnicity_cat <- replace(CRC_data_ordered$ethnicity_cat,CRC_data_ordered$ethnicity_cat_Other==1,"Other")
CRC_data_ordered$ethnicity_cat <- replace(CRC_data_ordered$ethnicity_cat,CRC_data_ordered$ethnicity_cat_White==1,"White")
CRC_data_ordered$ethnicity_cat <- replace(CRC_data_ordered$ethnicity_cat,CRC_data_matched$ethnicity==-1,"Do not know")
CRC_data_ordered$ethnicity_cat <- replace(CRC_data_ordered$ethnicity_cat,CRC_data_matched$ethnicity==-3,"Prefer not to answer")
CRC_data_ordered$ethnicity_cat <- replace(CRC_data_ordered$ethnicity_cat,is.na(CRC_data_ordered$ethnicity_cat),"Prefer not to answer")
label(CRC_data_ordered$ethnicity_cat) <- "Ethnicity"

table(CRC_data_ordered$ethnicity_cat)

##there are 3 missing values in ethnicity (imputed to prefer not to answer)
table(is.na(CRC_data_ordered$ethnicity_cat))

#assessment centre
centre_codes<-c("10003", "11001", 
                "11002", "11003", "11004", "11005", "11006", "11007", "11008", 
                "11009", "11010", "11011", "11012", "11013", "11014", "11016", 
                "11017", "11018", "11020", "11021", "11022", "11023")
centre_towns<-c("Stockport (pilot)", "Manchester", 
                "Oxford", "Cardiff","Glasgow","Edinburgh","Stoke","Reading","Bury",
                "Newcastle", "Leeds",  "Bristol", "Barts","Nottingham","Sheffield", "Liverpool",
                "Middlesborough", "Hounslow", "Croydon", "Birmingham", "Swansea", "Wrexham")
CRC_data_ordered$assessment_centre <- CRC_data_matched$assessment_centre
CRC_data_ordered$assessment_centre_town<-centre_towns[match(CRC_data_ordered$assessment_centre,centre_codes)]
label(CRC_data_ordered$assessment_centre_town) <- "Assessment Centre"

#CRC screening
table(CRC_data_ordered$CRC_screening_0)
CRC_data_ordered$CRC_screening_0 <- replace(CRC_data_ordered$CRC_screening_0,CRC_data_ordered$CRC_screening_0==-3,"Prefer not to answer")
CRC_data_ordered$CRC_screening_0 <- replace(CRC_data_ordered$CRC_screening_0,CRC_data_ordered$CRC_screening_0==-1,"Do not know")
CRC_data_ordered$CRC_screening_0 <- replace(CRC_data_ordered$CRC_screening_0,CRC_data_ordered$CRC_screening_0==0,"No")
CRC_data_ordered$CRC_screening_0 <- replace(CRC_data_ordered$CRC_screening_0,CRC_data_ordered$CRC_screening_0==1,"Yes")
label(CRC_data_ordered$CRC_screening_0) <- "CRC Screening"

##there are 4 missing values in CRC_screening_0 (not imputed)
table(is.na(CRC_data_ordered$CRC_screening_0))

#p-value function
pvalue <- function(CRC_data_ordered, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(CRC_data_ordered)
  g <- factor(rep(1:length(CRC_data_ordered), times=sapply(CRC_data_ordered, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

table1(~ sex + age_recr + ethnicity_cat + assessment_centre_town + CRC_screening_0 |
         cc_status, data=CRC_data_ordered)

table1(~ sex + age_recr + ethnicity_cat + assessment_centre_town + CRC_screening_0 |
         cc_status, data=CRC_data_ordered, overall=F, extra.col=list (`P-value` =pvalue))
 

