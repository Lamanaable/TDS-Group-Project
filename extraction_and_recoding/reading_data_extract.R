##### Please copy this file to your own directory and do not edit
##### the file here. Thank you.

rm(list=ls())
library(data.table)

# Loading an extract of the data (500 rows and 1,000 columns)
ukb_sample=fread("Data/ukb47946.csv", data.table=FALSE, nrows=500, select=1:1000)

hesin_sample=fread("Data/hesin.txt", data.table=FALSE, nrows=500, select=1:42)

hesin_diag_sample=fread("Data/hesin_diag.txt", data.table=FALSE, nrows=500, select=1:8)

hesin_oper_sample=fread("Data/hesin_oper.txt", data.table=FALSE, nrows=500, select=1:11)

death_sample=fread("Data/death.txt", data.table=FALSE, nrows=500, select=1:5)

death_cause_sample=fread("Data/death_cause.txt", data.table=FALSE, nrows=500, select=1:5)

covid_england_sample=fread("Data/covid19_result_england.txt", data.table=FALSE, nrows=500, select=1:9)

covid_wales_sample=fread("Data/covid19_result_wales.txt", data.table=FALSE, nrows=500, select=1:7)

covid_scotland_sample=fread("Data/covid19_result_scotland.txt", data.table=FALSE, nrows=500, select=1:6)

ukb_extract_sample=fread("ukb_extracted.rds", data.table=FALSE, nrows=500) 



