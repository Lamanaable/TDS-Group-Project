# GROUP 2 TDS Project Git Repository

## Project Title:
Constructed Framework for Risk-Stratification and Risk Prediction for Colorectal Cancer Using UK Biobank  

### Authors:  
Ellie Bloom, Herman Eaves, Sophie Lai, Wei Quan 

Imperial College London Health Data Analytics and Machine Learning MSc students 
### Finalised: 
May 2022
#### Data sources: 
Both held on secure server:   
UK Biobank   
HES 

## Contents:

1.  Outcome_definition  
Identifying cases 
  
2.  extraction_and_recoding   
Extracting selected variables, generating a data dictionary and aggregating across multiple instances 
  
3.  Data_exploration  
Combining identifies cases with curated variables. Includes applying exclusion crieria, quality check, transformation, grouping and one-hot encoding  
  
4.  Variable_definition   
Data dictionary for all variables   
  
5.  Analysis  
All of the statistical and machine learning models: 
* Univariate  
Univariate logistic analysis of each of the exposure variables with 5-year CRC outcome. 
* Logistic_regression
Multivariable logistic regression of the 16 baseline variables with 5-year CRC outcome. 
* Penalised_regression  
LASSO variable selection using full list of variables. 
* PLS 
group - Partial Least Squares - Discriminant Analysis (sgPLS-DA) variable selection using full list of variables.  
* Decision_trees  
Decision trees and boosting models  
* AUC_Comparison_Plot   
Comparison of AUC from all models run.   

