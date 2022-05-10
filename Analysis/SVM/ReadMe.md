Folder for SVM analysis

---------------
Scripts:
--------------
00_SVM_PCA_preprocessing.R
Group PCA preprocessing

00_SVM_scale.R
Normal SVM preprocessing

---------------
Folders:
---------------
01_SVM_PCA_tune:PCA-SVM tunning

01_SVM_tune:main-SVM tunning

02_SVM_PCA_result_script: python version of PCA-SVM results after tunning

02_SVM_result_script: python version of main-SVM results after tunning

03_SVM_PCA_result_R: R version of PCA-SVM results after tunning, outputs including confusion matrix and ROC curve

03_SVM_result_R: R version of main-SVM results after tunning, outputs including confusion matrix and ROC curve

Results_PCA_SVM: results for R version of PCA-SVM

Results_SVM: results for R version of main-SVM

Sens_colon: sensitivity analysis for colon

Sens_rectum: sensitivity analysis for rectum



02_PLS_result.R
Outputs for sgPLS after basic calibration, would not use this result as we have further stability selection

03_Stability_selection.R
Use focus package and do variable selection, calibrating on sparse parameters for group and within group.

04_constructed_summary_plot.R
Constructed summary plot for SgPLS, LASSO and intersection, including coefficeint, selection proportion and cumulative AUC

04_Result_LASSO_sgPLS.R
AUC for SgPLS, LASSO and intersection

05_Cox.R
Cox for intersection model

---------------
Data:
---------------
pca_train_5_year.csv/rds: training data for PCA-SVM

pca_test_5_year.csv/rds: testing data for PCA-SVM

train_5_year.csv/rds: training data for main-SVM
test_5_year.csv/rds: testing data for main-SV

