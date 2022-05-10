Folder for PLS analysis

-------------
Scripts:
--------------
00_Grou_CRC
Group variables to ensure further grouping analysis

01_PLS_calib.R
Test for basic calibration, would not use this result as we have further stability selection

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


-------------
Folders:
-------------
Figures: Output for exploratory Venn plot

Outputs: Outputs for SgPLS

Scripts: sgPLS function.R

Sensitivity_Analysis: Colon_only and Rectal_only, Outputs for outputs

Summary_lasso_sgPLS: Outputs for intersection model


