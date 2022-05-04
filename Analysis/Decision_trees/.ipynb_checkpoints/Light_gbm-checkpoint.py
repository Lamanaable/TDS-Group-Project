

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import lightgbm as lgbm 
import time


from sklearn.model_selection import StratifiedKFold

import shap

import math



data = pd.read_csv("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration/Outputs/CRC_data_5_year_final.csv")



def get_results(truth, prediction, df, idx):
    
    y_test2 = truth.map({'Control':0, 'Case':1})
    y_pred2 = pd.Series(prediction).map({'Control':0, 'Case':1})


    acc = accuracy_score(y_test2, y_pred2)
    f1 = f1_score(y_test2, y_pred2)
    precision = precision_score(y_test2, y_pred2)
    recall = recall_score(y_test2, y_pred2)

    df.loc[idx,:] = [acc, f1, precision, recall]

    return df



data = data.drop(columns = ['date_diagnosis', 'CRC_dvlp_year'
                    , 'age_first_screened_CRC', 'CRC_screening_0'
                    , 'death_date_0', 'group', 'Unnamed: 0'
                           , 'incident_case', 'prevalent_case',
                           'age_recr'])
# 
# numerical = ["Model_smoking_pack_year", "Model_height", "Weight",
#              "waist_hip_ratio", "deprivation_townsend" , "household_income_GBP", "time_TV",
#              "time_computer", "weekly_mobile_len_hrs", "sleep_duration" ,
#              "smoking_num_household", "diet_cereal", "diet_tea", "diet_water",
#              "weekly_red_wine_intake" ,
#              "weekly_champagne_and_white_wine_intake","weekly_beer_intake",
#              "weekly_spirits_intake" , "weekly_fortified_wine_intake", "father_death_age",
#              "overall_health_rate", "mother_death_age" , "pollution_NO_2010",
#              "pollution_particulate_PM10", "pollution_particulate_PM2.5",
#              "pollution_particulate_PM2.5_absorbance" , "pollution_particulate_PM_course",
#              "score_income", "score_employment", "score_health", "score_education" ,
#              "score_housing", "score_living_environment", "biomarker_RBC", "biomarker_HCT",
#              "biomarker_platelet", "biomarker_lymphocyte" , "biomarker_Neu",
#              "biomarker_urine_K", "biomarker_urine_Na", "biomarker_CHL", "biomarker_CRP",
#              "biomarker_glucose", "biomarker_HbA1c" ,"biomarker_HDL",
#              "biomarker_IGF_one","biomarker_LDL", "biomarker_TBL", "biomarker_Tprotein",
#              "biomarker_TRG" , "biomarker_VD" , "pulse" , "sysBP" , "diaBP" ,
#              "medication_20003_Aspirin" , "medication_20003_NSAIDS" ,
#              "medication_20003_Statins" , "medication_20003_antioxidants" ,
#              "medication_20003_antibiotics" , "medication_20003_ACEI" ,
#              "medication_20003_ARB" , "medication_20003_CCB" ,
#              "medication_20003_BetaBlocker" , "medication_20003_diuretics" ,
#              "medication_20003_glucosamine" , "medication_20003_H_Blocker" ,
#              "medication_20003_metformin" , "medication_20003_omega_3" ,
#              "operation_20004_appendicectomy" ,
#              "operation_20004_cholecystectomy.gall.bladder.removal" ,
#              "operation_20004_sterilisation" , "comorbidity_20002_asthma" ,
#              "comorbidity_20002_depression" , "comorbidity_20002_diabetes" ,
#              "comorbidity_20002_gastro.oesophageal.reflux..gord....gastric.reflux" ,
#              "comorbidity_20002_hayfever.allergic.rhinitis" ,
#              "comorbidity_20002_heart.attack.myocardial.infarction" ,
#              "comorbidity_20002_high.cholesterol" , "comorbidity_20002_hypertension" ,
#              "comorbidity_20002_hypothyroidism.myxoedema" ,
#              "comorbidity_20002_osteoarthritis" ,
#              "comorbidity_D_benign_rectal_neoplasms_poly" ,
#              "comorbidity_A_Intestinal_infectious_diseases" , "comorbidity_B_infectious" ,
#              "comorbidity_C_D_neoplasms" , "comorbidity_D_Blood_Immune" ,
#              "comorbidity_E_Endocrine_thyroid" , "comorbidity_E_Diabetes" ,
#              "comorbidity_E_Obesity" , "comorbidity_E_Metabolic_disorders" ,
#              "comorbidity_F_Mental_disorder" , "comorbidity_G_Nervous_system" ,
#              "comorbidity_G_Eye_adnexa" , "comorbidity_G_Ear" ,
#              "comorbidity_I_Circ_Hypertensive" , "comorbidity_I_Circ_Ischaemic_heart" ,
#              "comorbidity_I_other_heart_diseases" , "comorbidity_I_Cerebrovascular" ,
#              "comorbidity_I_Arteries" , "comorbidity_I_Veins" , "comorbidity_J_Respiratory"
#              , "comorbidity_K_oral" , "comorbidity_K_Gastrooesophageal_reflux_disease" ,
#              "comorbidity_K_Other_oesophagus" , "comorbidity_K_Gastric_ulcer" ,
#              "comorbidity_K_Duodenal_ulcer" , "comorbidity_K_Gastritis_duodenitis" ,
#              "comorbidity_K_Dyspepsia" , "comorbidity_K_other_stomach" ,
#              "comorbidity_K_Appendix" , "comorbidity_K_Hernia" , "comorbidity_K_51_52_IBD"
#              , "comorbidity_K55_K64_Other_intestines" , "comorbidity_K7_Liver" ,
#              "comorbidity_K8_Gallbladder" , "comorbidity_L_skin" ,
#              "comorbidity_N_Renal_Failure" , "comorbidity_N_Urolithiasis" ,
#              "comorbidity_N_Other_urinary" , "comorbidity_Q_Congenital" ,
#              "comorbidity_R0_Other_symptoms" , "comorbidity_R1_Other_symptoms" ,
#              "comorbidity_R3_Other_symptoms", "tt_status"]
# 
# df = pd.DataFrame()
# for col in numerical:
#     df = df.append(data.pop(col))
# 
# numericals = df.T
# 
# 
# for col in data.columns:
#     data[col] = data[col].astype('category')
# 
# data[numerical] = numericals
# 
# 


df1 = data.pop('cc_status') # remove column b and store it in df1
#df1 = df1.map({0:'Control', 1:'Case'})

data['cc_status']=df1 # add b series as a 'new' column.



#data['cc_status'] = data['cc_status'].astype('category')

X_train = data.loc[data['tt_status'] == 'train']
X_train = X_train.drop(['tt_status'], axis = 1)
y_train = X_train[['cc_status']]
y_train = y_train.squeeze()
X_train = X_train.drop(['cc_status'], axis = 1)

X_test = data.loc[data['tt_status'] == 'test']
X_test = X_test.drop(['tt_status'], axis = 1)
y_test = X_test[['cc_status']]
y_test = y_test.squeeze()
X_test = X_test.drop(['cc_status'], axis = 1)




clf = lgbm.LGBMClassifier(
    objective="binary", n_estimators=1000, random_state=8
)

eval_set = [(X_test, y_test)]

clf.fit(
    X_train,
    y_train,
    eval_set=eval_set,
    early_stopping_rounds=100,
    eval_metric="binary_logloss",
)

N_SPLITS = 7
strat_kf = StratifiedKFold(n_splits=N_SPLITS, shuffle=True, random_state=1121218)

scores = np.empty(N_SPLITS)
for idx, (train_idx, test_idx) in enumerate(strat_kf.split(X_train, y_train)):
    print("=" * 12 + f"Training fold {idx}" + 12 * "=")
    start = time.time()

    X_train_tmp, X_val = X_train.iloc[train_idx], X_train.iloc[test_idx]
    y_train_tmp, y_val = y_train[train_idx], y_train[test_idx]
    eval_set_tmp = [(X_val, y_val)]

    lgbm_clf = lgbm.LGBMClassifier(n_estimators=10000)
    lgbm_clf.fit(
        X_train_tmp,
        y_train_tmp,
        eval_set=eval_set_tmp,
        early_stopping_rounds=200,
        eval_metric="binary_logloss",
        verbose=False,
    )

    preds = lgbm_clf.predict_proba(X_val)
    loss = log_loss(y_val, preds)
    scores[idx] = loss
    runtime = time.time() - start
    print(f"Fold {idx} finished with score: {loss:.5f} in {runtime:.2f} seconds.\n")
    

