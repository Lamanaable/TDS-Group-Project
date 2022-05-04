

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

from sklearn.model_selection import train_test_split

from sklearn import tree
from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix
from sklearn.tree import export_graphviz
from IPython.display import Image 

from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import GradientBoostingClassifier

from sklearn.model_selection import GridSearchCV

from sklearn.metrics import accuracy_score
from sklearn.metrics import f1_score
from sklearn.metrics import precision_score
from sklearn.metrics import recall_score

from sklearn.inspection import permutation_importance
import shap


from mlxtend.plotting import plot_decision_regions

import math



data = pd.read_csv("/rds/general/project/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Data_exploration_old/CRC_data_grouped.csv")



def get_results(truth, prediction, df, idx):
    
    y_test2 = truth.map({'Control':0, 'Case':1})
    y_pred2 = pd.Series(prediction).map({'Control':0, 'Case':1})


    acc = accuracy_score(y_test2, y_pred2)
    f1 = f1_score(y_test2, y_pred2)
    precision = precision_score(y_test2, y_pred2)
    recall = recall_score(y_test2, y_pred2)

    df.loc[idx,:] = [acc, f1, precision, recall]

    return df



data = data.drop(columns = ['CRC_diag_date', 'CRC_dvlp_year','first_screened_CRC_instance'
                    , 'age_first_screened_CRC', 'CRC_screening_0', 'CRC_instance', 'CRC_behaviour'
                    , 'CRC_histology', 'death_date_0', 'birth_year', 'group', 'Unnamed: 0'
                           , 'sample_id', 'tt_status'])



numerical = ['Model_smoking_pack_year', 'Model_height', 'age_recr', 'Weight', 
             'waist_circumference', 'hip_circumference', 'number_cancers', 'number_noncancers'
             , 'number_operations', 'number_treatments_medication', 'deprivation_townsend', 'number_household',
             'num_vehicles', 'household_income_GBP', 'number_days_walk_ten', 'phys_act_walk_duration', 'time_outdoors_summer'
             ,'time_outdoors_winter', 'time_TV', 'time_computer', 'weekly_mobile_len_hrs',
             'sleep_duration', 'smoking_num_household', 'diet_cooked_vegatable', 'diet_raw_vegatable',
             'diet_fresh_fruit', 'diet_dried_fruit', 'diet_bread', 'diet_cereal', 'diet_tea',
             'diet_water', 'weekly_red_wine_intake', 'weekly_champagne_and_white_wine_intake', 
             'weekly_beer_intake', 'weekly_spirits_intake', 'weekly_fortified_wine_intake', 'father_death_age',
             'overall_health_rate', 'mother_death_age', 'pollution_NO_2010', 'pollution_particulate_PM10', 'pollution_particulate_PM2.5',
             'pollution_particulate_PM2.5_absorbance', 'pollution_particulate_PM_course', 'Idex_multiple_deprivation', 'score_income',
             'score_employment', 'score_health', 'score_education', 'score_housing', 'score_living_environment', 'biomarker_RBC',
             'biomarker_HCT', 'biomarker_platelet', 'biomarker_lymphocyte', 'biomarker_Neu', 'biomarker_urine_K', 'biomarker_urine_Na',
             'biomarker_CHL', 'biomarker_CRP', 'biomarker_glucose', 'biomarker_HbA1c', 'biomarker_HDL', 'biomarker_IGF_one',
             'biomarker_LDL', 'biomarker_TBL', 'biomarker_Tprotein', 'biomarker_TRG', 'biomarker_VD', 'pulse', 'sysBP', 'diaBP',
            'cardiovascular_Angina', 'cardiovascular_Heart.attack', 'cardiovascular_High.blood.pressure',
            'cardiovascular_Stroke', 'comorbidity_field6152_Asthma', 'comorbidity_field6152_Blood.clot.in.the.leg..DVT.',
            'comorbidity_field6152_Emphysema.chronic.bronchitis', 'comorbidity_field6152_Hayfever..allergic.rhinitis.or.eczema',
            'comorbidity_20002_angina', 'comorbidity_20002_asthma', 'comorbidity_20002_deep.venous.thrombosis..dvt.',
            'comorbidity_20002_depression', 'comorbidity_20002_diabetes', 'comorbidity_20002_eczema.dermatitis',
            'comorbidity_20002_enlarged.prostate', 'comorbidity_20002_gastro.oesophageal.reflux..gord....gastric.reflux',
            'comorbidity_20002_gout', 'comorbidity_20002_hayfever.allergic.rhinitis', 'comorbidity_20002_heart.attack.myocardial.infarction',
            'comorbidity_20002_hiatus.hernia', 'comorbidity_20002_high.cholesterol', 'comorbidity_20002_hypertension',
            'comorbidity_20002_hypothyroidism.myxoedema', 'comorbidity_20002_migraine', 'comorbidity_20002_osteoarthritis',
            'comorbidity_20002_unclassifiable', 'medication_20003_Aspirin', 'medication_20003_NSAIDS', 'medication_20003_Statins',
            'medication_20003_antioxidants', 'medication_20003_antibiotics', 'medication_20003_ACEI', 'medication_20003_ARB',
            'medication_20003_CCB', 'medication_20003_BetaBlocker', 'medication_20003_diuretics', 'medication_20003_glucosamine',
            'medication_20003_H_Blocker', 'medication_20003_metformin', 'medication_20003_omega_3', 'family_history_Bowel_cancer_num',
            'comorbidity_colon_rectal_neoplasms', 'comorbidity_A_Intestinal_infectious_diseases', 'comorbidity_A10_A99_other_infectious',
            'comorbidity_B_infectious', 'comorbidity_C_D_neoplasms', 'comorbidity_D_Blood_Immune', 'comorbidity_E_Endocrine_thyroid',
            'comorbidity_E_Diabetes', 'comorbidity_E_Other_glucose_disorder', "comorbidity_E_other_endocrine_glands" , "comorbidity_E_Malnutrition" , "comorbidity_E_Obesity"
             , "comorbidity_E_Metabolic_disorders", "comorbidity_F_Mental_disorder", "comorbidity_G_Nervous_system" 
             , "comorbidity_G_Eye_adnexa", "comorbidity_G_Ear", "comorbidity_I_Circ_Chronic_rheumatic_heart"
             , "comorbidity_I_Circ_Hypertensive", "comorbidity_I_Circ_Ischaemic_heart", "comorbidity_I_Pulmonary_heart"
             , "comorbidity_I_other_heart_diseases", "comorbidity_I_Cerebrovascular", "comorbidity_I_Arteries", "comorbidity_I_Veins", "comorbidity_I_Other_Circ"
             , "comorbidity_J_Respiratory", "comorbidity_K_oral", "comorbidity_K_Gastrooesophageal_reflux_disease", "comorbidity_K_Other_oesophagus" 
             , "comorbidity_K_Gastric_ulcer", "comorbidity_K_Duodenal_ulcer", "comorbidity_K_Peptic_ulcer", "comorbidity_K_Gastrojejunal_ulcer"
             , "comorbidity_K_Gastritis_duodenitis", "comorbidity_K_Dyspepsia", "comorbidity_K_other_stomach", "comorbidity_K_Appendix",
            "comorbidity_K_Hernia", "comorbidity_K_51_52_IBD", "comorbidity_K55_Other_intestines", "comorbidity_K56_Other_intestines"  
             , "comorbidity_K57_Other_intestines", "comorbidity_K58_Other_intestines", "comorbidity_K59_Other_intestines" 
             , "comorbidity_K60_Other_intestines", "comorbidity_K61_Other_intestines", "comorbidity_K62_Other_intestines" 
             , "comorbidity_K63_Other_intestines", "comorbidity_K_peritoneum", "comorbidity_K80_Gallbladder_Pancreas"
             , "comorbidity_L_skin", "comorbidity_N_Glomerular_diseases", "comorbidity_N_Renal_tubulo_interstitial"
             , "comorbidity_N_Renal_Failure", "comorbidity_N_Urolithiasis", "comorbidity_N_Other_kidney", "comorbidity_N_Other_urinary"
             , "comorbidity_Q_Congenital", "comorbidity_R0_Other_symptoms", "comorbidity_R1_Other_symptoms", "comorbidity_R3_Other_symptoms"
             , "comorbidity_R5_Other_symptoms", "comorbidity_R6_Other_symptoms", "comorbidity_R10_Other_symptoms", "comorbidity_R31_Other_symptoms"  
             , "comorbidity_R69_Other_symptoms", "comorbidity_T8_Injuries", "comorbidity_T81_Injuries", "comorbidity_Y8_External_causes"
             , "comorbidity_Z0_Personal_Family_history", "comorbidity_Z3_Personal_Family_history", 
             "comorbidity_Z4_Personal_Family_history", "comorbidity_Z5_Personal_Family_history", "comorbidity_Z7_Personal_Family_history", "comorbidity_Z8_Personal_Family_history"  
             , "comorbidity_Z9_Personal_Family_history", "comorbidity_Z51_Personal_Family_history", "comorbidity_Z53_Personal_Family_history"
             , "comorbidity_Z72_Personal_Family_history", "comorbidity_Z85_Personal_Family_history", "comorbidity_Z86_Personal_Family_history"
             , "comorbidity_Z87_Personal_Family_history", "comorbidity_Z88_Personal_Family_history"]
df = pd.DataFrame()
for col in numerical:
    df = df.append(data.pop(col))

numericals = df.T


for col in data.columns:
    data[col] = data[col].astype('category')
    
data[numerical] = numericals




df1 = data.pop('cc_status') # remove column b and store it in df1
df1 = df1.map({0:'Control', 1:'Case'})

data['cc_status']=df1 # add b series as a 'new' column.



data['cc_status'] = data['cc_status'].astype('category')


X_train, X_test, y_train, y_test = train_test_split(data.iloc[:,:-1], data.iloc[:,-1], test_size=0.30, random_state=8)



results = pd.DataFrame(index = ['Decision Tree', 'Random Forest'], 
                       columns = ['accuracy', 'f1', 'precision', 'recall'])



parameters = {'max_depth':range(2,50),'min_samples_leaf':[2,4,6,8,10,12,14,16,18,20,22,24,26,28,30], 
              'criterion' :['gini', 'entropy']}

clf = GridSearchCV(DecisionTreeClassifier(random_state = 8), 
                   parameters, n_jobs=47)
clf.fit(X=X_train, y=y_train)
tree_model = clf.best_estimator_

y_pred = tree_model.predict(X_test)

## Convert strings to ints to work with accuracy score functions
y_test2 = y_test.map({'Control':0, 'Case':1})
y_pred2 = pd.Series(y_pred).map({'Control':0, 'Case':1})

results = get_results(y_test, y_pred, results, 'Decision Tree')




set(y_test) - set(y_pred)

acc = accuracy_score(y_test2, y_pred2)
f1 = f1_score(y_test2, y_pred2)
precision = precision_score(y_test2, y_pred2)
recall = recall_score(y_test2, y_pred2)

print('accuracy = {}, f1 = {}. precision = {}, recall = {}'.format(acc, f1, precision, recall))
print(clf.best_params_)

labels = ['Case', 'Control']
cm = confusion_matrix(y_test, y_pred, labels=labels)
ax= plt.subplot()
sns.heatmap(cm, annot=True, ax = ax)

# labels, title and ticks
ax.set_xlabel('Predicted labels')
ax.set_ylabel('True labels');
ax.set_title('Confusion Matrix');
ax.xaxis.set_ticklabels(['Case', 'Control']); 
ax.yaxis.set_ticklabels(['Case', 'Control'])
plt.savefig("Confusion_matrix_1.jpg")



fig = plt.figure(figsize=(25,20))
_ = tree.plot_tree(tree_model, 
                   feature_names=X_train.columns,  
                   class_names=['Control', 'Case'],
                   filled=True)

fig.savefig("decision_tree_CV.pdf") # Easy to see the graph here



parameters = {'max_features':['sqrt'], 'n_estimators':[250, 500, 1000],
              'max_depth':range(2,40),'min_samples_leaf':range(2,40), 
              'criterion' :['gini', 'entropy']}

rf_class = GridSearchCV(RandomForestClassifier(random_state = 8),
                        parameters, n_jobs=47)
rf_class.fit(X=X_train, y=y_train)
rf_model = rf_class.best_estimator_

y_pred = rf_model.predict(X_test)

## Convert strings to ints to work with accuracy score functions
y_pred2 = pd.Series(y_pred).map({'Control':0, 'Case':1})

results = get_results(y_test, y_pred, results, 'Random Forest')


print('The best parameters are {}'.format(rf_class.best_params_))



set(y_test) - set(y_pred)


features = X_train.columns
features



importances = rf_model.feature_importances_
importances




importances_1 = [x for x in importances if x > 0.01]
importances_1



indices = np.argsort(importances)
indices



plt.figure(figsize= (10,10))
features = X_train.columns
importances = rf_model.feature_importances_
importances_1 = [x for x in importances if x > 0.01]
importances_1 = np.array(importances_1)
indices = np.argsort(importances_1)


plt.title('Feature Importances')
plt.barh(range(len(indices)), importances_1[indices], color='b',
         align='center')
plt.yticks(range(len(indices)), [features[i] for i in indices])
plt.xlabel('Relative Importance')
plt.savefig("Feature_importance.jpg")





result = permutation_importance(rf_model, X_test, y_test, n_repeats=10,
                                random_state=42, n_jobs=47)
sorted_idx = result.importances_mean.argsort()

fig, ax = plt.subplots(figsize = (10,10))
ax.boxplot(result.importances[sorted_idx].T,
           vert=False, labels=X_test.columns[sorted_idx])
ax.set_title("Permutation Importances (test set)")
fig.tight_layout()
plt.savefig("Perm_import.jpg")




explainer = shap.TreeExplainer(rf_model)
shap_values = explainer.shap_values(X_test)

plt.title('SHAP plot for Cases')
shap.summary_plot(shap_values, features = X_test, class_inds = [1])
plt.savefig("Shap_plot.jpg")


plt.title('SHAP Density plot for Cases')
shap.summary_plot(shap_values[1], features = X_test)
plt.savefig("Shap_density.jpg")




labels = ['Case', 'Control']
cm = confusion_matrix(y_test, y_pred, labels=labels)
ax= plt.subplot()
sns.heatmap(cm, annot=True, ax = ax)

# labels, title and ticks
ax.set_xlabel('Predicted labels')
ax.set_ylabel('True labels');
ax.set_title('Confusion Matrix')
ax.xaxis.set_ticklabels(['Case', 'Control']) 
ax.yaxis.set_ticklabels(['Case', 'Control'])
plt.savefig("Confusion_matrix_2.jpg")

results.to_csv('results_full.csv')






