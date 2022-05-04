#!/usr/bin/env python
# coding: utf-8



### loading packages

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import sklearn

from sklearn import tree
from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix
from sklearn.tree import export_graphviz

from sklearn.model_selection import train_test_split
from sklearn.model_selection import GridSearchCV

from sklearn.metrics import accuracy_score
from sklearn.metrics import f1_score
from sklearn.metrics import precision_score
from sklearn.metrics import recall_score
from sklearn.metrics import make_scorer
from sklearn.metrics import plot_confusion_matrix
from sklearn import metrics

from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler
from sklearn import svm
from mlxtend.plotting import plot_decision_regions

from sklearn.metrics import mean_squared_error
from sklearn.metrics import mean_absolute_error

# Load Data
train = pd.read_csv('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/SVM/train_5_year.csv',low_memory=False)
test = pd.read_csv('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/SVM/test_5_year.csv',low_memory=False)

def get_results(truth, prediction, df, idx):
    
    y_test2 = truth.map({'healthy':0, 'case':1})
    y_pred2 = pd.Series(prediction).map({'healthy':0, 'case':1})


    acc = accuracy_score(y_test2, y_pred2)
    f1 = f1_score(y_test2, y_pred2)
    precision = precision_score(y_test2, y_pred2)
    recall = recall_score(y_test2, y_pred2)

    df.loc[idx,:] = [acc, f1, precision, recall]

    return df

results = pd.DataFrame(columns = ['accuracy','f1','precision','recall'])

y_train = train['cc_status'] 
y_train = y_train.astype('category')
y_train = y_train.map({0: 'healthy', 1: 'case'})
y_test = test['cc_status'] 
y_test = y_test.astype('category')
y_test = y_test.map({0: 'healthy', 1: 'case'})
y_train.describe()

X_train = train.iloc[:,2:]
X_test = test.iloc[:,2:]
X_train.isnull().sum()[X_train.isnull().sum()>0]

# Apply linear SVM

# test in a smaller set
X_train_small = X_train.iloc[:,0:23]
X_test_small = X_test.iloc[:,0:23]
X_test_small.shape



## Using small data

svm_class = svm.SVC(kernel = 'linear', class_weight= {'healthy': 1, 'case': 2},
                    gamma='auto', random_state = 8)
svm_class.fit(X_train_small, y_train)

y_pred = svm_class.predict(X_test_small)

labels = ['case', 'healthy']
cm = confusion_matrix(y_test, y_pred, labels=labels)
cm = confusion_matrix(y_test, y_pred)
ax= plt.subplot()
sns.heatmap(cm, annot=True, ax = ax)

# labels, title and ticks
ax.set_xlabel('Predicted labels')
ax.set_ylabel('True labels')
ax.set_title('Confusion Matrix')
ax.xaxis.set_ticklabels(['case', 'healthy']); ax.yaxis.set_ticklabels(['case', 'healthy'])
plt.savefig("Outputs_5year/SVM_linear_small_cm.pdf")

results = get_results(y_test, y_pred, results, 'SVM_small_linear')

print(results)



## Using all data

svm_class = svm.SVC(kernel = 'linear', class_weight= {'healthy': 1, 'case': 2},
                    gamma='auto', random_state = 8,  probability = True)
svm_class.fit(X_train, y_train)

y_pred = svm_class.predict(X_test)

labels = ['case', 'healthy']
cm = confusion_matrix(y_test, y_pred, labels=labels)
cm = confusion_matrix(y_test, y_pred)
ax= plt.subplot()
sns.heatmap(cm, annot=True, ax = ax)

# labels, title and ticks
ax.set_xlabel('Predicted labels')
ax.set_ylabel('True labels')
ax.set_title('Confusion Matrix')
ax.xaxis.set_ticklabels(['case', 'healthy']); ax.yaxis.set_ticklabels(['case', 'healthy'])

plt.savefig("Outputs_5year/SVM_linear_cm.pdf")

results = get_results(y_test, y_pred, results, 'SVM_linear')

print(results)

# get support vectors
print(svm_class.support_vectors_)
# get number of support vectors for each class
print(svm_class.n_support_)

from matplotlib import pyplot as plt
from sklearn import svm

def f_importances(coef, names, top=-1):
    imp = coef
    imp, names = zip(*sorted(list(zip(imp, names))))

    # Show all features
    if top == -1:
        top = len(names)
    
    def pltcolor(lst):
        cols=[]
        for l in lst:
            if l > 0:
                cols.append('indianred')
            elif l < 0:
                cols.append('royalblue')
        return cols
    # Create the colors list using the function above
    cols = pltcolor(coef)

    plt.figure(figsize=(15, 10))
    plt.barh(range(top), imp[::-1][0:top][::-1], align='center',color = cols)
    plt.yticks(range(top), names[::-1][0:top][::-1])
    
    plt.savefig("Outputs_5year/SVM_linear_variable_importance.pdf",bbox_inches = 'tight')


features_names = X_train.columns
f_importances(svm_class.coef_[0], features_names,top=20)
#f_importances(svm_class.coef_[0], features_names,top=20)


from sklearn.metrics import plot_roc_curve
y_test2 = y_test.map({'healthy':0, 'case':1})
y_pred = svm_class.predict_proba(X_test)[:,1]

plot_roc_curve(svm_class,X_test,y_test)
plt.savefig("Outputs_5year/SVM_linear_ROC.pdf")





# Soft margin, non-linear kernel & Tune model
# Linear Model with Soft Margin
y_train2 = y_train.map({'healthy':0, 'case':1})

num_features = np.size(X_train, axis=1)
param_grid =  {'C': [0.01, 0.1, 1,5,10,100], 
   'kernel': ['linear']}
## NOTE: We are includeing C=1 and gamma=1/(num_features * X_train_scaled.var())

optimal_params = GridSearchCV(
    svm.SVC(class_weight= {0: 1, 1: 2}), 
    param_grid,
    cv=5,
    scoring='f1', 
    verbose=0,
    n_jobs = 20
    )

optimal_params.fit(X_train, y_train2)
print(optimal_params.best_params_)

# RBF kernal
y_train2 = y_train.map({'healthy':0, 'case':1})

num_features = np.size(X_train, axis=1)
param_grid = {'C': [0.01, 0.1, 1,5,10,100], 
   'gamma': [1/num_features, 3,2,1, 0.1, 0.01, 0.001], 
   'kernel': ['rbf']}
## NOTE: We are includeing C=1 and gamma=1/(num_features * X_train_scaled.var())

optimal_params = GridSearchCV(
    svm.SVC(class_weight= {0: 1, 1: 2}), 
    param_grid,
    cv=5,
    scoring='f1', 
    verbose=0,
    n_jobs = 20
    )

optimal_params.fit(X_train, y_train2)
print(optimal_params.best_params_)

# Sigmoid Kernal
y_train2 = y_train.map({'healthy':0, 'case':1})

num_features = np.size(X_train, axis=1)
param_grid = {'coef0': [0.1,0.5,1,2,3], 
   'gamma': [1/num_features, 3,2,1, 0.1, 0.01, 0.001], 
   'kernel': ['sigmoid']}

optimal_params = GridSearchCV(
    svm.SVC(class_weight= {0: 1, 1: 2}), 
    param_grid,
    cv=5,
    scoring='f1', 
    verbose=0,
    n_jobs = 20
    )

optimal_params.fit(X_train, y_train2)
print(optimal_params.best_params_)
