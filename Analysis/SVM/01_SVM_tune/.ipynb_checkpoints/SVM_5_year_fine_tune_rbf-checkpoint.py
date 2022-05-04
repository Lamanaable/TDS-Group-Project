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

y_train = train['cc_status'] 
y_train = y_train.astype('category')
y_train = y_train.map({0: 'healthy', 1: 'case'})
y_test = test['cc_status'] 
y_test = y_test.astype('category')
y_test = y_test.map({0: 'healthy', 1: 'case'})
y_train.describe()

X_train = train.iloc[:,2:]
X_test = test.iloc[:,2:]







# RBF kernal
y_train2 = y_train.map({'healthy':0, 'case':1})

num_features = np.size(X_train, axis=1)
param_grid = {'C': [0.005,0.01,0.02,0.04], 
   'gamma': list(np.arange(0.001, 0.01, 0.001)), 
   'kernel': ['rbf']}

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
