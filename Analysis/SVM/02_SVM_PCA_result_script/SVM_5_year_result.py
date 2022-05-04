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
train = pd.read_csv('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/SVM/pca_train_5_year.csv',low_memory=False)
test = pd.read_csv('/rds/general/user/wq21/projects/hda_21-22/live/TDS/Group_2/TDS-Group-Project/Analysis/SVM/pca_test_5_year.csv',low_memory=False)

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

svm_class_small = svm.SVC(kernel = 'linear', class_weight= {'healthy': 1, 'case': 2},
                    gamma='auto', random_state = 8)
svm_class_small.fit(X_train_small, y_train)

y_pred_small = svm_class_small.predict(X_test_small)

labels = ['case', 'healthy']
cm = confusion_matrix(y_test, y_pred_small, labels=labels)
cm = confusion_matrix(y_test, y_pred_small)
ax= plt.subplot()
sns.heatmap(cm, annot=True, ax = ax)

# labels, title and ticks
ax.set_xlabel('Predicted labels')
ax.set_ylabel('True labels')
ax.set_title('Confusion Matrix')
ax.xaxis.set_ticklabels(['case', 'healthy']); ax.yaxis.set_ticklabels(['case', 'healthy'])
plt.savefig("Outputs_5year/SVM_linear_small_cm.pdf")
plt.show()

results = get_results(y_test, y_pred_small, results, 'SVM_small_linear')

print(results)




## Using all data

svm_class_linear = svm.SVC(kernel = 'linear', class_weight= {'healthy': 1, 'case': 2},
                    gamma='auto', random_state = 8,  probability = True)
svm_class_linear.fit(X_train, y_train)

y_pred_linear = svm_class_linear.predict(X_test)

labels = ['case', 'healthy']
cm = confusion_matrix(y_test, y_pred_linear, labels=labels)
cm = confusion_matrix(y_test, y_pred_linear)
ax= plt.subplot()
sns.heatmap(cm, annot=True, ax = ax)

# labels, title and ticks
ax.set_xlabel('Predicted labels')
ax.set_ylabel('True labels')
ax.set_title('Confusion Matrix')
ax.xaxis.set_ticklabels(['case', 'healthy']); ax.yaxis.set_ticklabels(['case', 'healthy'])

plt.savefig("Outputs_5year/SVM_linear_cm.pdf")
plt.show()

results = get_results(y_test, y_pred_linear, results, 'SVM_linear')

print(results)

# variable importance

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

    plt.figure(figsize=(10, 10))
    plt.barh(range(top), imp[::-1][0:top][::-1], align='center',color = cols)
    plt.yticks(range(top), names[::-1][0:top][::-1])
    
    plt.savefig("Outputs_5year/SVM_linear_variable_importance.pdf",bbox_inches = 'tight')
    plt.show()


features_names = X_train.columns
f_importances(svm_class_linear.coef_[0], features_names,top=15)
#f_importances(svm_class.coef_[0], features_names,top=20)





# Soft margin, non-linear kernel & Tune model
# Linear Model with Soft Margin
clf_svm_linear = svm.SVC(kernel = 'linear', class_weight= {'healthy': 1, 'case': 2},
                         C=0.01,
                         gamma='auto', random_state = 8,  probability = True)
clf_svm_linear.fit(X_train, y_train)
y_pred_tune_linear = clf_svm_linear.predict(X_test)

plt.figure(figsize=(6.4, 4.8))
labels = ['case', 'healthy']
cm = confusion_matrix(y_test, y_pred_tune_linear, labels=labels)
cm = confusion_matrix(y_test, y_pred_tune_linear)
ax= plt.subplot()
sns.heatmap(cm, annot=True, ax = ax)

# labels, title and ticks
ax.set_xlabel('Predicted labels')
ax.set_ylabel('True labels')
ax.set_title('Confusion Matrix')
ax.xaxis.set_ticklabels(['case', 'healthy']); ax.yaxis.set_ticklabels(['case', 'healthy'])

plt.savefig("Outputs_5year/SVM_linear_tune_cm.pdf")
plt.show()

results = get_results(y_test, y_pred_tune_linear, results, 'SVM_linear_tune')

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

    plt.figure(figsize=(10,10))
    plt.barh(range(top), imp[::-1][0:top][::-1], align='center',color = cols)
    plt.yticks(range(top), names[::-1][0:top][::-1])
    plt.savefig("Outputs_5year/SVM_linear_tune_variable_importance.pdf",bbox_inches = 'tight')
    plt.show()


features_names = X_train.columns
f_importances(clf_svm_linear.coef_[0], features_names,top=15)


# RBF kernal
clf_svm_radial = svm.SVC(kernel='rbf',class_weight= {'healthy': 1, 'case': 2},
                         C=0.2, gamma=0.001,
                         probability=True)

clf_svm_radial.fit(X_train, y_train)
y_pred_radial = clf_svm_radial.predict(X_test)

plt.figure(figsize=(6.4, 4.8))
labels = ['case', 'healthy']
cm = confusion_matrix(y_test, y_pred_radial, labels=labels)
cm = confusion_matrix(y_test, y_pred_radial)
ax= plt.subplot()
sns.heatmap(cm, annot=True, ax = ax)

# labels, title and ticks

ax.set_xlabel('Predicted labels')
ax.set_ylabel('True labels')
ax.set_title('Confusion Matrix')
ax.xaxis.set_ticklabels(['case', 'healthy']); ax.yaxis.set_ticklabels(['case', 'healthy'])

plt.savefig("Outputs_5year/SVM_rbf_tune_cm.pdf")
plt.show()

results = get_results(y_test, y_pred_radial, results, 'SVM_rbf_tune')




# Sigmoid Kernal
clf_svm_sigmoid = svm.SVC(kernel='sigmoid',class_weight= {'healthy': 1, 'case': 2},
                          random_state=1, 
                          coef0 = 4.0, gamma = 0.0005,
                          probability=True)
clf_svm_sigmoid.fit(X_train, y_train)

y_pred_sigmoid = clf_svm_sigmoid.predict(X_test)

plt.figure(figsize=(6.4, 4.8))
labels = ['case', 'healthy']
cm = confusion_matrix(y_test, y_pred_sigmoid, labels=labels)
cm = confusion_matrix(y_test, y_pred_sigmoid)
ax= plt.subplot()
sns.heatmap(cm, annot=True, ax = ax)

# labels, title and ticks
ax.set_xlabel('Predicted labels')
ax.set_ylabel('True labels')
ax.set_title('Confusion Matrix')
ax.xaxis.set_ticklabels(['case', 'healthy']); ax.yaxis.set_ticklabels(['case', 'healthy'])

plt.savefig("Outputs_5year/SVM_sigmoid_tune_cm.pdf")
plt.show()

results = get_results(y_test, y_pred_sigmoid, results, 'SVM_sigmoid_tune')

print(results)
results.to_csv('Outputs_5year/results.csv')



# ROC plt
# ROC curve linear

from sklearn.metrics import plot_roc_curve

plot_roc_curve(svm_class_small,X_test_small,y_test)
plt.savefig("Outputs_5year/SVM_linear_small_ROC.pdf")
plt.show()

plot_roc_curve(svm_class_linear,X_test,y_test)
plt.savefig("Outputs_5year/SVM_linear_ROC.pdf")
plt.show()


plot_roc_curve(clf_svm_linear,X_test,y_test)
plt.savefig("Outputs_5year/SVM_linear_tune_ROC.pdf")
plt.show()


plot_roc_curve(clf_svm_radial,X_test,y_test)
plt.savefig("Outputs_5year/SVM_rbf_tune_ROC.pdf")
plt.show()

plot_roc_curve(clf_svm_sigmoid,X_test,y_test)
plt.savefig("Outputs_5year/SVM_sigmoid_tune_ROC.pdf")
plt.show()

