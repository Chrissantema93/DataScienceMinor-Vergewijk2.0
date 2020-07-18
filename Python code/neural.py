# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

#conda config
#conda update
conda install pillow
conda install tensorflow
conda install keras
conda install pandas
conda install matplotlib
conda install scikit-learn
conda install seaborn
conda install graphviz
conda install pydot


from __future__ import print_function
import os
data_path = ['../data']


import pandas as pd
import numpy as np

filepath = os.sep.join(data_path + ['output_2.csv'])

data = pd.read_csv(filepath, encoding = 'ISO-8859-1', sep="|")#, names=fields )#, sep=",", header=None)
data.head

import chardet    
rawdata = open(filepath, 'rb').read()
result = chardet.detect(rawdata)
charenc = result['encoding']
print(charenc)




mask = data.dtypes == np.object
categorical_cols = data.columns[mask]
categorical_cols


#categorical_cols.remove(0)
index = [13]
#index = [0,1,13]
categorical_cols = np.delete(categorical_cols, index)

data = data.drop(categorical_cols, axis=1)

data.dtypes.value_counts()



from numpy import loadtxt
from keras.models import Sequential
from keras.layers import Dense
from keras.wrappers.scikit_learn import KerasClassifier
from keras.utils import np_utils
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import KFold
from sklearn.preprocessing import LabelEncoder
from sklearn.pipeline import Pipeline



data.loc[data['Dichtheid_Bevolking'] == 'high', ['Dichtheid_Bevolking']] = 3
data.loc[data['Dichtheid_Bevolking'] == 'mid', ['Dichtheid_Bevolking']] = 2
data.loc[data['Dichtheid_Bevolking'] == 'low', ['Dichtheid_Bevolking']] = 1


# split into input (X) and output (y) variables
X = data.iloc[:,0:429]
y = data.loc[:,'Dichtheid_Bevolking']



def baseline_model():
	# create model
	model = Sequential()
	model.add(Dense(8, input_dim=429, activation='relu'))
	model.add(Dense(3, activation='softmax'))
	# Compile model
	model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])
	return model



estimator = KerasClassifier(build_fn=baseline_model, epochs=200, batch_size=5, verbose=0)
kfold = KFold(n_splits=10, shuffle=True)

results = cross_val_score(estimator, X, y, cv=kfold)
print("Baseline: %.2f%% (%.2f%%)" % (results.mean()*100, results.std()*100))



predictions = baseline_model().predict(X)





https://machinelearningmastery.com/multi-class-classification-tutorial-keras-deep-learning-library/ 









