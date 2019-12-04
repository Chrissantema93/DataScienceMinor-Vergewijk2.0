# -*- coding: utf-8 -*-
"""
Created on Wed Dec  4 11:45:36 2019

@author: anton
"""

from __future__ import print_function
import os

import pandas as pd
from pandas                  import DataFrame
import numpy as np
from keras.layers import Dense
from keras.models import Sequential
from sklearn.model_selection import train_test_split




## Zet hier eerst het pad goed 
data_path = ['..//GitHub//Vergewijk//Data']
#data_path = ['C://Users//anton//Documents//GitHub//Vergewijk//Data']

filepath = os.sep.join(data_path + ['output_2.csv'])

data = pd.read_csv(filepath, encoding = 'ISO-8859-1', sep="|")#, names=fields )#, sep=",", header=None)
#data.head
data = data.fillna(0)

#import chardet    
#rawdata = open(filepath, 'rb').read()
#result = chardet.detect(rawdata)
#charenc = result['encoding']
#print(charenc)



## Het ophalen van alle colomen die als datatype object hebben
mask = data.dtypes == np.object
categorical_cols = data.columns[mask]
categorical_cols

# Op index 13 zit de waarde die we willen voorspellen. De rest wordt weggegeooid
#categorical_cols.remove(0)
index = [13]
#index = [0,1,13]
categorical_cols = np.delete(categorical_cols, index)

data = data.drop(categorical_cols, axis=1)

#data.dtypes.value_counts()

#How mid low omzetten naar getallen tussen 0 en 1
data.loc[data['Dichtheid_Bevolking'] == 'high', ['Dichtheid_Bevolking']] = 0.9
data.loc[data['Dichtheid_Bevolking'] == 'mid', ['Dichtheid_Bevolking']] = 0.5
data.loc[data['Dichtheid_Bevolking'] == 'low', ['Dichtheid_Bevolking']] = 0.2

# test en training data
X = data.copy()
X = X.drop(columns=['Dichtheid_Bevolking'])#.values
y = data["Dichtheid_Bevolking"]#.values

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.33, random_state=42)


#X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3)

#prediction_values = test.values

#def baseline_model():
# create model
model = Sequential()
model.add(Dense(500, input_dim=429, activation='relu'))
model.add(Dense(2000, activation='softmax'))
model.add(Dense(500, activation='softmax'))
model.add(Dense(150, activation='softmax'))
model.add(Dense(25, activation='softmax'))
model.add(Dense(1, activation='softmax'))
# Compile model
model.compile(loss='sparse_categorical_crossentropy', optimizer='sgd', metrics=['accuracy'])
#return model

model.fit(X_train, y_train, validation_data=(X_test, y_test), epochs=300, batch_size=10)

predictions = model.predict(X)
predictions


#estimator = KerasClassifier(build_fn=baseline_model, epochs=200, batch_size=5, verbose=0)
#kfold = KFold(n_splits=10, shuffle=True)

#results = cross_val_score(estimator, X, y, cv=kfold)
#print("Baseline: %.2f%% (%.2f%%)" % (results.mean()*100, results.std()*100))



