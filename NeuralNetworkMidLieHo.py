# -*- coding: utf-8 -*-

# -*- coding: utf-8 -*-
"""
Created on Tue Dec  3 11:45:48 2019

@author: Lennard Robert Kras
"""

#conda install ipython pillow tensorflow keras pandas matplotlib scikit-learn seaborn graphviz pydot

import pandas as pd
import numpy as np

from sklearn.model_selection import train_test_split
from pandas                  import DataFrame
from keras.models            import Sequential
from keras.layers            import Dense

from keras import backend

from keras.wrappers.scikit_learn import KerasClassifier

from sklearn.preprocessing import LabelEncoder
from keras.utils import np_utils


from sklearn.model_selection import cross_val_score
from sklearn.model_selection import KFold



from tensorflow.python.client import device_lib
print(device_lib.list_local_devices())


#Nog leuker: gebruik conda install tensorflow-gpu

datapath = "Data/KnnMergedData_3.csv"

def replacecommas(x):
    if(type(x) is str):
        return x.replace(',','.')
    else:
        return x

dataf = pd.read_csv(datapath, encoding = 'ISO-8859-1', sep=";").applymap(replacecommas)
dataf.drop(dataf.columns[0:4],axis=1,inplace=True)

#shrooms = pd.read_csv(datapath)

# Ik liep tegen problemen aan doordat op sommige momenten een kolom allen maar
# numerieke waarde kon bestaan. Daarom wijs ik met behulp van deze class een 
# waarde toe aan elke level van elke feature. Deze kunnen dan heen en weer 
# 'vertaald' worden. In ons geval is het met mid lie hoe niet zo complex (1,2,3)
class ClassifierTranslator:
    def __init__(self, df: DataFrame):
        self.__TranslateDict = dict.fromkeys(df.columns, [])
        for col in df.columns:
            self.__TranslateDict[col] = list(set(df[col]))

    def TranslateValueForward(self, colname: str, valueclass) -> int:
        if colname in self.__TranslateDict:
            try:
                return self.__TranslateDict[colname].index(valueclass)
            except ValueError:
                return -2
        else:
            return -1

    def TranslateValueBackward(self, colname: str, classNumber: int):
        if colname in self.__TranslateDict and \
                len(self.__TranslateDict[colname]) > classNumber:
            try:
                return self.__TranslateDict[colname][classNumber]
            except ValueError:
                return -2
        else:
            return -1

    def TranslateListForward(self, colname: str, l: list) -> list:
        return list(map(lambda x: self.TranslateValueForward(colname, x), l))

    def TranslateListBackward(self, colname: str, l: list) -> list:
        return list(map(lambda x: self.TranslateValueBackward(colname, x), l))

#translator = ClassifierTranslator(shrooms)

##Voorbeelden...
##print(translator.TranslateValueBackward("class", 1))
##print(translator.TranslateListBackward("class", [0,1,2]))
##print(translator.TranslateListForward("class", ["p","e","p"]))


translator = ClassifierTranslator(dataf)

yColName = "Inkomen"

X = dataf.drop(yColName, axis = 1)
Y = dataf[yColName]

classN = len(X.columns)

        
# Floats voor X kunnen blijkbaar gewoon als intput worden gebruikt en hoeven dus niet te worden vertaqld naar levels. Echter kan dit weer niet met de clusters die de mid low high waarden hebben.
# clusters moeten dus wel worden omgezet.
# =============================================================================
for col in X.columns[classN-4:classN]:
    X[col] = translator.TranslateListForward(colname = col, l = X[col])
# =============================================================================

X_train, \
X_test, \
Y_train, \
Y_test = train_test_split(X, Y, test_size=0.33, random_state=42)

def baseline_model():
    model = Sequential()
    model.add(Dense(classN*4, input_dim=classN, activation='relu'))
    model.add(Dense(classN*2, activation='relu'))
    model.add(Dense(3,  activation='softmax')) #Attentie! het aantal units moet hier het aantal classen matchen, dus 3 in geval van low mid high
    model.compile(loss = 'categorical_crossentropy', optimizer = 'adam', metrics = ['accuracy'])
    return model

def dummy_encoder(df):
    encoder = LabelEncoder()
    encoder.fit(df)
    encoded_df = encoder.transform(df)
    dummy_df = np_utils.to_categorical(encoded_df)
    return dummy_df

dummy_y_train = dummy_encoder(Y_train)
dummy_y_test = dummy_encoder(Y_test)

model = baseline_model()

model.fit(x = X_train, y = dummy_y_train , epochs=100, batch_size=10)
_, accuracy = model.evaluate(X_test, dummy_y_test)
print('Accuracy: %.2f' % (accuracy*100))

# =============================================================================
# estimator = KerasClassifier(build_fn=baseline_model, epochs=200, batch_size=5, verbose=0)
# kfold = KFold(n_splits=10, shuffle=True)
# results = cross_val_score(estimator = estimator, X = X, y = dummy_y, cv = kfold)
# =============================================================================
# =============================================================================
# print("Baseline: %.2f%% (%.2f%%)" % (results.mean()*100, results.std()*100))
# =============================================================================

# =============================================================================
# model.fit(X_train, Y_train, epochs=150, batch_size=10)
# 
# 
# _ , accuracy = model.evaluate(X_test, Y_test)
# print('Accuracy: %.2f' % (accuracy*100))
# =============================================================================

