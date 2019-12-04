# -*- coding: utf-8 -*-
"""
Created on Tue Dec  3 11:45:48 2019

@author: Lennard Robert Kras
"""

#conda install ipython pillow tensorflow keras pandas matplotlib scikit-learn seaborn graphviz pydot

import pandas as pd
from pandas import DataFrame
from keras.models import Sequential
from keras.layers import Dense

datapath = "data/mushrooms.csv"
shrooms = pd.read_csv(datapath)

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

translator = ClassifierTranslator(shrooms)

#Voorbeelden...
#print(translator.TranslateValueBackward("class", 1))
#print(translator.TranslateListBackward("class", [0,1,2]))
#print(translator.TranslateListForward("class", ["p","e","p"]))

# Modeleren van het model
## Omzetten van alle 'categorische' waarden naar getallen.
X = shrooms.drop("class", axis = 1)
Y = DataFrame(translator.TranslateListForward("class",shrooms["class"]))
for col in X.columns:
    X[col] = translator.TranslateListForward(col, X[col])


#initializeert het model
model = Sequential() 

#Hoeveelheid features in X (22)
classN = len(X.columns) 

# Eerste 'hidden' laag met 22 features. In een voorbeeld zag ik 
# dat de (hoeveelheid features * 8) de parameter units moest zijn, al kon ik
# niet echt teruglezen waarom.
model.add(Dense(classN * 8, input_dim= classN, activation="relu")) 

#Tweede hidden laag
model.add(Dense(classN * 4, activation="relu")) 

#Laatste sigmoid laag
model.add(Dense(1, activation="sigmoid")) 

model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy']) 

#Fitten van het model
#Epoch: een trainings ronde
#batch_size: Een hoeveelheid rows die tijdens 1 epoch worden verwerkt. Dat wil zeggen: 
#Dus: Hoe meer epochs, hoe meer je model traint. Hoe grote de batch size, hoe meer er tijdens 1 epoch behandeld word.
model.fit(X,Y,epochs=150, batch_size=10)

_, accuracy = model.evaluate(X, Y)
print('Accuracy: %.2f' % (accuracy*100))


#TODO:
# - Wat doet de parameter van Dense - units überhaupt?
# - Correct bepalen van epochs en batch size
