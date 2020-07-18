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

datapath = "data/mushrooms.csv"
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

#NOTE: Als er 2 _ voor een attribuut naam staan in een python class
# dan is het vaak ten zeerste aangeraden om niks met dit attribuut te doen.
# Het word dan als private field/attribuut gezien worden net als in talen als
# C++ , C#, Java.

class NeuralNetwork:
    
    def __init__(self, 
                 datasetPath: str, 
                 epochs: int,
                 batch_size: int,
                 yColName: str):

        dataset = pd.read_csv(datasetPath)
        self.translator = ClassifierTranslator(dataset)
        
        ### Omzetten van alle 'categorische' waarden naar getallen.
        X = dataset.drop(yColName, axis = 1)
        Y = DataFrame(self.translator.TranslateListForward(colname = yColName,
                                                           l = dataset[yColName]))
        for col in X.columns:
            X[col] = self.translator.TranslateListForward(colname = col, 
                                                          l = X[col])
        
        self.__X_train, \
        self.__X_test, \
        self.__Y_train, \
        self.__Y_test = train_test_split(X, Y, test_size=0.33, random_state=42)
        
        ##Hoeveelheid features in X (22)
        classN = len(X.columns)
        
        ##initializeert het model
        self.model = Sequential()
        
        ## Eerste 'hidden' laag met 22 features. In een voorbeeld zag ik 
        ## dat de (hoeveelheid features * 8) de parameter units moest zijn, al kon ik
        ## niet echt teruglezen waarom.
        self.model.add(Dense(classN * 8, 
                             input_dim = classN, 
                             activation='relu'))
        
        ##Tweede hidden laag
        self.model.add(Dense(classN * 4, 
                             activation ='relu'))
        
        ##Laatste sigmoid laag
        self.model.add(Dense(1, 
                             activation='sigmoid'))
        
        self.model.compile(loss = 'binary_crossentropy',
                           optimizer = 'adam',
                           metrics = ['accuracy'])

        self.model.fit(self.__X_train,
                       self.__Y_train,
                       epochs=epochs, 
                       batch_size=batch_size)
        
        _, self.accuracy = self.model.evaluate(self.__X_test, self.__Y_test)
        print('Accuracy: %.2f' % (self.accuracy*100))
        
    def Predict(self, data: DataFrame):
        return self.model.predict(data)

network = NeuralNetwork(datasetPath = datapath, 
                        epochs = 150, 
                        batch_size = 10, 
                        yColName = "class")

##Fitten van het model
##Epochs: een trainings ronde
##batch_size: hoeveel rows er tijdens 1 epoch iteratief worden verwerkt tot het
## het einde van de data is bereikt.

#TODO:
# - Wat doet de parameter van Dense - units überhaupt?
# - Correct bepalen van epochs en batch size

