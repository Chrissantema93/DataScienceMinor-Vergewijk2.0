

#################################################################
      GEMEENTEDATA VOORSPELLEN
#################################################################




g2014$jaar <- 2014
g2016$jaar <- 2016
g2018$jaar <- 2018


G_Total <- list()
G_Total[[1]] <- g2014
G_Total[[2]] <- g2016
G_Total[[3]] <- g2018


gtotaal <- bind_rows(G_Total[[1]],G_Total[[2]],G_Total[[3]])


  
#gtotaal$Sub_BUURT

#bewaar coderingcode en buurtnaam, deze gaan we na het opvullen van waardes weer terug plakken
Code.Naam_g <- gtotaal[c('Sub_naam.gebied..voormalig.deelgemeente.','Sub_BUURT', 'Codering_code')]

#aleen numerieke waardes
nums <- unlist(lapply(gtotaal, is.numeric))
gtotaal <- gtotaal[,nums]

#Alle NA values vullen met de gemiddelde waarde die behoort bij de kolom. Dit is gedaan zodat elk veld gevuld is.
  for(k in 1:ncol(gtotaal)){
    gtotaal[is.na(gtotaal[,k]), k] <- as.integer(mean(gtotaal[,k], na.rm = TRUE))
  }

FinalResult_G <- bind_cols(Code.Naam_g, gtotaal)


testData <- FinalResult_G[FinalResult_G$jaar == 2014, ]
testData$jaar <- 2015
trainingData <- FinalResult_G





for (j in colnames(testData)[5:(ncol(testData)-1)]){
  print(j)
  #(ncol(testData)-1)
  # trainingData_2 <- trainingData[trainingData$Codering_code == i,]
  trainingData_2 <- trainingData[,j]
  # testData_2 <- testData[testData$Codering_code == i,]
  testData_2 <- testData[,j] 
  

  test <- as.name(j)
  f <- reformulate(c('`jaar`'," (1|`Sub_BUURT`)"),response=test)
  lmm <- lmer(f, data = trainingData, REML = FALSE)
  
 
  
  for ( k in unique(trainingData$Sub_BUURT)){
  #  print(k)
    #testData[k,j] <- NA
    distPred <- predict(lmm, data.frame(`jaar`= 2015, Sub_BUURT = k), allow.new.levels = FALSE)  # predict distance     
    #print(distPred)
    actuals_preds <- data.frame(cbind(actuals=testData$Aantal_inwoners_aantal, predicteds=distPred))  # make actuals_predicteds dataframe.
    # print(actuals_preds[2])
    testData[testData$Sub_BUURT == k, j] <- actuals_preds#[1,2]  
  }

}

 g2015 <- testData
 
 
 
 
 testData <- FinalResult_G[FinalResult_G$jaar == 2014, ]
 testData$jaar <- 2017
 trainingData <- FinalResult_G
 
 
 
 
 
 for (j in colnames(testData)[5:(ncol(testData)-1)]){
   print(j)
   #(ncol(testData)-1)
   # trainingData_2 <- trainingData[trainingData$Codering_code == i,]
   trainingData_2 <- trainingData[,j]
   # testData_2 <- testData[testData$Codering_code == i,]
   testData_2 <- testData[,j] 
   
   
   test <- as.name(j)
   f <- reformulate(c('`jaar`'," (1|`Sub_BUURT`)"),response=test)
   lmm <- lmer(f, data = trainingData, REML = FALSE)
   
   
   
   for ( k in unique(trainingData$Sub_BUURT)){
     #  print(k)
     #testData[k,j] <- NA
     distPred <- predict(lmm, data.frame(`jaar`= 2017, Sub_BUURT = k), allow.new.levels = FALSE)  # predict distance     
     #print(distPred)
     actuals_preds <- data.frame(cbind(actuals=testData$Aantal_inwoners_aantal, predicteds=distPred))  # make actuals_predicteds dataframe.
     # print(actuals_preds[2])
     testData[testData$Sub_BUURT == k, j] <- actuals_preds#[1,2]  
   }
   
 }
 g2017 <- testData
 
 



#################################################################
                2019 VOORSPELLEN
#################################################################









buurten2014 <- allsets[[1]]
buurten2015 <- allsets[[2]]
buurten2016 <- allsets[[3]]
buurten2017 <- allsets[[4]]
buurten2018 <- allsets[[5]]


#filter alle wijken uit de dataset
buurten2018 <- buurten2018[which(buurten2018$Soort_regio_omschrijving == "Buurt"), ]
buurten2017 <- buurten2017[which(buurten2017$Soort_regio_omschrijving == "Buurt"), ]
buurten2016 <- buurten2016[which(buurten2016$Soort_regio_omschrijving == "Buurt"), ]
buurten2015 <- buurten2015[which(buurten2015$Soort_regio_omschrijving == "Buurt"), ]
buurten2014 <- buurten2014[which(buurten2014$Soort_regio_omschrijving == "Buurt"), ]

for (i in 1:nrow(buurten2014)) {
  xy <- buurten2016[which(buurten2016$Codering_code == buurten2014$Codering_code[i]),]
  buurten2014[i,"_Wijken_en_buurten"] <- xy$`_Wijken_en_buurten`
}

## Alle kolommen namen uit buurten2014 wegschrijven naar k.naam 
k.naam <- colnames(buurten2014)

for(j in 1:108) {
  if (j >= 9 & j <= 24) {
    k.naam[j] <- gsub("\\_%", "_aantal", x = colnames(buurten2014[j]))
  }
  else{
    k.naam[j] <- colnames(buurten2014[j])
  }
}

colnames(buurten2014) <- k.naam


buurten2014 <- merge(x = buurten2014, y = g2014, by = "Codering_code")
buurten2016 <- merge(x = buurten2016, y = g2016, by = "Codering_code")
buurten2018 <- merge(x = buurten2018, y = g2018, by = "Codering_code")
buurten2015 <- merge(x = buurten2015, y = g2015, by = "Codering_code") 
buurten2017 <- merge(x = buurten2017, y = g2017, by = "Codering_code")

buurten2016$`Si_X..bewoners.dat.bekend.is.met.het.Steunpunt.Mantelzorg`<- as.numeric(buurten2016$`Si_X..bewoners.dat.bekend.is.met.het.Steunpunt.Mantelzorg`)
buurten2018$`Si_X..bewoners.dat.bekend.is.met.het.Steunpunt.Mantelzorg`<- as.numeric(buurten2018$`Si_X..bewoners.dat.bekend.is.met.het.Steunpunt.Mantelzorg`)


# vanaf hier begint mijn code

CBS_Total <- list()
CBS_Total[[1]] <- buurten2014
CBS_Total[[2]] <- buurten2015
CBS_Total[[3]] <- buurten2016
CBS_Total[[4]] <- buurten2017
CBS_Total[[5]] <- buurten2018

cbstotaal <- bind_rows(CBS_Total[[1]],CBS_Total[[2]],CBS_Total[[3]],CBS_Total[[4]],CBS_Total[[5]])

#bewaar coderingcode en buurtnaam, deze gaan we na het opvullen van waardes weer terug plakken
Code.Naam <- cbstotaal[c('Codering_code','_Wijken_en_buurten')]

#aleen numerieke waardes
nums <- unlist(lapply(cbstotaal, is.numeric))
cbstotaal <- cbstotaal[,nums]

#Alle NA values vullen met de gemiddelde waarde die behoort bij de kolom. Dit is gedaan zodat elk veld gevuld is.
for(k in 1:ncol(cbstotaal)){
  cbstotaal[is.na(cbstotaal[,k]), k] <- as.integer(mean(cbstotaal[,k], na.rm = TRUE))
}


FinalResult <- bind_cols(Code.Naam, cbstotaal)




testData <- FinalResult[FinalResult$jaar == 2014,]

#testData <- buurten2014[,]
testData$Jaar_ <- 2019
trainingData <- FinalResult[FinalResult$Jaar_ %in% c(2014, 2015, 2016, 2017, 2018), ]  # model training data
trainingData <- trainingData[which(trainingData$Codering_code %in% buurten2016$Codering_code), ]


#for ( i in unique(trainingData$Codering_code)){

for (j in colnames(testData)[5:ncol(testData)]){
  print(j)
  # trainingData_2 <- trainingData[trainingData$Codering_code == i,]
  trainingData_2 <- trainingData[,j]
  # testData_2 <- testData[testData$Codering_code == i,]
  testData_2 <- testData[,j] 
  
  #testData_2[,j] <- NA
  test <- as.name(j)
  f <- reformulate(c('`Jaar_`'," (1|`Codering_code`)"),response=test)
  lmm <- lmer(f, data = trainingData, REML = FALSE)
  
  # ?reformulate
  
  for ( k in unique(trainingData$Codering_code)){
    
    #testData[k,j] <- NA
    distPred <- predict(lmm, data.frame(`Jaar_`= 2019, Codering_code = k), allow.new.levels = FALSE)  # predict distance     
    #print(distPred)
    actuals_preds <- data.frame(cbind(actuals=testData$Aantal_inwoners_aantal, predicteds=distPred))  # make actuals_predicteds dataframe.
    # print(actuals_preds[2])
    testData[testData$Codering_code == k, j] <- floor(actuals_preds[1,2])  
  }
  #lmMod <- lm(reformulate(termlabels = 'Jaar_', response = test) , data=trainingData_2)  # build the model
  
  #reformulate(termlabels = Jaar_, response = j)
  
}
#}

hoi <- testData


testData[testData <0] <- NA




FinalResult_2 <- bind_rows(FinalResult, testData)


#bewaar coderingcode en buurtnaam, deze gaan we na het opvullen van waardes weer terug plakken
Code.Naam <- FinalResult_2[c('Codering_code','_Wijken_en_buurten')]

#aleen numerieke waardes
nums <- unlist(lapply(FinalResult_2, is.numeric))
FinalResult_2 <- FinalResult_2[,nums]

knnOutput <- knnImputation(FinalResult_2, k = 10, scale = T, meth = "weighAvg")  # perform knn imputation.
anyNA(knnOutput)


FinalResult_2 <- bind_cols(Code.Naam, knnOutput)


write.csv2(FinalResult_2, file = "Data/2019.csv", sep = ";", row.names = FALSE)
  
  
  
  
  
  
  
  

  
  
  
  
  
  

    