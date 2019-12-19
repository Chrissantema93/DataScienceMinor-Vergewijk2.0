#####################################
# VergeWijk © 2019
# 4. Opschonen van alle datasets.R
#####################################

# De functie stopt in Allsets (Large list) vijf lijsten met voor ieder jaar de CBS data.
allsets <- PrepareData("Data/Cbs/")

temp_buurten <- extractBuurten(allsets)

# De data per jaar wordt weggeschreven naar een eigen variabele
CBSbuurten2014 <- temp_buurten[[1]]
CBSbuurten2015 <- temp_buurten[[2]]
CBSbuurten2016 <- temp_buurten[[3]]
CBSbuurten2017 <- temp_buurten[[4]]
CBSbuurten2018 <- temp_buurten[[5]]


#Alle CBS data wordt onder elkaar gezet (in een data.frame)
#btotaal <- bind_rows(buurten2018, buurten2017, buurten2016, buurten2015, buurten2014)  


temp_jaren <- voorspelGdata()

g2014 <- temp_jaren[[1]]
g2015 <- temp_jaren[[2]]
g2016 <- temp_jaren[[3]]
g2017 <- temp_jaren[[4]]
g2018 <- temp_jaren[[5]]


buurten2014 <- merge(x = CBSbuurten2014, y = g2014, by = "Codering_code")
buurten2015 <- merge(x = CBSbuurten2015, y = g2015, by = "Codering_code") 
buurten2016 <- merge(x = CBSbuurten2016, y = g2016, by = "Codering_code")
buurten2017 <- merge(x = CBSbuurten2017, y = g2017, by = "Codering_code")
buurten2018 <- merge(x = CBSbuurten2018, y = g2018, by = "Codering_code")


buurten2016$`Si_X..bewoners.dat.bekend.is.met.het.Steunpunt.Mantelzorg`<- as.numeric(buurten2016$`Si_X..bewoners.dat.bekend.is.met.het.Steunpunt.Mantelzorg`)
buurten2018$`Si_X..bewoners.dat.bekend.is.met.het.Steunpunt.Mantelzorg`<- as.numeric(buurten2018$`Si_X..bewoners.dat.bekend.is.met.het.Steunpunt.Mantelzorg`)



if (file.exists(file = "Data/2019.csv")) {
  Buurten_Totaal <- read.csv(file = "Data/2019.csv", sep= ";", stringsAsFactors = FALSE, dec=",", check.names=FALSE)
} else {
  cbstotaal <- bind_rows(buurten2014, buurten2015, buurten2016, buurten2017, buurten2018)
  # 
  # #bewaar coderingcode en buurtnaam, deze gaan we na het opvullen van waardes weer terug plakken
  Code.Naam <- cbstotaal[c('Codering_code','_Wijken_en_buurten')]
  # 
  # #aleen numerieke waardes
  nums <- unlist(lapply(cbstotaal, is.numeric))
  cbstotaal <- cbstotaal[,nums]
  
  # #Alle NA values vullen met de gemiddelde waarde die behoort bij de kolom. Dit is gedaan zodat elk veld gevuld is.
  for(k in 1:ncol(cbstotaal)){
    cbstotaal[is.na(cbstotaal[,k]), k] <- as.integer(mean(cbstotaal[,k], na.rm = TRUE))
  }
  # 
  # 
  FinalResult <- bind_cols(Code.Naam, cbstotaal)
  # 
  # 
  # 
  # 
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
  # 
  # 
  # #bewaar coderingcode en buurtnaam, deze gaan we na het opvullen van waardes weer terug plakken
  Code.Naam <- FinalResult_2[c('Codering_code','_Wijken_en_buurten')]
  # 
  #aleen numerieke waardes
  nums <- unlist(lapply(FinalResult_2, is.numeric))
  FinalResult_2 <- FinalResult_2[,nums]
  # 
  knnOutput <- knnImputation(FinalResult_2, k = 10, scale = T, meth = "weighAvg")  # perform knn imputation.
  anyNA(knnOutput)
  # 
  # 
  Buurten_Totaal <- bind_cols(Code.Naam, knnOutput)
  Buurten_Totaal[is.na(Buurten_Totaal$`_Wijken_en_buurten`), "_Wijken_en_buurten"] <- "'s Gravenland"
}


# 
# #####################################################################


buurten2018 <- Buurten_Totaal[which(Buurten_Totaal$jaar == 2018),]
buurten2017 <- Buurten_Totaal[which(Buurten_Totaal$jaar == 2017),]
buurten2016 <- Buurten_Totaal[which(Buurten_Totaal$jaar == 2016),]
buurten2015 <- Buurten_Totaal[which(Buurten_Totaal$jaar == 2015),]
buurten2014 <- Buurten_Totaal[which(Buurten_Totaal$jaar == 2014),]

btotaal <- bind_rows(buurten2018, buurten2017, buurten2016, buurten2015, buurten2014)  
#rm(list = c("g2018", "g2016", "g2014"))



#bind_rows(buurten2018, buurten2017, buurten2016, buurten2015, buurten2014) -> btotaal  

poly.g <- geojsonio::geojson_read("Data/Polygons/Rotterdam Wijken_2.geojson", what = "sp")


#merge de data van cbs2018 met de geo data van 2018
# buurtcode en Codering_code zijn bijde kolommen met de code van wijk, hierop worden ze gemerged 
Buurt.Pol2018 <- merge(x = poly.g, y = buurten2018, by.x = "buurtcode", by.y = "Codering_code", all.y = TRUE)
Buurt.Pol2017 <- merge(x = poly.g, y = buurten2017, by.x = "buurtcode", by.y = "Codering_code", all.y = TRUE)
Buurt.Pol2016 <- merge(x = poly.g, y = buurten2016, by.x = "buurtcode", by.y = "Codering_code", all.y = TRUE)
Buurt.Pol2015 <- merge(x = poly.g, y = buurten2015, by.x = "buurtcode", by.y = "Codering_code", all.y = TRUE)
Buurt.Pol2014 <- merge(x = poly.g, y = buurten2014, by.x = "buurtcode", by.y = "Codering_code", all.y = TRUE)


#rm(list = c("buurten2018", "buurten2017", "buurten2016", "buurten2015", "buurten2014"))
#rm(list = c("BuurtEnPoly2018", "BuurtEnPoly2017", "BuurtEnPoly2016", "BuurtEnPoly2015", "BuurtEnPoly2014"))

#Zit nu in Shiny
#lijst met inwoners van alleen de buurten (wijken zijn eruit gefilterd)
#InwonersPerWijk <- states@data$Aantal_inwoners_aantal[which(states@data$Soort_regio_omschrijving == "Buurt")]

#als eerst vul ik de lijst op met ??n 0, omdat de lijst 93 lang moet zijn en niet 92
#dan doe ik alles + 1 omdat de helekaart grijs wordt als er ??n 0 in de vector zit
#daarna alles X50 zodat het wat beter schaalt, de kleuren gaan van 10 tot 1.000.000 en de normale
#waardes zitten tussen de 0 en ~20.000 ofzo dus dan heb je weinig kleurverschil

FixLength <- function (Vect,Size) {
  #gebruik: vult de vector op tot de gewenste lengte.
  #         dus Fixlength(C(4,7,9), 7) wordt: C(4,7,9,0,0,0,0)
  
  # Vect: een vector
  # Size: De lengte die die lijst moet worden
  
  Leng <- length(Vect)
  
  if (Leng <= (Size - 1)) {
    Difference <- Size - Leng
    for (i in Difference) {
      Vect <- c(Vect,0)
    }
  }
  return(Vect)
}

#geen idee wat dit is maar het doet wat 
pal <- colorNumeric("viridis", NULL, reverse = TRUE)

#for(i in 7:ncol(b2018)){
#  test <- mean(b2018[b2018$Soort_regio_omschrijving == 'Buurt' ,i])
#  for(j in 1:nrow(b2018)){
#    b2018[j, i] <- b2018[j, i] / test
#  }
#}

#b2018 <- b2018[b2018$Soort_regio_omschrijving == 'buurt' ,]

# De centroids worden berekend
centroids <- getSpPPolygonsLabptSlots(Buurt.Pol2018)
## In cents worden de midelpunten van de polygonen opgeslagen. Deze worden gebruikt om een marker te kunnen plotten
cents <- SpatialPointsDataFrame(coords=centroids, data=Buurt.Pol2018@data, proj4string=CRS("+proj=longlat +ellps=clrk66"))
# De coordinaten worden uit de polygonenopslage gekoppieerd naar het dataframe.
cents@data$Coord1 <-cents@coords[,1]
cents@data$Coord2 <-cents@coords[,2]





