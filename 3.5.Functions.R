extractBuurten <- function(allesets) {
  buurten2014 <- allesets[[1]]
  buurten2015 <- allesets[[2]]
  buurten2016 <- allesets[[3]]
  buurten2017 <- allesets[[4]]
  buurten2018 <- allesets[[5]]
  
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
  
  
  res <- list(buurten2014,buurten2015,buurten2016,buurten2017,buurten2018)
  
  return(res)
  
}

voorspelGdata <- function() {
  
  ##################################################################
  #GEMEENTEDATA VOORSPELLEN
  ##################################################################
  
  #IF een van de files niet aanwezig is... doe dan het hele proces opnieuw
  if (
    !file.exists("Data/G2014 - 2018/g2014.csv") ||
    !file.exists("Data/G2014 - 2018/g2015.csv") ||
    !file.exists("Data/G2014 - 2018/g2016.csv") ||
    !file.exists("Data/G2014 - 2018/g2017.csv") ||
    !file.exists("Data/G2014 - 2018/g2018.csv")
  ) {
    g14 <- MergeGemeenteJaar("Data/Gemeente/",2014)
    g16 <- MergeGemeenteJaar("Data/Gemeente/",2016)
    g18 <- MergeGemeenteJaar("Data/Gemeente/",2018)
    
    
    g14$jaar <- 2014
    g16$jaar <- 2016
    g18$jaar <- 2018
    
    
    G_Total <- list()
    G_Total[[1]] <- g14
    G_Total[[2]] <- g16
    G_Total[[3]] <- g18
    
    
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
    
    g15 <- testData
    
    
    
    
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
    g17 <- testData
    
    write.csv2(g14, file = "Data/G2014 - 2018/g2014.csv", sep = ";", row.names = FALSE)
    write.csv2(g15, file = "Data/G2014 - 2018/g2015.csv", sep = ";", row.names = FALSE)
    write.csv2(g16, file = "Data/G2014 - 2018/g2016.csv", sep = ";", row.names = FALSE)
    write.csv2(g17, file = "Data/G2014 - 2018/g2017.csv", sep = ";", row.names = FALSE)
    write.csv2(g18, file = "Data/G2014 - 2018/g2018.csv", sep = ";", row.names = FALSE)
    
    
  }
  else {
    g14 <- read.csv(file = "Data/G2014 - 2018/g2014.csv", sep= ";", stringsAsFactors = FALSE, dec=",", check.names=FALSE)
    g15 <- read.csv(file = "Data/G2014 - 2018/g2015.csv", sep= ";", stringsAsFactors = FALSE, dec=",", check.names=FALSE)
    g16 <- read.csv(file = "Data/G2014 - 2018/g2016.csv", sep= ";", stringsAsFactors = FALSE, dec=",", check.names=FALSE)
    g17 <- read.csv(file = "Data/G2014 - 2018/g2017.csv", sep= ";", stringsAsFactors = FALSE, dec=",", check.names=FALSE)
    g18 <- read.csv(file = "Data/G2014 - 2018/g2018.csv", sep= ";", stringsAsFactors = FALSE, dec=",", check.names=FALSE)
  }
  
  
  res <- list(g14,g15,g16,g17,g18)
  return(res)
  
}

