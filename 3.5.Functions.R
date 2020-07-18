extractBuurten <- function(allesets) {
  
  
  #----------------------------------------------
  # De fucntie extractBuurten neemt als parameter een lijst met Dataframes
  # van de CBS data. In deze Dataframes zitten buurten en wijken. Wij 
  # hebben alleen de buurten nodig. In deze fucntie worden dus de buurten
  # geslecteerd en de data wordt opgeschoont.
  # De fucntie returnt uiteindelijk weer de lijst.
  #----------------------------------------------
  
  
  # De data uit Allsets wordt voor ieder jaar aan een DataFrame gekoppeld.
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
  
  #de buurten van het jaar 2014 komen niet overeen met alle andere jaren.
  #in onderstaande for loop worden de namen van het jaar 2016 gebruikt om zo
  #de namen van 2014 gelijk te maken aan die van de andere jaren.
  for (i in 1:nrow(buurten2014)) {
    xy <- buurten2016[which(buurten2016$Codering_code == buurten2014$Codering_code[i]),]
    buurten2014[i,"_Wijken_en_buurten"] <- xy$`_Wijken_en_buurten`
  }
  
  ## Alle kolommen namen uit buurten2014 wegschrijven naar k.naam 
  k.naam <- colnames(buurten2014)
  
  #Kolomnamen opschonen
  for(j in 1:108) {
    if (j >= 9 & j <= 24) {
      k.naam[j] <- gsub("\\_%", "_aantal", x = colnames(buurten2014[j]))
    }
    else{
      k.naam[j] <- colnames(buurten2014[j])
    }
  }
  
  #Opgeschoonde kolomnamen terugplaatsen.
  colnames(buurten2014) <- k.naam
  
  
  res <- list(buurten2014,buurten2015,buurten2016,buurten2017,buurten2018)
  
  return(res)
  
}

voorspelGdata <- function() {
  
  ##################################################################
  #GEMEENTEDATA VOORSPELLEN
  ##################################################################
  
  
  #----------------------------------------------
  # De functie voorspelGdata is een fucntie die de data van
  # de gemeente Rotterdam inlaad en de missende data voorspelt.
  # De functie is efficient gemaakt doordat die eigenlijk
  # maar één keer hoeft te draaien. Daarna worden de resultaten opgeslagen en
  # deze worden de volgende keer weer hergruikt in plaats van dat ze opnieuw gemaakt worden.
  # De fucntie returnt een lijst met Dataframes.
  #----------------------------------------------
  
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
    
    # Het juiste jaartal toevoegen aan de data
    g14$jaar <- 2014
    g16$jaar <- 2016
    g18$jaar <- 2018
    
    # De dataframes in een list plaatsen
    G_Total <- list()
    G_Total[[1]] <- g14
    G_Total[[2]] <- g16
    G_Total[[3]] <- g18
    
    # De beschikbare gemeentedata mergen tot een dataframe
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
    
    #De numerieke velden koppelen aan de buurtcode + naam
    FinalResult_G <- bind_cols(Code.Naam_g, gtotaal)
    
    # Dataset preparen als input voor het predicten
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
      
      # Het aanmaken van de formule om een voorspelling te kunnen doen.
      test <- as.name(j)
      f <- reformulate(c('`jaar`'," (1|`Sub_BUURT`)"),response=test)
      lmm <- lmer(f, data = trainingData, REML = FALSE)
      
      
      # In iedere itteratie wordt er een predictie gedaan (1 itteratie = 1 buurtcode)
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
    
    
    
    ## Hetzelfde proces als hierboven, maar dan voor 2017
    testData <- FinalResult_G[FinalResult_G$jaar == 2014, ]
    testData$jaar <- 2017
    trainingData <- FinalResult_G
    
    
    
    
    
    for (j in colnames(testData)[5:(ncol(testData)-1)]){
      print(j)
      trainingData_2 <- trainingData[,j]
      testData_2 <- testData[,j]
      
      
      test <- as.name(j)
      f <- reformulate(c('`jaar`'," (1|`Sub_BUURT`)"),response=test)
      lmm <- lmer(f, data = trainingData, REML = FALSE)
      
      
      
      for ( k in unique(trainingData$Sub_BUURT)){
        distPred <- predict(lmm, data.frame(`jaar`= 2017, Sub_BUURT = k), allow.new.levels = FALSE)  # predict distance
        actuals_preds <- data.frame(cbind(actuals=testData$Aantal_inwoners_aantal, predicteds=distPred))  # make actuals_predicteds dataframe.
        testData[testData$Sub_BUURT == k, j] <- actuals_preds#[1,2]
      }
      
    }
    g17 <- testData
    
    # Alle jaren gemeentedata wegschrijven naar CSV bestanden zodat het predicten de volgende keer niet meer hoeft te gebeuren.
    write.csv2(g14, file = "Data/G2014 - 2018/g2014.csv", sep = ";", row.names = FALSE)
    write.csv2(g15, file = "Data/G2014 - 2018/g2015.csv", sep = ";", row.names = FALSE)
    write.csv2(g16, file = "Data/G2014 - 2018/g2016.csv", sep = ";", row.names = FALSE)
    write.csv2(g17, file = "Data/G2014 - 2018/g2017.csv", sep = ";", row.names = FALSE)
    write.csv2(g18, file = "Data/G2014 - 2018/g2018.csv", sep = ";", row.names = FALSE)
    
    
  }
  # Als aan het begin bleek dat de CSV bestanden aanwezig waren, dan worden deze ingelezen en hoeft de rest van de code niet uitgevoerd te worden.
  else {
    g14 <- read.csv(file = "Data/G2014 - 2018/g2014.csv", sep= ";", stringsAsFactors = FALSE, dec=",", check.names=FALSE)
    g15 <- read.csv(file = "Data/G2014 - 2018/g2015.csv", sep= ";", stringsAsFactors = FALSE, dec=",", check.names=FALSE)
    g16 <- read.csv(file = "Data/G2014 - 2018/g2016.csv", sep= ";", stringsAsFactors = FALSE, dec=",", check.names=FALSE)
    g17 <- read.csv(file = "Data/G2014 - 2018/g2017.csv", sep= ";", stringsAsFactors = FALSE, dec=",", check.names=FALSE)
    g18 <- read.csv(file = "Data/G2014 - 2018/g2018.csv", sep= ";", stringsAsFactors = FALSE, dec=",", check.names=FALSE)
  }
  
  # zhet resultaat wordt gemaakt en gereturned
  res <- list(g14,g15,g16,g17,g18)
  return(res)
  
}

