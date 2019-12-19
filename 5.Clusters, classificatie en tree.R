#####################################
# VergeWijk © 2019
# 5.Clusters, classificatie en tree.R
#####################################

#####################################
#Prepareren van de data voor tree
#####################################
descision.test <- Buurten_Totaal#[Buurten_Totaal$Soort_regio_omschrijving == 'Buurt' ,]
descision.test <- descision.test[which(descision.test$Codering_code %in% buurten2018$Codering_code), ]
descision.test[is.na(descision.test)] <- 0 ##!!!! <- Iets anders voor verzinnen. Zomaar een 0 plaatsen heeft grote invloed op de uitkomst.

#Maakt van elke waarde in een kolom de z-score.
ZScoreColumn <- function(col) {
  pop_sd <- sd(col)
  pop_mean <- mean(col)
  
  sapply(col, function(value){
    (value - pop_mean) / pop_sd
  })
}

#Toepassen van de z-score functie
for(i in 7:ncol(descision.test)){
  descision.test[i] <- sapply(descision.test[i], ZScoreColumn)
}

#van elke bedachte 'cluster' een gemiddelde z-score maken.
descision.test$Cultuur_recreatie   <- rowMeans(select(descision.test,"R-U_Cultuur,_recreatie,_overige_diensten_aantal", "H+J_Vervoer,_informatie_en_communicatie_aantal", "G+I_Handel_en_horeca_aantal", "Bedrijfsvestigingen_totaal_aantal"))
descision.test$Dichtheid_Bevolking <- rowMeans(select(descision.test,"Bevolkingsdichtheid_aantal_inwoners_per_km2", "Omgevingsadressendichtheid_per_km2", "Personenauto's_naar_oppervlakte_per_km2", "Percentage_meergezinswoning_%", "Mate_van_stedelijkheid_code"))
descision.test$Parkeergelegenheid  <- rowMeans(select(descision.test,"Motortweewielers_aantal", "Personenauto's_totaal_aantal", "Personenauto's;_6_jaar_en_ouder_aantal", "Personenauto's;_jonger_dan_6_jaar_aantal"))
descision.test$mean_Woning_grootte <- rowMeans(select(descision.test,"Hoekwoning_m3", "Hoekwoning_m3", "Huurwoning_m3", "Koopwoning_m3", "Tussenwoning_m3", "Vrijstaande_woning_m3", "Appartement_m3"))
descision.test$Woning_grootte      <- rowMeans(select(descision.test,"mean_Woning_grootte", "Gemiddelde_woningwaarde_x_1_000_euro", "Bouwjaar_voor_2000_%"))
descision.test$Inkomen             <- rowMeans(descision.test[,c("Gemiddeld_inkomen_per_inwoners_x_1_000_euro", "20%_personen_met_hoogste_inkomen_%", "Gemiddelde_woningwaarde_x_1_000_euro", "20%_huishoudens_met_hoogste_inkomen_%")] )
descision.test$Wijkgrootte         <- rowMeans(descision.test[,c("Oppervlakte_land_ha", "Oppervlakte_totaal_ha")] )
descision.test$Onderhoud_omgeving  <- rowMeans(descision.test[,c("Sub_X..voldoende.aanwezig.gebruiksgroen..picknick..sporten..spelen.", "Sub_X..tevreden.over.onderhoud.stoepen", "Sub_Milieu.subjectief", "Sub_X..voldoende.aanwezig.groen..grasveldjes..bomen.")] )
descision.test$Openbaar_vervoer    <- rowMeans(descision.test[,c("Obj_aantal.tramhaltes", "Obj_X..woningen.met.bushaltes.binnen.normafstand", "Obj_aantal.metrostations", "Obj_aantal.bushaltes")] )
descision.test$Ontwikkeling        <- rowMeans(descision.test[,c("Si_X..werkende.jongeren..18.t.m.22.jr.", "Si_X..bewoners..23.t.m.64.jr..met.werk", "Si_X..bewoners..18.jr.en.ouder..dat.nog.maar.kort.in.Nederland.woont", "Si_X..bewoners.dat.zegt.dat.de.omgang.tussen.etnische.groepen.in.de.buurt.goed.is")] )



#Nieuwe kolommen in een andere set plaatsen
Test.set <- descision.test %>% select( Codering_code, Jaar_ , Cultuur_recreatie, Dichtheid_Bevolking, Parkeergelegenheid, Woning_grootte, Inkomen, Wijkgrootte, Onderhoud_omgeving, Openbaar_vervoer, Ontwikkeling) 

#####################################
#Density plots van de data
#####################################
plotDensity <- function(featureName) {
  return(ggplot(Test.set, aes_string(featureName))  +
    geom_histogram(binwidth = 0.1, aes(y=..density..), fill="yellow") +
    geom_density(fill="red", alpha=.2))
}

plot1 <- plotDensity("Dichtheid_Bevolking")
plot2 <- plotDensity("Cultuur_recreatie")
plot3 <- plotDensity("Parkeergelegenheid")
plot4 <- plotDensity("Woning_grootte")
plot5 <- plotDensity("Inkomen")
plot6 <- plotDensity("Wijkgrootte")
plot7 <- plotDensity("Onderhoud_omgeving")
plot8 <- plotDensity("Openbaar_vervoer")
plot9 <- plotDensity("Ontwikkeling")



grid.arrange(plot1, plot2 , plot3, plot4) 

#####################################
#Catagoriën toewijzen
#####################################
curateDataSet <- function(dataset) {
  # curateColumn <- function(col) {
  #   cSd <- sd(col)
  #   cMean <- mean(col)
  #   
  #   high <- qnorm(0.7, mean = cMean, sd= cSd)
  #   low  <- qnorm(0.3, mean = cMean, sd= cSd)
  #   curateValue <- function(value){
  #     if(value <= low){
  #       return("low")
  #     }
  #     else if((value > low) && (value < high)){
  #       return("mid")
  #     }
  #     else{
  #       return("high")
  #     }
  #   }
  #   
  
    curateColumn <- function(col) {
      cSd <- sd(col)
      cMean <- mean(col)
      high_p <- qnorm(0.8, mean = cMean, sd= cSd)
      high <- qnorm(0.6, mean = cMean, sd= cSd)
      low  <- qnorm(0.4, mean = cMean, sd= cSd)
      low_m  <- qnorm(0.2, mean = cMean, sd= cSd)
      

      curateValue <- function(value){
        
          if(value <= low_m){
            return("low_m")
          }
          else if((value > low_m) && (value < low)){
            return("low")
          }
          else if((value > low) && (value < high)){
            return("mid")
          }
         
            else if((value > high) && (value < high_p)){
              return("high")
            }
            else{
              return("high_p")
            }
          }
   
    sapply(col, curateValue)
  }
  
  res <- sapply(dataset[-c(1,2)], curateColumn)
  res <- cbind(dataset[c(1,2)], res)
  colnames(res) <- colnames(dataset)
  return(res)
}

Test.CuratedSet <- curateDataSet(Test.set)


#Jaar veranderen Jaar_
Test.CuratedSet <-
Test.CuratedSet %>% 
  rename(
    Jaar = Jaar_
    )


#####################################
#Decision tree + voorbeeld predictie
#####################################
CreateDicisionForumla <- function(xTarget, yFeatures) {
  backtick <- function(x) {
    paste("`", x, "`", sep="")
  }
  paste(xTarget, " ~ ", paste(sapply(yFeatures, backtick), collapse=" + "),  sep="")
}

create_train_test <- function(data, size = 0.7, train = TRUE) {
  total_row = size * nrow(data)
  train_sample <-  1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

Test.CuratedSet[] <- lapply(Test.CuratedSet, factor)

train <- create_train_test(Test.CuratedSet, size = 1) # Met een size van 1 traint het eigenlijk niet.. de norm is 0.7
train_test <- create_train_test(Test.CuratedSet, size = 1, train = FALSE)
colnames(train)[1] <- "Codering_code" #weghalen van laagstreepje. Dit kan niet in een formule.

firstTwoColIndex <- -c(1,2)

xTarg <- "Codering_code"
yFeat <- colnames(Test.set)[firstTwoColIndex]
target <- CreateDicisionForumla(xTarg, yFeat)


fit <- rpart(target, data = train, method = "class", cp = 0.000001)
draw.tree(fit, print.levels = TRUE)

# predicter <- function(tree, prediction){
#   nCols <- 4
#   if(length(prediction) > nCols && length(prediction) < nCols){
#     stop("prediction moet een vector zijn van precies 4 factors, te kiezen uit low, mid en high")
#   }
#   
#   df <- data.frame(matrix(ncol = nCols, nrow = 0))
#   res <- rbind(df, prediction)
#   colnames(res) <- colnames(Test.set)[-c(1,2)]
#   
#   print(predict(tree, res ,type="prob", na.action = "na.exclude"))
# }

predicter <- function(tree, prediction) {
  yNCols <- length(yFeat)
  pNCols <- length(prediction)
  
  if(yNCols != pNCols) {
    stop(paste("Parameter 'prediction' bevat niet alle clusters. Moet gelijk zijn aan een lengte van: ", yNCols, ". Huidige lengte: ", pNCols))
  }
  
  res <-  rbind(data.frame(matrix(ncol = pNCols, nrow = 0)), prediction)
  colnames(res) <- yFeat
  return(predict(tree, res, type="prob", na.action = "na.exclude"))
}


caret::confusionMatrix(data=predict(fit, type="class"), reference=train$Codering_code)
fit$variable.importance

# predicter(fit, c("high", "high", "high", "high", "high", "high", "high", "high", "high")) 
# ff <- predict(fit, train_test, type="prob")
