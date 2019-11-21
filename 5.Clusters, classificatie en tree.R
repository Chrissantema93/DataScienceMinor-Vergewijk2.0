#####################################
# VergeWijk © 2019
# 5.Clusters, classificatie en tree.R
#####################################

#####################################
#Prepareren van de data voor tree
#####################################
descision.test <- btotaal[btotaal$Soort_regio_omschrijving == 'Buurt' ,]
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


#Nieuwe kolommen in een andere set plaatsen
Test.set <- descision.test %>% select( Codering_code, Jaar_ , Cultuur_recreatie, Dichtheid_Bevolking, Parkeergelegenheid, Woning_grootte, Inkomen) 

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

grid.arrange(plot1, plot2 , plot3, plot4) 

#####################################
#Catagoriën toewijzen
#####################################
curateDataSet <- function(dataset) {
  curateColumn <- function(col) {
    cSd <- sd(col)
    cMean <- mean(col)
    
    high <- qnorm(0.7, mean = cMean, sd= cSd)
    low  <- qnorm(0.3, mean = cMean, sd= cSd)
    curateValue <- function(value){
      if(value <= low){
        return("low")
      }
      else if((value > low) && (value < high)){
        return("mid")
      }
      else{
        return("high")
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

train <- create_train_test(Test.CuratedSet, size = 1) # Met een size van 1 traint het eigenlijk niet.. de norm is 0.7
train_test <- create_train_test(Test.CuratedSet, size = 1, train = FALSE)
colnames(train)[1] <- "Codering_code" #weghalen van laagstreepje. Dit kan niet in een formule.

xTarg <- "Codering_code"
yFeat <- colnames(Test.set)[-c(1,2)]

target <- CreateDicisionForumla(xTarg, yFeat)


fit <- rpart::rpart(target, data = train, method = "class")

rpart.plot::prp(fit, yesno = TRUE, nn = TRUE ,
                 legend.y = FALSE, 
                faclen = 0, varlen=0, 
                cex = 0.7)


predicter <- function(tree, prediction){
  nCols <- 4
  if(length(prediction) > nCols && length(prediction) < nCols){
    stop("prediction moet een vector zijn van precies 4 factors, te kiezen uit low, mid en high")
  }
  
  df <- data.frame(matrix(ncol = nCols, nrow = 0))
  res <- rbind(df, prediction)
  colnames(res) <- colnames(Test.set)[-c(1,2)]
  
  print(predict(tree, res ,type="prob", na.action = "na.exclude"))
}















#randomforest
trans <- train
trans[] <- lapply(train, factor)

caret::confusionMatrix(data = predict(fit, type = "class"), reference = )
randomFit <- randomForest(as.formula(target), trans, ntree=1500,mtry=5)
randomFit
# heeft een error rate van 76.15%... veel te hoog. Eventueel oplossen met meer features.
#validation/pruning

crossValidate <- function(tree, formula, traindata, reference, lowcp){
  confusion <- caret::confusionMatrix(data = predict(tree, type = "class"), reference = reference)
  if(confusion$overall["Accuracy"] == 1) {
    return(tree)
  }
  
  
  lowcpTree <- rpart::rpart(formula, traindata, method = "class", cp = lowcp, model = TRUE)
  lowestXerrorI <- which.min(lowcpTree$cptable[, "xerror"])
  bestcp <- round(lowcpTree$cptable[lowestXerrorI, "CP"],4)
  return(rpart::prune(lowcpTree, cp = bestcp))
}

newtree <- crossValidate(fit, target, trans, trans$Codering_code, 0.000000001)
#accuray is met 0.03 verhoogt... woehoe.

rpart::printcp(newtree)
#Nog even onderzoeken hoe de accuracy van beide tree kan worden verbeterd. Dit schiet niet op zo nml.

