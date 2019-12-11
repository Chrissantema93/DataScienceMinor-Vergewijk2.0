###################

#deze package heb je nodig en moeten we later nog even in 1.import verwerken
#install.packages("DMwR")
library(DMwR)

?write.csv

# heel gedeelte hieronder overgenomen uit 4.opsch.. ff zodat ik in dit bestand kon werken

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
buurten2015 <- buurten2015[which(buurten2015$Codering_code %in% buurten2018$Codering_code), ] 
buurten2017 <- buurten2017[which(buurten2017$Codering_code %in% buurten2018$Codering_code), ] 

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


colnames(cbstotaal)[colSums(is.na(cbstotaal)) > (nrow(cbstotaal) / 2)]


#get all colnames met NA's, meer dan 1/5e van de column leeg.
coltodel <- colnames(cbstotaal)[colSums(is.na(cbstotaal)) > (nrow(cbstotaal) / 2)]
cbstotaal <- cbstotaal[ , !(names(cbstotaal) %in% coltodel)]

#?knnImputation

knnOutput <- knnImputation(cbstotaal, k = 10, scale = T, meth = "weighAvg")  # perform knn imputation.
anyNA(knnOutput)
#> FALSE


FinalResult <- bind_cols(Code.Naam,knnOutput)

write.csv2(FinalResult, file = "Data/KnnMergedData.csv", sep = ";", row.names = FALSE)


###################






















