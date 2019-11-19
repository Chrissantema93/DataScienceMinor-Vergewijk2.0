# Voordat dit script gerund kan worden moeten eerst de volgende scripts gerund worden:
# - Prepare data.R        ## Functie om het dataset van het CBS in te laden
# - MergeGemeenteJaar. R  ## Functie om de data van Rotterdam in te laden, en te mergen met CBS data


# De functie stopt in Allsets (Large list) vijf lijsten met voor ieder jaar de CBS data.
allsets <- PrepareData("Data/Cbs/")

# De data per jaar wordt weggeschreven naar een eigen variabele
buurten2014 <- allsets[[1]]
buurten2015 <- allsets[[2]]
buurten2016 <- allsets[[3]]
buurten2017 <- allsets[[4]]
buurten2018 <- allsets[[5]]

#Alle CBS data wordt onder elkaar gezet (in een data.frame)
bind_rows(buurten2018, buurten2017, buurten2016, buurten2015, buurten2014) -> btotaal  

g2014 <- MergeGemeenteJaar("Data/Gemeente/",2014)
g2016 <- MergeGemeenteJaar("Data/Gemeente/",2016)
g2018 <- MergeGemeenteJaar("Data/Gemeente/",2018)

buurten2014 <- merge(x = buurten2014, y = g2014, by = "Codering_code")
buurten2016 <- merge(x = buurten2016, y = g2016, by = "Codering_code")
buurten2018 <- merge(x = buurten2018, y = g2018, by = "Codering_code")
buurten2015 <- buurten2017[which(buurten2018$Codering_code %in% buurten2018$Codering_code), ]
buurten2017 <- buurten2017[which(buurten2018$Codering_code %in% buurten2018$Codering_code), ]

rm(list = c("g2018", "g2016", "g2014"))

#library('dplyr')

## Alle kolommen namen uit buurten2014 wegschrijven naar k.naam 
k.naam <- colnames(buurten2014)

# 
  for(j in 1:108){
    if(j >= 9 & j<= 24){ 
      k.naam[j] <- gsub("\\_%", "_aantal", x = colnames(buurten2014[j]))
        }
  else{
    k.naam[j] <- colnames(buurten2014[j])
    }
  }


colnames(buurten2014) <- k.naam

#bind_rows(buurten2018, buurten2017, buurten2016, buurten2015, buurten2014) -> btotaal  




#install.packages("geojsonio")
#library("geojsonio")
poly.g <- geojsonio::geojson_read("Data/Polygons/Rotterdam Wijken_2.geojson", what = "sp")


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

#merge de data van cbs2018 met de geo data van 2018
# buurtcode en Codering_code zijn bijde kolommen met de code van wijk, hierop worden ze gemerged 
Buurt.Pol2018 <- merge(x = poly.g, y = buurten2018, by.x = "buurtcode", by.y = "Codering_code", all.y = TRUE)
Buurt.Pol2017 <- merge(x = poly.g, y = buurten2017, by.x = "buurtcode", by.y = "Codering_code", all.y = TRUE)
Buurt.Pol2016 <- merge(x = poly.g, y = buurten2016, by.x = "buurtcode", by.y = "Codering_code", all.y = TRUE)
Buurt.Pol2015 <- merge(x = poly.g, y = buurten2015, by.x = "buurtcode", by.y = "Codering_code", all.y = TRUE)
Buurt.Pol2014 <- merge(x = poly.g, y = buurten2014, by.x = "buurtcode", by.y = "Codering_code", all.y = TRUE)

rownames(buurten2018) <- make.unique(buurten2018[,2])

#rm(list = c("buurten2018", "buurten2017", "buurten2016", "buurten2015", "buurten2014"))
#rm(list = c("BuurtEnPoly2018", "BuurtEnPoly2017", "BuurtEnPoly2016", "BuurtEnPoly2015", "BuurtEnPoly2014"))


#Zit nu in Shiny
#lijst met inwoners van alleen de buurten (wijken zijn eruit gefilterd)
#InwonersPerWijk <- states@data$Aantal_inwoners_aantal[which(states@data$Soort_regio_omschrijving == "Buurt")]


#als eerst vul ik de lijst op met één 0, omdat de lijst 93 lang moet zijn en niet 92
#dan doe ik alles + 1 omdat de helekaart grijs wordt als er één 0 in de vector zit
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


#library('shiny')
#library('leaflet')
#library('dplyr')

#geen idee wat dit is maar het doet wat 
pal <- colorNumeric("viridis", NULL)


#for(i in 7:ncol(b2018)){
#  test <- mean(b2018[b2018$Soort_regio_omschrijving == 'Buurt' ,i])
#  for(j in 1:nrow(b2018)){
#    b2018[j, i] <- b2018[j, i] / test
#  }
#}

#b2018 <- b2018[b2018$Soort_regio_omschrijving == 'buurt' ,]



shinyApp(ui = ui1, server = server1)        









