#####################################
# VergeWijk © 2019
# 2.Prepare Data.R
#####################################

PrepareData <- function(PATH) {
  #' @param PATH De plek waar de csv bestanden staan
  
  #----------------------------------------------
  # Deze fucntie importeert alle CSV bestanden van het CBS.
  # De bestanden worden door middel van een loop een voor een
  # ingeladen, opgeschoont en toegevoegd aan een lijst
  # uiteindelijk stuur deze functie een lijst met
  # opgeschoonde Dataframes terug.
  #----------------------------------------------
  
  datalist <- list()
  
  for (i in 1:5) {
    #inladen vd de sets
    datalist[[i]] <- read.csv(file = paste(PATH, "Kerncijfers_wijken_en_buurten_20",i+13,".csv", sep = ""), sep= ";", stringsAsFactors = FALSE, dec=",", check.names=FALSE)
    
    # de rijen 3 en 4 aan elkaar knopen
    datalist[[i]][1,] <- paste (datalist[[i]][3,] , datalist[[i]][4,], sep = " ")
    
    #Een aantal rijen zijn niet nodig door de bewerkingen van de data
    datalist[[i]] <- datalist[[i]][c(1, 5:119),]
    
    #De kolomnamen goedzetten
    colnames(datalist[[i]]) <- datalist[[i]][1,]
    datalist[[i]] <- datalist[[i]][-1,]
    
    #De rijnamen goedzetten (en uniemaken indien nodig)
    rownames(datalist[[i]]) <- make.unique(datalist[[i]][,1])
    #  datalist[[i]] <- datalist[[i]][,-1]   # Bij het vastknopen van de rijen gaat de rijnaam verloren, dus is deze nog nodig
    
    ## Het opschonen van collomnamen.
    colnames(datalist[[i]]) <- gsub("\\|", "", colnames(datalist[[i]]))
    colnames(datalist[[i]]) <- gsub("\\ ", "_", colnames(datalist[[i]]))
    
    #rare tekens die verder in de applicatie voor problemen zorgen vervangen
    colnames(datalist[[i]]) <- gsub("²", "2", colnames(datalist[[i]]))
    colnames(datalist[[i]]) <- gsub("é", "e", colnames(datalist[[i]]))
    colnames(datalist[[i]]) <- gsub(" ", "s", colnames(datalist[[i]]))
    colnames(datalist[[i]]) <- gsub("ë", "e", colnames(datalist[[i]]))
    colnames(datalist[[i]]) <- gsub("³", "3", colnames(datalist[[i]]))
    
    
    
    # Lege velden (.) vullen met de waarde NA
    datalist[[i]][datalist[[i]] == '.'] <- NA
    
    #Decimale getallen bevatten een komma. Deze komma's worden een punt.
    for(j in 1:ncol(datalist[[i]])){
      for(k in 1:nrow(datalist[[i]])){
        datalist[[i]][k,j] <- gsub("\\,", ".", datalist[[i]][k,j])
      }
    }
    
    #Numerieke velden numeric maken
    for(j in 5:ncol(datalist[[i]])){
      datalist[[i]][ ,j] <- as.numeric(datalist[[i]][ ,j])
    }
    
    #de kolommen 9 t/m 24 omrekenen naar absolute waardes
    if (i == 1) {
      for(j in 9:24){
        for(k in 1:nrow(datalist[[i]])){
          datalist[[i]][k,j] <- ((datalist[[i]][k,j] * datalist[[i]][k,6])/100)
        }
      }
    }
    
    # Kollomen met alleen maar NA waardes worden verwijderd, want aan deze data hebben we niks.
    Filter(function(x)!all(is.na(x)), datalist[[i]]) -> datalist[[i]]
    
    #Alle NA values vullen met de gemiddelde waarde die behoort bij de kolom. Dit is gedaan zodat elk veld gevuld is.
    for(k in 5:ncol(datalist[[i]])){
      datalist[[i]][is.na(datalist[[i]][,k]), k] <- as.integer(mean(datalist[[i]][,k], na.rm = TRUE))
    }
    
    #verwijderen van bepaalde column namen 
    ColumnToDrop <- c("Meest_voorkomende_postcode_code")
    datalist[[i]] = datalist[[i]][,!(names(datalist[[i]]) %in% ColumnToDrop)]
    
    #het toevoegen van een jaartal aan een regel
    for(k in 1:nrow(datalist[[i]])){
      datalist[[i]][k,'Jaar'] <- 2000 + i + 13
    }
    
  }
  
  return(datalist)
}
