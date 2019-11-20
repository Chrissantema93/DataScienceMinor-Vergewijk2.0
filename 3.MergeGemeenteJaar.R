#####################################
# VergeWijk © 2019
# 3.MergeGemeenteJaar.R
#####################################

MergeGemeenteJaar <- function(PATH,year) {
  
  # ophalen alle categorien van dit jaar  
  g_obj <- read.csv(file = paste(PATH, "gemeente ",year," fi_obj.csv", sep="")  , sep = ';',stringsAsFactors = FALSE, encoding = "UTF-8")
  g_sub <- read.csv(file = paste(PATH, "gemeente ",year," fi_subj.csv", sep="") , sep = ';',stringsAsFactors = FALSE, encoding="UTF-8")
  g_si  <- read.csv(file = paste(PATH, "gemeente ",year," si.csv", sep="") , sep = ';',stringsAsFactors = FALSE, encoding="UTF-8")
  g_fi  <- read.csv(file = paste(PATH, "gemeente ",year," vi.csv", sep="") , sep = ';',stringsAsFactors = FALSE, encoding="UTF-8")
  
  # colnames uniekmaken omdat er veel dezelfde tussen zitten
  colnames(g_obj) <- paste("Obj", colnames(g_obj), sep = "_")
  colnames(g_sub) <- paste("Sub", colnames(g_sub), sep = "_")
  colnames(g_si)  <- paste("Si", colnames(g_si), sep = "_")
  colnames(g_fi)  <- paste("Fi", colnames(g_fi), sep = "_")
  
  # alles naar ??n dataset mergen
  merged_g <- merge(
    x = merge(x = g_sub, y = g_obj, by.x = "Sub_wijknr", by.y = "Obj_wijknr"), 
    y = merge(x = g_si, y = g_fi, by.x = "Si_wijknr", by.y = "Fi_wijknr"), 
    by.x = "Sub_wijknr", 
    by.y = "Si_wijknr")
  
  # verwijderen van wijken, industrie terreinen, havens, ect
  merged_g <- merged_g[-which(merged_g$Sub_wijknr %in% c(4001,4007,4012,4011,4008,4005,4003,4002,4010,4006,5000,4000,4013)),]
  
  #verwijder alle procent tekens van de waardes af
  for(j in 1:ncol(merged_g)){
    for(k in 1:nrow(merged_g)){
      merged_g[k,j] <- gsub("%", "", merged_g[k,j])
      merged_g[k,j] <- gsub(",", ".", merged_g[k,j])
    }
  }
  
  #ophalen kolommen die omgezet kunnen worden naar numeric
  testRow <- merged_g[2,]
  
  NumericColumns <- c()
  
  for (i in 1:ncol(testRow)) {
    if (!is.na(as.numeric(testRow[1,i]))) {
      NumericColumns[i] <- i
    }
  }
  
  NumericColumns <- NumericColumns[!is.na(NumericColumns)]
  
  #kolommen omzetten naar numeric
  for (colnum in NumericColumns) {
    merged_g[,colnum] <- as.numeric(merged_g[,colnum])
  }
  
  #grootijsselmonde mergen
  ijselbuurten <- merged_g[which(merged_g$Sub_BUURT %in% c("Groot IJsselmonde-Noord","Groot IJsselmonde-Zuid")),]
  
  #aller mergen in de bovenste
  for (i in NumericColumns) {
    ijselbuurten[1,i] <- ((ijselbuurten[1,i] + ijselbuurten[2,i]) / 2)
  }
  
  #naam goed zetten
  ijselbuurten$Sub_BUURT <- "Groot IJsselmonde"
  
  #ijsselmond noord en zuid verwijderen
  merged_g <- merged_g[-which(merged_g$Sub_BUURT %in% c("Groot IJsselmonde-Noord","Groot IJsselmonde-Zuid")),]
  
  #ijsselmond (merged) terug zetten
  merged_g <- dplyr::bind_rows(merged_g, ijselbuurten[1,])
  
  #haal alle te dupliceren wijken op
  rowsToDup <- merged_g[which(grepl("/",merged_g$Sub_BUURT)),]
  
  #tellen hoeveelheid keer verdubbelen, dit cre?ert een lijst met lijsten met daarin elke naam apart
  dupRowNames <- as.list(strsplit(rowsToDup$Sub_BUURT, "/"))
  
  #verdubbel process + rijen namen fixen
  duplicatedRows <- c()
  
  for (nameslist in dupRowNames) {
    mergedname <- ""
    
    #merge namen terug bij elkaar voor matchen
    for (k in 1:length(nameslist)) {
      if (k == 1) {
        mergedname = nameslist[[k]]
      }
      else {
        mergedname <- paste(mergedname, nameslist[[k]], sep = "/")
      }
    }
    
    #selecteer de juiste row voor deze interatie van de loop
    selectedrow <- rowsToDup[which(rowsToDup$Sub_BUURT == mergedname),]
    
    for (name in nameslist) {
      
      #ff een temprow zodat 't origineel niet aangetast wordt
      temprow <- selectedrow
      temprow[1,"Sub_BUURT"] <- name
      
      #gooi de rij terug in een dataframe
      duplicatedRows <- dplyr::bind_rows(duplicatedRows,temprow)
      
    }
  }
  
  #rijen verwijderen uit originele
  merged_g <- merged_g[-which(merged_g$Sub_BUURT %in% rowsToDup$Sub_BUURT),]
  
  #verdubbelde rijen terug erin gooien met nieuwe juiste namen
  merged_g <- dplyr::bind_rows(merged_g, duplicatedRows)
  
  #toevoegen van de buurt codes om later te kunnen linkeden met cbs data
  #lege column toevoegen waar de waardes in komen
  merged_g[,"Codering_code"] <- NA
  
  buurtcodes <- c('BU05991086','BU05990515','BU05990531','BU05991290','BU05990532','BU05990558','BU05991081','BU05990321','BU05991572',
                  'BU05991570','BU05990112','BU05990113','BU05990845','BU05990320','BU05991702','BU05991703','BU05991087','BU05991289',
                  'BU05991593','BU05991449','BU05990662','BU05990661','BU05991082','BU05991692','BU05991699','BU05991085','BU05990451',
                  'BU05991017','BU05991079','BU05990841','BU05990842','BU05990843','BU05991446','BU05990534','BU05991284','BU05990325',
                  'BU05990665','BU05991468','BU05990836','BU05990118','BU05990119','BU05990324','BU05991088','BU05990455','BU05990454',
                  'BU05990452','BU05991463','BU05991467','BU05991574','BU05990837','BU05991283','BU05990535','BU05990111','BU05990327',
                  'BU05990328','BU05990456','BU05991577','BU05991391','BU05991448','BU05990516','BU05992704','BU05990814','BU05991444',
                  'BU05990660','BU05990329','BU05990323','BU05990110','BU05991701','BU05990847','BU05991571','BU05990664','BU05990322',
                  'BU05991080','BU05991575','BU05991466','BU05991578','BU05991576','BU05991573')
  
  namen <-      c('Afrikaanderwijk', 'Agniesebuurt','Bergpolder','Beverwaard','Blijdorp','Blijdorpsepolder','Bloemhof','Bospolder','Carnisse',
                  'Charlois','Cool','CS-kwartier','De Esch','Delfshaven','Dorp','Rijnpoort','Feijenoord','Groot IJsselmonde',
                  'Heijplaat','Het Lage Land','Hillegersberg-noord','Hillegersberg-zuid','Hillesluis','Hoogvliet-noord','Hoogvliet-zuid','Katendrecht','Kleinpolder',
                  'Kop van Zuid','Kop van Zuid-Entrepot','Kralingen-west','Kralingen Oost','Kralingse Bos', 'Dijkzigt','Nieuwe Westen','Noordereiland','Zestienhoven',
                  'Kralingseveer','Liskwartier','Lombardijen','Middelland','Molenlaankwartier','Nesselande','Nieuw Crooswijk','Nieuwe Werk',
                  'Schieveen','NoordKethel','Ommoord','Oosterflank','Oud-Charlois','Oud Crooswijk','Oud IJsselmonde','Oude Noorden','Oude Westen',
                  'OudMathenesse','Witte Dorp','Overschie','Pendrecht','Pernis', 'Prinsenland','Provenierswijk','Rozenburg','Rubroek',
                  's-Gravenland','Schiebroek','Schiemond','Spangen','Stadsdriehoek', 'Strand en duin','Struisenburg','Tarwewijk','Terbregge',
                  'Tussendijken','Vreewijk','Wielewaal','Zevenkamp','Zuiderpark en Zuidrand','Zuidplein','Zuidwijk')
  
  names(buurtcodes) <- namen
  
  
  #toevoegen van de codes
  for (i in 1:nrow(merged_g)) {
    buurtcode <- buurtcodes[merged_g[i,]$Sub_BUURT]
    if(!is.na(buurtcode)){
      merged_g[i,'Codering_code'] <- unname(buurtcode)
    }
  }
  return(merged_g)
}
