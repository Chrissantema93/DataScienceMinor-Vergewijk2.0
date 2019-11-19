MergeGemeenteJaar <- function(PATH,year) {
  
  # ophalen alle categorien van dit jaar  
  g_obj <- read.csv(file = paste(PATH, "gemeente ",year," fi_obj.csv", sep="")  , sep = ';',stringsAsFactors = FALSE)
  g_sub <- read.csv(file = paste(PATH, "gemeente ",year," fi_subj.csv", sep="") , sep = ';',stringsAsFactors = FALSE)
  g_si <- read.csv(file = paste(PATH, "gemeente ",year," si.csv", sep="") , sep = ';',stringsAsFactors = FALSE)
  g_fi <- read.csv(file = paste(PATH, "gemeente ",year," vi.csv", sep="") , sep = ';',stringsAsFactors = FALSE)
  
  # colnames uniekmaken omdat er veel dezelfde tussen zitten
  colnames(g_obj) <- paste("Obj", colnames(g_obj), sep = "_")
  colnames(g_sub) <- paste("Sub", colnames(g_sub), sep = "_")
  colnames(g_si) <- paste("Si", colnames(g_si), sep = "_")
  colnames(g_fi) <- paste("Fi", colnames(g_fi), sep = "_")
  
  # alles naar één dataset mergen
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
  merged_g <- bind_rows(merged_g, ijselbuurten[1,])
  
  
  
  
  
  #haal alle te dupliceren wijken op
  rowsToDup <- merged_g[which(grepl("/",merged_g$Sub_BUURT)),]
  
  #tellen hoeveelheid keer verdubbelen, dit creëert een lijst met lijsten met daarin elke naam apart
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
      duplicatedRows <- bind_rows(duplicatedRows,temprow)
      
    }
  }
  
  #rijen verwijderen uit originele
  merged_g <- merged_g[-which(merged_g$Sub_BUURT %in% rowsToDup$Sub_BUURT),]
  
  #verdubbelde rijen terug erin gooien met nieuwe juiste namen
  merged_g <- bind_rows(merged_g, duplicatedRows)
  
  
  
  #toevoegen van de buurt codes om later te kunnen linkeden met cbs data
  #lege column toevoegen waar de waardes in komen
  merged_g[,"Codering_code"] <- NA
  
  #toevoegen van de codes
  for (i in 1:nrow(merged_g)) {
    if (merged_g[i,]$Sub_BUURT == 'Afrikaanderwijk') { merged_g[i,'Codering_code'] <- 'BU05991086' }
    else if (merged_g[i,]$Sub_BUURT == 'Agniesebuurt') { merged_g[i,'Codering_code'] <- 'BU05990515' }
    else if (merged_g[i,]$Sub_BUURT == 'Bergpolder') { merged_g[i,'Codering_code'] <- 'BU05990531' }
    else if (merged_g[i,]$Sub_BUURT == 'Beverwaard') { merged_g[i,'Codering_code'] <- 'BU05991290' }
    else if (merged_g[i,]$Sub_BUURT == 'Blijdorp') { merged_g[i,'Codering_code'] <- 'BU05990532' }
    else if (merged_g[i,]$Sub_BUURT == 'Blijdorpsepolder') { merged_g[i,'Codering_code'] <- 'BU05990558' }
    else if (merged_g[i,]$Sub_BUURT == 'Bloemhof') { merged_g[i,'Codering_code'] <- 'BU05991081' }
    else if (merged_g[i,]$Sub_BUURT == 'Bospolder') { merged_g[i,'Codering_code'] <- 'BU05990321' }
    else if (merged_g[i,]$Sub_BUURT == 'Carnisse') { merged_g[i,'Codering_code'] <- 'BU05991572' }
    else if (merged_g[i,]$Sub_BUURT == 'Charlois') { merged_g[i,'Codering_code'] <- 'BU05991570' }
    else if (merged_g[i,]$Sub_BUURT == 'Cool') { merged_g[i,'Codering_code'] <- 'BU05990112' }
    else if (merged_g[i,]$Sub_BUURT == 'CS-kwartier') { merged_g[i,'Codering_code'] <- 'BU05990113' }
    else if (merged_g[i,]$Sub_BUURT == 'De Esch') { merged_g[i,'Codering_code'] <- 'BU05990845' }
    else if (merged_g[i,]$Sub_BUURT == 'Delfshaven') { merged_g[i,'Codering_code'] <- 'BU05990320' }
    else if (merged_g[i,]$Sub_BUURT == 'Dorp') { merged_g[i,'Codering_code'] <- 'BU05991702' }
    else if (merged_g[i,]$Sub_BUURT == 'Rijnpoort') { merged_g[i,'Codering_code'] <- 'BU05991703' }
    else if (merged_g[i,]$Sub_BUURT == 'Feijenoord') { merged_g[i,'Codering_code'] <- 'BU05991087' }
    else if (merged_g[i,]$Sub_BUURT == 'Groot IJsselmonde') { merged_g[i,'Codering_code'] <- 'BU05991289' }
    else if (merged_g[i,]$Sub_BUURT == 'Heijplaat') { merged_g[i,'Codering_code'] <- 'BU05991593' }
    else if (merged_g[i,]$Sub_BUURT == 'Het Lage Land') { merged_g[i,'Codering_code'] <- 'BU05991449' }
    else if (merged_g[i,]$Sub_BUURT == 'Hillegersberg-noord') { merged_g[i,'Codering_code'] <- 'BU05990662' }
    else if (merged_g[i,]$Sub_BUURT == 'Hillegersberg-zuid') { merged_g[i,'Codering_code'] <- 'BU05990661' }
    else if (merged_g[i,]$Sub_BUURT == 'Hillesluis') { merged_g[i,'Codering_code'] <- 'BU05991082' }
    else if (merged_g[i,]$Sub_BUURT == 'Hoogvliet-noord') { merged_g[i,'Codering_code'] <- 'BU05991692' }
    else if (merged_g[i,]$Sub_BUURT == 'Hoogvliet-zuid') { merged_g[i,'Codering_code'] <- 'BU05991699' }
    else if (merged_g[i,]$Sub_BUURT == 'Katendrecht') { merged_g[i,'Codering_code'] <- 'BU05991085' }
    else if (merged_g[i,]$Sub_BUURT == 'Kleinpolder') { merged_g[i,'Codering_code'] <- 'BU05990451' }
    else if (merged_g[i,]$Sub_BUURT == 'Kop van Zuid') { merged_g[i,'Codering_code'] <- 'BU05991017' }
    else if (merged_g[i,]$Sub_BUURT == 'Kop van Zuid-Entrepot') { merged_g[i,'Codering_code'] <- 'BU05991079' }
    else if (merged_g[i,]$Sub_BUURT == 'Kralingen-west') { merged_g[i,'Codering_code'] <- 'BU05990841' }
    else if (merged_g[i,]$Sub_BUURT == 'Kralingen Oost') { merged_g[i,'Codering_code'] <- 'BU05990842' }
    else if (merged_g[i,]$Sub_BUURT == 'Kralingse Bos') { merged_g[i,'Codering_code'] <- 'BU05990843' }
    else if (merged_g[i,]$Sub_BUURT == 'Kralingseveer') { merged_g[i,'Codering_code'] <- 'BU05991446' }
    else if (merged_g[i,]$Sub_BUURT == 'Liskwartier') { merged_g[i,'Codering_code'] <- 'BU05990534' }
    else if (merged_g[i,]$Sub_BUURT == 'Lombardijen') { merged_g[i,'Codering_code'] <- 'BU05991284' }
    else if (merged_g[i,]$Sub_BUURT == 'Middelland') { merged_g[i,'Codering_code'] <- 'BU05990325' }
    else if (merged_g[i,]$Sub_BUURT == 'Molenlaankwartier') { merged_g[i,'Codering_code'] <- 'BU05990665' }
    else if (merged_g[i,]$Sub_BUURT == 'Nesselande') { merged_g[i,'Codering_code'] <- 'BU05991468' }
    else if (merged_g[i,]$Sub_BUURT == 'Nieuw Crooswijk') { merged_g[i,'Codering_code'] <- 'BU05990836' }
    else if (merged_g[i,]$Sub_BUURT == 'Nieuwe Werk') { merged_g[i,'Codering_code'] <- 'BU05990118' }
    else if (merged_g[i,]$Sub_BUURT == 'Dijkzigt') { merged_g[i,'Codering_code'] <- 'BU05990119' }
    else if (merged_g[i,]$Sub_BUURT == 'Nieuwe Westen') { merged_g[i,'Codering_code'] <- 'BU05990324' }
    else if (merged_g[i,]$Sub_BUURT == 'Noordereiland') { merged_g[i,'Codering_code'] <- 'BU05991088' }
    else if (merged_g[i,]$Sub_BUURT == 'Zestienhoven') { merged_g[i,'Codering_code'] <- 'BU05990455' }
    else if (merged_g[i,]$Sub_BUURT == 'Schieveen') { merged_g[i,'Codering_code'] <- 'BU05990454' }
    else if (merged_g[i,]$Sub_BUURT == 'NoordKethel') { merged_g[i,'Codering_code'] <- 'BU05990452' }
    else if (merged_g[i,]$Sub_BUURT == 'Ommoord') { merged_g[i,'Codering_code'] <- 'BU05991463' }
    else if (merged_g[i,]$Sub_BUURT == 'Oosterflank') { merged_g[i,'Codering_code'] <- 'BU05991467' }
    else if (merged_g[i,]$Sub_BUURT == 'Oud-Charlois') { merged_g[i,'Codering_code'] <- 'BU05991574' }
    else if (merged_g[i,]$Sub_BUURT == 'Oud Crooswijk') { merged_g[i,'Codering_code'] <- 'BU05990837' }
    else if (merged_g[i,]$Sub_BUURT == 'Oud IJsselmonde') { merged_g[i,'Codering_code'] <- 'BU05991283' }
    else if (merged_g[i,]$Sub_BUURT == 'Oude Noorden') { merged_g[i,'Codering_code'] <- 'BU05990535' }
    else if (merged_g[i,]$Sub_BUURT == 'Oude Westen') { merged_g[i,'Codering_code'] <- 'BU05990111' }
    else if (merged_g[i,]$Sub_BUURT == 'OudMathenesse') { merged_g[i,'Codering_code'] <- 'BU05990327' }
    else if (merged_g[i,]$Sub_BUURT == 'Witte Dorp') { merged_g[i,'Codering_code'] <- 'BU05990328' }
    else if (merged_g[i,]$Sub_BUURT == 'Overschie') { merged_g[i,'Codering_code'] <- 'BU05990456' }
    else if (merged_g[i,]$Sub_BUURT == 'Pendrecht') { merged_g[i,'Codering_code'] <- 'BU05991577' }
    else if (merged_g[i,]$Sub_BUURT == 'Pernis') { merged_g[i,'Codering_code'] <- 'BU05991391' }
    else if (merged_g[i,]$Sub_BUURT == 'Prinsenland') { merged_g[i,'Codering_code'] <- 'BU05991448' }
    else if (merged_g[i,]$Sub_BUURT == 'Provenierswijk') { merged_g[i,'Codering_code'] <- 'BU05990516' }
    else if (merged_g[i,]$Sub_BUURT == 'Rozenburg') { merged_g[i,'Codering_code'] <- 'BU05992704' }
    else if (merged_g[i,]$Sub_BUURT == 'Rubroek') { merged_g[i,'Codering_code'] <- 'BU05990814' }
    else if (merged_g[i,]$Sub_BUURT == 's-Gravenland') { merged_g[i,'Codering_code'] <- 'BU05991444' }
    else if (merged_g[i,]$Sub_BUURT == 'Schiebroek') { merged_g[i,'Codering_code'] <- 'BU05990660' }
    else if (merged_g[i,]$Sub_BUURT == 'Schiemond') { merged_g[i,'Codering_code'] <- 'BU05990329' }
    else if (merged_g[i,]$Sub_BUURT == 'Spangen') { merged_g[i,'Codering_code'] <- 'BU05990323' }
    else if (merged_g[i,]$Sub_BUURT == 'Stadsdriehoek') { merged_g[i,'Codering_code'] <- 'BU05990110' }
    else if (merged_g[i,]$Sub_BUURT == 'Strand en duin') { merged_g[i,'Codering_code'] <- 'BU05991701' }
    else if (merged_g[i,]$Sub_BUURT == 'Struisenburg') { merged_g[i,'Codering_code'] <- 'BU05990847' }
    else if (merged_g[i,]$Sub_BUURT == 'Tarwewijk') { merged_g[i,'Codering_code'] <- 'BU05991571' }
    else if (merged_g[i,]$Sub_BUURT == 'Terbregge') { merged_g[i,'Codering_code'] <- 'BU05990664' }
    else if (merged_g[i,]$Sub_BUURT == 'Tussendijken') { merged_g[i,'Codering_code'] <- 'BU05990322' }
    else if (merged_g[i,]$Sub_BUURT == 'Vreewijk') { merged_g[i,'Codering_code'] <- 'BU05991080' }
    else if (merged_g[i,]$Sub_BUURT == 'Wielewaal') { merged_g[i,'Codering_code'] <- 'BU05991575' }
    else if (merged_g[i,]$Sub_BUURT == 'Zevenkamp') { merged_g[i,'Codering_code'] <- 'BU05991466' }
    else if (merged_g[i,]$Sub_BUURT == 'Zuiderpark en Zuidrand') { merged_g[i,'Codering_code'] <- 'BU05991578' }
    else if (merged_g[i,]$Sub_BUURT == 'Zuidplein') { merged_g[i,'Codering_code'] <- 'BU05991576' }
    else if (merged_g[i,]$Sub_BUURT == 'Zuidwijk') { merged_g[i,'Codering_code'] <- 'BU05991573' }
  }
  
  return(merged_g)
  
}
