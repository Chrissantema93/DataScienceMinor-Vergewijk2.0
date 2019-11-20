#####################################
# VergeWijk © 2019
# 1.Import.R
#####################################

#Alles in environment verwijderen
remove(list = ls())

#####################################
#Hulp functies
#####################################
detachAllPkgs <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  return()
}

InstallAndLoadPkgs <- function(packages) {
  notInstalled <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(notInstalled) > 0) install.packages(notInstalled, dep=TRUE)
  lapply(packages, require, character.only = TRUE)
  return()
}

#####################################
#Installeren en laden van libraries
#####################################
detachAllPkgs()
InstallAndLoadPkgs(c("shiny"       , # Om de data te presenteren
                     "shinyWidgets", # Nodig voor slidertextinput
                     "leaflet"     , # Map in shiny
                     "dplyr"       , # ??? https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html 
                     "data.table"  , # ???
                     "rpart"       , # Nodig voor maken decision tree
                     "maptree"     , # Plot een betere tree dan rpart
                     "tidyverse"   , # Een pakket met van alles
                     "gridExtra"   , # Hiermee zet je plotjes netjes naast elkaar
                     "partykit"    , # Gebruiken we deze nog?
                     "sp"          , # Voor het mergen van de Spatial Polygon data
                     "geojsonio"     # Voor zover ik weet wordt deze niet meer gebruikt
                     ))

