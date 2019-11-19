#Alles in environment verwijderen
remove(list = ls())

#####################################
# Uitvoeren van alle scripts
#####################################

source("1.Import.R")
source("2.Prepare data.R")
source("3.MergeGemeenteJaar.R")
source("4.Opschonen van alle datasets.R") #duurt lang, even geduld.
source("5.Clusters, classificatie en tree.R")
runApp("6.Shiny.R")
