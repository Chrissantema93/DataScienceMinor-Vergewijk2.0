#####################################
# VergeWijk © 2019
# 0.Main.R
#####################################

#####################################
# Uitvoeren van alle scripts
#####################################

#Eventueel kunnen 2 en 3 samen in 1 script?

source("1.Import.R") #Let op! Heel je environment word hier leeg gemaakt.
source("2.Prepare data.R")
source("3.MergeGemeenteJaar.R")
source("4.Opschonen van alle datasets.R")
source("5.Clusters, classificatie en tree.R")
runApp("6.Shiny.R")
