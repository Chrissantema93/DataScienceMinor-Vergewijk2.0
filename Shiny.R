 library(shinyWidgets)
 library(leaflet)
 library(shiny)
 library(dplyr)
 library(sp)

source("Prepare data.R")
source("MergeGemeenteJaar.R")
source("Opschonen van alle datasets.R")
source("Clusters, classificatie en tree.R")
 

ui1 <- fluidPage(
  titlePanel("Vergewijk 'Welke wijk is geschikt voor mij?'."),
  sidebarLayout(
    sidebarPanel(
      sliderTextInput(inputId = 'Input1', label = 'Cultuur & recreatie: ', choices = c("low", "mid", "high"), selected = 'low', grid = TRUE),
      sliderTextInput(inputId = 'Input2', label = 'Dichtheid bevolking: ', choices = c("low", "mid", "high"), selected = 'low', grid = TRUE),
      sliderTextInput(inputId = 'Input3', label = 'Parkeergelegenheid: ', choices = c("low", "mid", "high"), selected = 'low', grid = TRUE),
      sliderTextInput(inputId = 'Input4', label = 'Woninggrootte: ', choices = c("low", "mid", "high"), selected = 'low', grid = TRUE),
     # textOutput("treepred"),
      selectInput(inputId = "Jaartal", "Uit welk jaar wilt u de gegevens zien?", choices = 
                    c('2018' = 'Buurt.Pol2018',
                      '2017' = 'Buurt.Pol2017',
                      '2016' = 'Buurt.Pol2016',
                      '2015' = 'Buurt.Pol2015',
                      '2014' = 'Buurt.Pol2014'),
                  selected= '2018')
    ),      
    mainPanel(        
      tabsetPanel(id = "Test",          
                  tabPanel("3D Plot",
                           selectInput("Var", "Welke wilt u", names(Buurt.Pol2018@data), selected=""),
                           leafletOutput("vancouver.map", width="80%",height="400px"),
                           selectInput("Business", "Wat is er gekozen", Buurt.Pol2018$buurtnaam, selected="")
                  ),          
                  tabPanel("Contour Graph",
                           tableOutput("data")
                  ),
                  tabPanel("Dashboard",
                           textOutput("tekst.dashboard"),
                           fluidRow(#12,
                             column(6,plotOutput('Leeftijdsopbouw')),
                             column(6,plotOutput('Demografie'))
                           ),
                           plotOutput('Tijdlijn')
                  ),
                  tabPanel("Decision tree"
                  )
                  
      )
    )
  )
)





server1 <- function(input, output, session){
  
  ######################################
  #         Generieke zaken (begin)    #
  ######################################
  
  inputx = reactive({   # (1/2) In inputx wordt de waarde uit de selectinput $Jaartal gestopt. 
    get(input$Jaartal)  # (2/2) Dit is het dataset waar een gebruiker mee wil werken in de applicatie
  }) 
  
  output$data <- renderTable( # De data die gekozen is bij inputx() wordt in een output gestopt
    inputx()@data
  )
  
  observe({ # Input$Var kan verschillen per dataset. Deze code past de input aan op basis van het dataset.
    updateSelectInput(session, "Var",
                      choices = names(inputx())
    )
  })
  
  ######################################
  #         Generieke zaken (eind)    #
  ######################################
  
  
  ##############################
  #     MAP   (begin)         #
  ##############################
  
  output$vancouver.map <- renderLeaflet({
    Chosen_var <- inputx()@data[,input$Var][which(inputx()@data$Soort_regio_omschrijving == "Buurt")]
    tt2 <- FixLength(Chosen_var, 78) + 1
    test <-   predicter(fit, c(input$Input1, input$Input2 ,input$Input3  , input$Input4))
    test3 <-  test[,which(test  > 0) ]
    #test2 <- subset(buurten2018, buurten2018$Codering_code %in% names(test3) , select = `_Wijken_en_buurten`)
    test4 <-  names(test3)
    selected_polygon <- subset(Buurt.Pol2018,Buurt.Pol2018@data$buurtcode %in%  test4)
    
    
    
    #lol <- predicter(fit, c("high", "high", "high", "high"))
    #lol2 <-  lol[,which(lol  > 0) ]
    ##lol3 <- subset(buurten2018, buurten2018$Codering_code %in% names(lol2) , select = `_Wijken_en_buurten`)
    ##lol4 <- as.vector(lol3[1])
    #lol5 <-  names(lol2)
    #lol6 <- subset(Buurt.Pol2018,Buurt.Pol2018@data$buurtcode %in%  lol5)
    
    
    
    
    ##Op het moment dat de onderstaande code wordt aangeroepen wordt de tekst voor de pop gemaakt.
    state_popup <- paste0("<strong>Name of the country </strong>", 
                          inputx()$buurtnaam, 
                          "<br><strong> information is  </strong>", 
                          inputx()@data[,input$Var],
                          
                          sep = "<br/>", ##Hier wordt 'link_click' gebruikt om te navigeren naar de pagina met het Dashboard
                          actionLink("?url=Test/Dashboard", "Ga naar het dashboard", onclick = 'Shiny.onInputChange(\"link_click\",  Math.random())'),
                          "Via het dashboard ziet u in een oogopslag de belangrijkste gegevens over deze wijk!"
    )
    
    ## De onderstaande code zorgt ervoor dat de daadwerkelijke map aangemaakt wordt. Dit is inclusief een legenda.
    mymap <- leaflet(inputx()) %>%
      addTiles() %>%
      addPolygons(data = inputx(), 
                  layerId= ~buurtnaam,
                  stroke = FALSE, 
                  smoothFactor = 0.3, 
                  fillOpacity = 1, 
                  group= "Test",
                  fillColor = ~pal(log10(tt2)),
                  popup = state_popup,
                  weight = 1 
      ) %>%
      
      
      addPolylines(data = selected_polygon, 
                   layerId= ~buurtcode,
                   stroke = TRUE, 
                   #smoothFactor = 0.3, 
                   opacity = 10 ,
                   color = "White",
                   group= "Voorspelde wijk",
                   #fillColor = ~pal(log10(tt2)),
                   popup = state_popup,
                   weight = 2 
      ) %>%
      
      addLegend(
        position = "bottomleft",
        pal = pal, 
        values = ~log10(tt2), 
        opacity = 1.0,
        title = 'Schaalverdeling',
        labFormat = labelFormat(transform = function(x) round(10^x))) %>%
      
      addLayersControl(
        overlayGroups =c("Test", "Lol"),
        options = layersControlOptions(collapsed=FALSE)
      )
    
    
    
  })
  
  ##############################
  #     MAP   (Eind)           #
  ############################## 
  
  
  ##############################
  #     klik locatie (begin)   #
  ############################## 
  
  observeEvent(input$vancouver.map_shape_click, { # update the location selectInput on map clicks
    p <- input$vancouver.map_shape_click
    print(p$id)     # HEt ID is gelijk aan het layer_id dat is opgegeven bij het maken van de map
    if(!is.null(p$id)){
      if(is.null(input$Business) || input$Business!=p$id){
        updateSelectInput(session, "Business", selected=p$id)  # update van input$Business
      }
    }
  })
  
  ## Dit wordt in de popup in Leaflet gebruikt om naar het Dashboard te kunnen gaan
  observeEvent(input$link_click, {
    updateTabsetPanel(session, "Test", "Dashboard")
  })
  
  ##############################
  #     klik locatie (eind)   #
  ############################## 
  
  output$tekst.dashboard <- renderText({ 
    paste("De pagina waar u zich nu bevindt geeft inzicht in de buurt die u geselecteerd heeft op de kaart, of via het menu. 
          De buurt waar u nu informatie van bekijkt heet: ", input$Business)
  })
  
  ###################################
  #   Leeftijdsopbouw               #
  ###################################
  
  l.opbouw = reactive({   # (1/2) In inputx wordt de waarde uit de selectinput $Jaartal gestopt. 
    Leeftijdsopbouw <- subset(buurten2018[rownames(buurten2018) == input$Business  ,c("0_tot_15_jaar_aantal", "15_tot_25_jaar_aantal", 
                                                                                      "25_tot_45_jaar_aantal", "45_tot_65_jaar_aantal", 
                                                                                      "65_jaar_of_ouder_aantal")])  # (2/2) Dit is het dataset waar een gebruiker mee wil werken in de applicatie
  }) 
  
  output$Leeftijdsopbouw <- renderPlot({
    color <- c("Black","turquoise2","Green","Grey","cyan4","Red", "Yellow")
    pie(as.vector(t(l.opbouw())), labels = colnames(l.opbouw()) , col = color , main = paste("De leeftijdsopbouw in de buurt: ", (input$Business)))
    legend(x = .9, y = .1, legend = colnames(l.opbouw()) , cex = 0.7, fill = color)
  })
  
  
  
  ###################################
  #   Demografie                    #
  ###################################
  
  demograf = reactive({   # (1/2) In inputx wordt de waarde uit de selectinput $Jaartal gestopt. 
    Demografie <- subset(buurten2018[rownames(buurten2018) == input$Business ,c("Marokko_aantal", "Nederlandse_Antillen_en_Aruba_aantal", 
                                                                                "Suriname_aantal", "Turkije_aantal", "Overig_niet-westers_aantal", 
                                                                                "Westers_totaal_aantal")])
  }) 
  
  output$Demografie <- renderPlot({
    color <- c("Black","turquoise2","Green","Grey","cyan4","Red", "Yellow")
    pie(as.vector(t(demograf())), labels = colnames(demograf()) , col = color , main = paste("Demografie in de buurt: ", (input$Business)))
    legend("topleft", legend = colnames(demograf()) , cex = 0.7, fill = color)
  })  
  
  ###################################
  #   Tijdlijn                      #
  ###################################
  
  Tijdlijn = reactive({   # (1/2) In inputx wordt de waarde uit de selectinput $Jaartal gestopt. 
    Historie <- subset(btotaal[btotaal$`_Wijken_en_buurten` == input$Business , ])
  }) 
  
  output$Tijdlijn <- renderPlot({
    plot(Tijdlijn()$Jaar_, Tijdlijn()[,input$Var], type = 'l')
  })  
  
  
 # output$treepred<-renderText({
 #   test <-   predicter(fit, c(input$Input1, input$Input2 ,input$Input3  , input$Input4))
 #   test3 <-  colnames(test)[,which(test  > 0) ]
#    test2 <- subset(buurten2018, buurten2018$Codering_code %in% names(test3) , select = `_Wijken_en_buurten`)
#    test4 <- as.vector(test2[1])
#    as.character(lol4)
#    
    
    
 #       lol <- predicter(fit, c("high", "high", "high", "high"))
 #       lol2 <-  lol[,which(lol  > 0) ]
 #       lol3 <- subset(buurten2018, buurten2018$Codering_code %in% names(lol2) , select = `_Wijken_en_buurten`)
 #       lol4 <- as.vector(lol3[1])
 #       lol5 <-  names(lol2)
 #       lol6 <- subset(Buurt.Pol2018,Buurt.Pol2018@data$buurtcode %in%  lol5)
        

        
  #      test <-   predicter(fit, c(input$Input1, input$Input2 ,input$Input3  , input$Input4))
  #      test3 <-  test[,which(test  > 0) ]
  #      #test2 <- subset(buurten2018, buurten2018$Codering_code %in% names(test3) , select = `_Wijken_en_buurten`)
  #      test4 <-  names(test3)
  #      selected_polygon <- subset(Buurt.Pol2018,Buurt.Pol2018@data$buurtcode %in%  test4)
        
        
        
        
  #})
  
  
  
  
  
  
}




shinyApp(ui = ui1, server = server1)        




























