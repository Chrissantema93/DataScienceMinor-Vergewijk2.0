#####################################
# VergeWijk  2019
# 6.Shiny.R
#####################################

min_ww <- min(btotaal$Gemiddelde_woningwaarde_x_1_000_euro)
max_ww <- max(btotaal$Gemiddelde_woningwaarde_x_1_000_euro)
middle_ww <- min_ww + ((max_ww - min_ww) / 2)



ui1 <- dashboardPage(
  dashboardHeader(title = "Vergewijk"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("3D plot",       tabName = "3Dplot",       icon = icon("map-marked-alt")),
      menuItem("Contour Graph", tabName = "ContourGraph", icon = icon("table")),
      menuItem("Dashboard",     tabName = "Dashboard",    icon = icon("chart-pie")),
      menuItem("Decision tree", tabName = "Decisiontree", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "3Dplot",
              h2("3D plot"),
              fluidRow(
                box(width = 12,
                    fluidRow(
                      column(4,
                             h2("Financieel"),
                             sliderInput(inputId = "Huisprijs", label = "Gemiddelde woningwaarde x 1000",
                                         min = min_ww, 
                                         max = max_ww, 
                                         value = 
                                           c(middle_ww, 
                                             middle_ww)
                             ),
                             br(),
                             hr(),
                             h2("Interesse"),
                             sliderTextInput(inputId = 'Input1', label = 'Cultuur & recreatie: ', choices = c("low_m", "low", "mid", "high", "high_p"), selected = 'low', grid = TRUE),
                             sliderTextInput(inputId = 'Input2', label = 'Dichtheid bevolking: ', choices = c("low_m", "low", "mid", "high", "high_p"), selected = 'low', grid = TRUE),
                             sliderTextInput(inputId = 'Input3', label = 'Parkeergelegenheid: ', choices = c("low_m", "low", "mid", "high", "high_p"), selected = 'low', grid = TRUE),
                             sliderTextInput(inputId = 'Input4', label = 'Woninggrootte: ', choices = c("low_m", "low", "mid", "high", "high_p"), selected = 'low', grid = TRUE),
                             sliderTextInput(inputId = 'Input5', label = 'Inkomen: ', choices = c("low_m", "low", "mid", "high", "high_p"), selected = 'low', grid = TRUE),
                             sliderTextInput(inputId = 'Input6', label = 'Wijkgrootte: ', choices = c("low_m", "low", "mid", "high", "high_p"), selected = 'low', grid = TRUE),
                             sliderTextInput(inputId = 'Input7', label = 'Onderhoud_omgeving: ', choices = c("low_m", "low", "mid", "high", "high_p"), selected = 'low', grid = TRUE),
                             sliderTextInput(inputId = 'Input8', label = 'Openbaar_vervoer: ', choices = c("low_m", "low", "mid", "high", "high_p"), selected = 'low', grid = TRUE),
                             sliderTextInput(inputId = 'Input9', label = 'Ontwikkeling: ', choices = c("low_m", "low", "mid", "high", "high_p"), selected = 'low', grid = TRUE),
                             # textOutput("treepred"),
                             selectInput(inputId = "Jaartal", "Uit welk jaar wilt u de gegevens zien?", choices =
                                           c('2018' = 'Buurt.Pol2018',
                                             '2017' = 'Buurt.Pol2017',
                                             '2016' = 'Buurt.Pol2016',
                                             '2015' = 'Buurt.Pol2015',
                                             '2014' = 'Buurt.Pol2014'),
                                         selected= '2018')
                      ),
                      column(8,
                             selectInput("Var", "Welke feature wilt u bekijken?", names(Buurt.Pol2018@data), selected="Aantal_inwoners_aantal"),
                             div(
                               style= 'margin-bottom:15px', 
                               leafletOutput("vancouver.map", width="100%",height="500px")
                             ),
                             infoBoxOutput(width = 12,"progressBox"),
                             
                             div(
                               style='visibility:hidden',
                               selectInput("Business", "Welke wijk is er gekozen", Buurt.Pol2018$buurtnaam, selected="Stadsdriehoek")
                             )
                             
                             
                      )
                    )
                )
              )
              
              
              
      ),
      
      tabItem(tabName = "ContourGraph",
              h2("Contour Graph"),
              fluidRow(
                box(width = 12,
                    div(style = 'overflow-x: scroll', tableOutput('data'))
                )
              )
      ),
      
      tabItem(tabName = "Dashboard",
              h2("Dashboard"),
              fluidRow(
                box(width = 12,
                    textOutput("tekst.dashboard"),
                    fluidRow(#12,
                      column(6,plotOutput('Leeftijdsopbouw')),
                      column(6,plotOutput('Demografie'))
                    ),
                    plotOutput('Tijdlijn')
                )
              )
      ),
      
      tabItem(tabName = "Decisiontree",
              h2("Decision tree"),
              selectInput(inputId = 'Vergelijk_wijk',
                          label = "Met welke wijk wilt u een vergelijking maken?",
                          choices = Buurt.Pol2018$buurtnaam, selected="Stadsdriehoek", multiple = TRUE),
              #selectInput("Vergelijk_wijk", "Met welke wijk wilt u een vergelijking maken?", Buurt.Pol2018$buurtnaam, selected="Stadsdriehoek"),
              plotOutput('ggplot'),
              plotOutput('ggplot2'),
              plotOutput('ggplot3'),
              plotOutput('ggplot4'),
              plotOutput('ggplot5'),
              plotOutput('ggplot6'),
              plotOutput('ggplot7'),
              plotOutput('ggplot8'),
              plotOutput('ggplot9'),
              plotOutput('ggplot10'),
              plotOutput('ggplot11'),
              plotOutput('ggplot12')
              
      )
    )
  )
)





server1 <- function(input, output, session){
  
  ######################################
  #         Generieke zaken (begin)    #
  ######################################
  
  output$dimension_display <- renderText({
    paste(input$dimension[1], input$dimension[2], input$dimension[2]/input$dimension[1])
  })
  
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
  
  output$progressBox <- renderInfoBox({
    infoBox(
      "Gekozen buurt", input$Business, icon = icon("map-marker-alt"),
      color = "purple"
    )
  })
  
  ######################################
  #         Generieke zaken (eind)    #
  ######################################
  
  
  ##############################
  #     MAP   (begin)         #
  ##############################
  
  output$vancouver.map <- renderLeaflet({
    Chosen_var <- inputx()@data[,input$Var]#[which(inputx()@data$Soort_regio_omschrijving == "Buurt")]
    tt2 <- FixLength(Chosen_var, 78) + 1
    test <-   predicter(fit, c(input$Input1, input$Input2 ,input$Input3  , input$Input4, input$Input5, input$Input6, input$Input7, input$Input8, input$Input9))
    test3 <-  test[,which(test  > 0) ]
    #test2 <- subset(buurten2018, buurten2018$Codering_code %in% names(test3) , select = `_Wijken_en_buurten`)
    test4 <-  names(test3)
    selected_polygon <- subset(Buurt.Pol2018,Buurt.Pol2018@data$buurtcode %in%  test4)
    selected_polygon2 <- subset(cents,cents@data$buurtcode %in%  test4)
    
    selected_house <- subset(Buurt.Pol2018,    Buurt.Pol2018@data$Gemiddelde_woningwaarde_x_1_000_euro >200 & Buurt.Pol2018@data$Gemiddelde_woningwaarde_x_1_000_euro < 300)
    selected_house@data$`_Wijken_en_buurten` <- toupper(selected_house@data$`_Wijken_en_buurten`)
    
    #    Buurt.Pol2018@data$Gemiddelde_woningwaarde_x_1_000_euro >200 & Buurt.Pol2018@data$Gemiddelde_woningwaarde_x_1_000_euro < 300
    
    # lol <- predicter(fit, c("high", "high", "high", "high"))
    # lol2 <-  lol[,which(lol  > 0) ]
    # lol3 <- subset(buurten2018, buurten2018$Codering_code %in% names(lol2) , select = `_Wijken_en_buurten`)
    #  lol4 <- as.vector(lol3[1])
    #  lol5 <-  names(lol2)
    #  lol6 <- subset(Buurt.Pol2018,Buurt.Pol2018@data$buurtcode %in%  lol5)
    #  lol7 <- subset(cents,cents@data$buurtcode %in%  lol5)
    
    
    
    ##Op het moment dat de onderstaande code wordt aangeroepen wordt de tekst voor de pop gemaakt.
    state_popup <- paste0("<strong>Name of the district: </strong>", 
                          inputx()$buurtnaam, 
                          "<br><strong>Value is:   </strong>", 
                          inputx()@data[,input$Var],
                          "<br><a href='#shiny-tab-Dashboard' data-toggle='tab' data-value='Dashboard'> <b> Ga naar het dashboard </b> </a> <br>",
                          "Via het dashboard ziet u in een oogopslag de belangrijkste  gegevens over deze wijk!",
                          sep = "<br/>" ##Hier wordt 'link_click' gebruikt om te navigeren naar de pagina met het Dashboard
                          
    )
    
    
    ## De onderstaande code zorgt ervoor dat de daadwerkelijke map aangemaakt wordt. Dit is inclusief een legenda.
    mymap <- leaflet(inputx()) %>%
      addTiles() %>%
      addPolygons(data = inputx(), 
                  layerId= ~buurtnaam,
                  stroke = FALSE, 
                  smoothFactor = 0.3, 
                  fillOpacity = 1, 
                  group= "Data",
                  fillColor = ~pal(log10(tt2)),
                  popup = state_popup,
                  weight = 1 
      ) %>%
      
      addPolylines(data = selected_house, 
                   layerId= ~`_Wijken_en_buurten`,
                   stroke = TRUE, 
                   #smoothFactor = 0.3, 
                   opacity = 0.5 ,
                   color = "red",
                   group= "Huizenprijs",
                   #fillColor = ~pal(log10(tt2)),
                   popup = state_popup,
                   weight = 5 
      ) %>%

            addPolylines(data = selected_polygon, 
                   layerId=  ~buurtcode,
                   stroke = TRUE, 
                   #smoothFactor = 0.3, 
                   opacity = 10 ,
                   color = "White",
                   group= "Voorspelde wijk",
                   #fillColor = ~pal(log10(tt2)),
                   popup = state_popup,
                   weight = 1 
      ) %>%
      

      
      
      
      # 
      # addMarkers( data = selected_polygon2,
      #             layerId= ~`_Wijken_en_buurten`,
      #             lng = selected_polygon2@data$Coord1,
      #             lat = selected_polygon2@data$Coord2,
      #             popup = selected_polygon2@data$`_Wijken_en_buurten`
#      )%>%
      
      addLegend(
        position = "bottomleft",
        pal = pal, 
        values = ~log10(tt2), 
        opacity = 1.0,
        title = 'Schaalverdeling',
        labFormat = labelFormat(transform = function(x) round(10^x))) %>%
      
      addLayersControl(
        overlayGroups =c("Data", "Huizenprijs", "Voorspelde wijk"),
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
    Leeftijdsopbouw <- subset(buurten2018[buurten2018$`_Wijken_en_buurten` == input$Business  ,c("0_tot_15_jaar_aantal", "15_tot_25_jaar_aantal", 
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
    Demografie <- subset(buurten2018[buurten2018$`_Wijken_en_buurten` == input$Business ,c("Marokko_aantal", "Nederlandse_Antillen_en_Aruba_aantal", 
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
    ggplot(data=Tijdlijn(), aes(x=Jaar_, y= !!as.symbol(input$Var))) + geom_line(color="blue") + geom_point() +
      geom_text(aes(label=!!as.symbol(input$Var), vjust= -1)) + 
      ggtitle(paste("Verloop van feature", as.symbol(input$Var), "over de jaren")) +
      labs(x = "Jaren")
  })  
  
  
  output$ggplot <- renderPlot({ 
    Geloofsovertuiging <- subset(Buurten_Totaal,
                                 `_Wijken_en_buurten` %in% c(input$Business, input$Vergelijk_wijk) )
    #  Geloofsovertuiging <- tidyr::unite(Geloofsovertuiging,"id_loc",Jaar_,`_Wijken_en_buurten`,remove = F)
    
    ggplot(Geloofsovertuiging, aes(fill=`_Wijken_en_buurten`, y=Si_X..bewoners.dat.zegt.dat.er.in.de.buurt.genoeg..plekken.voor.geloofsbetuiging.en.levensbeschouwlijke.bijeenkomsten, x= Jaar_ , group = `_Wijken_en_buurten`)) + 
      geom_bar(position="dodge", stat="identity") +
      scale_x_continuous(breaks = seq(2014,2019,1))+
      
      # scale_color_manual(values=`_Wijken_en_buurten`) +
      #facet_grid(. ~ `_Wijken_en_buurten`) +
      ggtitle("Bewoners die vinden dat er voldoende plek is voor geloofsbetuiging!") + 
      labs(y="Aantal (in %)", x = "Jaren")
  })
  
  
  
  output$ggplot2 <- renderPlot({ 
    Boodschappen <- subset(Buurten_Totaal,
                           `_Wijken_en_buurten` %in% c(input$Business, input$Vergelijk_wijk))
    ggplot(Boodschappen, aes(fill=`_Wijken_en_buurten`, y=Sub_X..voldoende.aanwezig.winkels.dagelijkse.boodschappen, x= Jaar_ , group = `_Wijken_en_buurten`)) + 
      #    ggplot(Boodschappen, aes(fill=Jaar_, y=Sub_X..voldoende.aanwezig.winkels.dagelijkse.boodschappen, x= Jaar_)) + 
      geom_bar(position="dodge", stat="identity") +
      #      facet_grid(. ~ `_Wijken_en_buurten`) +
      ggtitle("Voldoende winkels aanwezig voor de dagelijkse boodschappen") + 
      labs(y="Aantal (In %)", x = "Jaren")
  })
  
  
  output$ggplot3 <- renderPlot({ 
    Woningvoorraad <- subset(Buurten_Totaal,
                             `_Wijken_en_buurten` %in% c(input$Business, input$Vergelijk_wijk))
    ggplot(Woningvoorraad, aes(fill=`_Wijken_en_buurten`, y=Woningvoorraad_aantal, x= Jaar_ , group = `_Wijken_en_buurten`)) + 
      #ggplot(Woningvoorraad, aes(fill=Jaar_, y=Woningvoorraad_aantal, x= Jaar_)) + 
      geom_bar(position="dodge", stat="identity") +
      #      facet_grid(. ~ `_Wijken_en_buurten`) +
      ggtitle("Woningvoorraad") + 
      labs(y="Aantal (n)", x = "Jaren")
  })
  
  output$ggplot4 <- renderPlot({ 
    Bushaltes <- subset(Buurten_Totaal,
                        `_Wijken_en_buurten` %in% c(input$Business, input$Vergelijk_wijk))
    
    ggplot(Bushaltes, aes(fill=`_Wijken_en_buurten`, y=Obj_X..woningen.met.bushaltes.binnen.normafstand, x= Jaar_ , group = `_Wijken_en_buurten`)) +
      #ggplot(Bushaltes, aes(fill=Jaar_, y=Obj_X..woningen.met.bushaltes.binnen.normafstand, x= Jaar_)) + 
      geom_bar(position="dodge", stat="identity") +
      #      facet_grid(. ~ `_Wijken_en_buurten`) +
      ggtitle("%woningen in de buurt van een bushalte") + 
      labs(y="Aantal (In %)", x = "Jaren")
  })
  
  output$ggplot5 <- renderPlot({ 
    Metrostations <- subset(Buurten_Totaal,
                            `_Wijken_en_buurten` %in% c(input$Business, input$Vergelijk_wijk))
    
    ggplot(Metrostations, aes(fill=`_Wijken_en_buurten`, y=Obj_aantal.metrostations, x= Jaar_ , group = `_Wijken_en_buurten`)) +
      #ggplot(Metrostations, aes(fill=Jaar_, y=Obj_aantal.metrostations, x= Jaar_)) + 
      geom_bar(position="dodge", stat="identity") +
      #      facet_grid(. ~ `_Wijken_en_buurten`) +
      ggtitle("Aantal Metrostations") + 
      labs(y="Aantal (n)", x = "Jaren")
  })
  
  output$ggplot6 <- renderPlot({ 
    Bakker <- subset(Buurten_Totaal,
                     `_Wijken_en_buurten` %in% c(input$Business, input$Vergelijk_wijk))
    
    ggplot(Bakker, aes(fill=`_Wijken_en_buurten`, y=Obj_X..woningen.met.bakker.binnen.normafstand, x= Jaar_ , group = `_Wijken_en_buurten`)) +
      #ggplot(Bakker, aes(fill=Jaar_, y=Obj_X..woningen.met.bakker.binnen.normafstand, x= Jaar_)) + 
      geom_bar(position="dodge", stat="identity") +
      #      facet_grid(. ~ `_Wijken_en_buurten`) +
      ggtitle("Wonignen in de buurt van een bakker") + 
      labs(y="Aantal (n)", x = "Jaren")
  })
  
  output$ggplot7 <- renderPlot({ 
    Bushaltes2 <- subset(Buurten_Totaal,
                         `_Wijken_en_buurten` %in% c(input$Business, input$Vergelijk_wijk))
    
    ggplot(Bushaltes2, aes(fill=`_Wijken_en_buurten`, y=Obj_aantal.bushaltes, x= Jaar_ , group = `_Wijken_en_buurten`)) +
      #ggplot(Bushaltes2, aes(fill=Jaar_, y=Obj_aantal.bushaltes, x= Jaar_)) + 
      geom_bar(position="dodge", stat="identity") +
      #      facet_grid(. ~ `_Wijken_en_buurten`) +
      ggtitle("Het aantal bushaltes in de buurt") + 
      labs(y="Aantal (n)", x = "Jaren")
  })
  
  output$ggplot8 <- renderPlot({ 
    Levens_Kwaliteit <- subset(Buurten_Totaal,
                               `_Wijken_en_buurten` %in% c(input$Business, input$Vergelijk_wijk))
    
    ggplot(Levens_Kwaliteit, aes(fill=`_Wijken_en_buurten`, y=Si_Oordeel.kwaliteit.van.leven, x= Jaar_ , group = `_Wijken_en_buurten`)) +
      #ggplot(Levens_Kwaliteit, aes(fill=Jaar_, y=Si_Oordeel.kwaliteit.van.leven, x= Jaar_)) + 
      geom_bar(position="dodge", stat="identity") +
      #      facet_grid(. ~ `_Wijken_en_buurten`) +
      ggtitle("Oordeel over de levenskwaliteit in de wijk") + 
      labs(y="Aantal (n)", x = "Jaren")
  })
  
  output$ggplot9 <- renderPlot({ 
    Basischolen <- subset(Buurten_Totaal,
                          `_Wijken_en_buurten` %in% c(input$Business, input$Vergelijk_wijk))
    
    ggplot(Basischolen, aes(fill=`_Wijken_en_buurten`, y=Sub_X..voldoende.aanwezig.basisscholen, x= Jaar_ , group = `_Wijken_en_buurten`)) +
      #ggplot(Basischolen, aes(fill=Jaar_, y=Sub_X..voldoende.aanwezig.basisscholen, x= Jaar_)) + 
      geom_bar(position="dodge", stat="identity") +
      #      facet_grid(. ~ `_Wijken_en_buurten`) +
      ggtitle("Inwoners die vinden dat er voldoende basisscholen in de buurt zijn.") + 
      labs(y="Aantal (In %)", x = "Jaren")
  })
  
  output$ggplot10 <- renderPlot({ 
    Stankoverlast <- subset(Buurten_Totaal,
                            `_Wijken_en_buurten` %in% c(input$Business, input$Vergelijk_wijk))
    
    ggplot(Stankoverlast, aes(fill=`_Wijken_en_buurten`, y=Sub_X..veel.stankoverlast.verkeer, x= Jaar_ , group = `_Wijken_en_buurten`)) +
      #ggplot(Stankoverlast, aes(fill=Jaar_, y=Sub_X..veel.stankoverlast.verkeer, x= Jaar_)) + 
      geom_bar(position="dodge", stat="identity") +
      #      facet_grid(. ~ `_Wijken_en_buurten`) +
      ggtitle("Inwoners die stankoverlast ervaren van het verkeer") + 
      labs(y="Aantal (In %)", x = "Jaren")
  })
  
  
  output$ggplot11 <- renderPlot({ 
    Buurt_omgang <- subset(Buurten_Totaal,
                           `_Wijken_en_buurten` %in% c(input$Business, input$Vergelijk_wijk))
    
    ggplot(Buurt_omgang, aes(fill=`_Wijken_en_buurten`, y=Si_X..bewoners.dat.zegt.dat.buurtbewoners.veel.met.elkaar.om.gaan, x= Jaar_ , group = `_Wijken_en_buurten`)) +
      #ggplot(Buurt_omgang, aes(fill=Jaar_, y=Si_X..bewoners.dat.zegt.dat.buurtbewoners.veel.met.elkaar.om.gaan, x= Jaar_)) + 
      geom_bar(position="dodge", stat="identity") +
      #      facet_grid(. ~ `_Wijken_en_buurten`) +
      ggtitle("In hoeverre gaan inwoners van een buurt met elkaar om") + 
      labs(y="Aantal (In %)", x = "Jaren")
  })
  
  output$ggplot12 <- renderPlot({ 
    Vernieling <- subset(Buurten_Totaal,
                         `_Wijken_en_buurten` %in% c(input$Business, input$Vergelijk_wijk))
    
    ggplot(Vernieling, aes(fill=`_Wijken_en_buurten`, y=Fi_Vernielde.kapotte.banken..vuilnisbakken.etc.komt.vaak.voor.als.buurtprobleem, x= Jaar_ , group = `_Wijken_en_buurten`)) +
      #ggplot(Vernieling, aes(fill=Jaar_, y=Fi_Vernielde.kapotte.banken..vuilnisbakken.etc.komt.vaak.voor.als.buurtprobleem, x= Jaar_)) + 
      geom_bar(position="dodge", stat="identity") +
      #      facet_grid(. ~ `_Wijken_en_buurten`) +
      ggtitle("Ervaren overlast door vernieling van openbare voorzieningen") + 
      labs(y="Aantal (n)", x = "Jaren")
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
