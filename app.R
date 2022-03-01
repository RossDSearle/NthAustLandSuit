sessionStartTime <- as.character(Sys.time())

library(shiny)
library(raster)
library(stringr)
library(leaflet)
library(leafem)
library(XML)
library(leaflet.extras)
library(htmltools)
library(shinyBS)
library(rhandsontable)
library(RColorBrewer)
library(zip)
library(shinycssloaders)
library(shinyalert)
library(shinyjs)
library(shinybusy)
library(shinyWidgets)

source('appConfig.R')
source("Utils.R")
source('SuitUtils_Roper.R')
source('drillMap.R')

####  App load tasks #####

suitFile = paste0(suitFrameworkPath, '/ROWRA_Suitability_Framework_V5.xlsx')
classFile = paste0(suitFrameworkPath, '/LimitationRangesRoper.xlsx')
mappingsFile <- paste0(suitFrameworkPath, '/LimitationsFileMappingsRoper2.xlsx')

suitFramework <- readSuitFramework(suitFile = suitFile, sheet = 'SUBCLASSES-2-M-linked', classFile = classFile)
limRastersList <- getLimitationsPaths(mapFile = mappingsFile)

attList <- list.files(paste0(dataRoot, '/Maps/Attributes'), recursive = F, full.names = F)
attList <- str_remove(attList, '.tif')

useList <- getSuitList(suitFramework)



server <- function(input, output,session) {
  
  #####  APP SETUP ##########################
  ##### Initialise global variables   #######
  
  RV <- reactiveValues()
  RV$sessionStartTime <- as.character(Sys.time())
  RV$currentDrill <- NULL
  RV$Lat <- NULL
  RV$Lon <- NULL
  RV$CurrentRegionRec <- NULL
  RV$CurrentRegion <- NULL
  RV$CurrentUseList <- NULL
  RV$CurrentAttList <- NULL
  RV$CurrentCovariatesList <- NULL
  RV$CurrentExtent <- NULL
  RV$CurrentRaster<- NULL
  RV$MapLegendVals <- NULL
  RV$MapLegendPallete <- NULL
  RV$MapDataPath <- NULL
  RV$MapIsCat <- NULL
  

  SMIPSDrillbtn <- if (isTRUE(ALWAYS_DRILL)) reactiveVal('SmipsDrillOn') else reactiveVal('SmipsDrillOff')

  ####  Set up lists etc on App load  #####  
  
  
  observe({
    RV$CurrentRegion = defaultRegion
  })
  # update the Regions list
  observe({
    req(RV$CurrentRegion)
      updateSelectInput(session, "Region", choices =  regionsDF$region, selected = RV$CurrentRegion )
  })
  
  # update the Map Product list
  observe({
    updateSelectInput(session, "ProductType", choices =  AvailableMapProducts$prodNames, selected = defaultProductType )
  })
  
  
  
  
  # setup Product Type info
  observe({
    
    # query <- parseQueryString(session$clientData$url_search)
    # if (!is.null(query[['region']])) {
    #   RV$CurrentRegion = query[['region']]
    # }else{
    #   RV$CurrentRegion = defaultRegion
    # }
    
    req( input$Region)
    
    input$Region
  #  updateSelectInput(session, "Region", selected = reg )
    
    reg <-input$Region
    print(paste0('Current Region is ', reg))
    RV$CurrentRegionRec <- regionsDF[regionsDF$region == reg, ]
    
    usePath <- paste0(dataRoot, '/', reg, '/Suitability')
    u1 <- list.files(usePath, pattern='.tif$', full.names = F)
    u2 <- u1[!grepl('_Uncert.tif', u1)]
    u3 <- str_remove_all(u2, '.tif') 
    RV$CurrentUseList <- u3
    
    attPath <- paste0(dataRoot, '/', reg, '/Attributes')
    a1 <- list.files(attPath, pattern='.tif$', full.names = F)
    a2 <- str_remove_all(a1, '.tif') 
    RV$CurrentAttList <- a2
    
    covPath <- paste0(dataRoot, '/', reg, '/Covariates')
    c1 <- list.files(covPath, pattern='.tif$', full.names = F)
    c2 <- str_remove_all(c1, '.tif') 
    RV$CurrentCovariatesList <- c2
    
    req(input$Region, input$ProductType)
    updateSelectInput(session, "ProductName", choices = NULL, selected = NULL)
    
    if(input$ProductType == 'Suitability'){
      lvs <- RV$CurrentUseList
    }else if(input$ProductType == 'Attributes') {
      lvs <-  RV$CurrentAttList
    }else if(input$ProductType == 'Covariates') {
      lvs <-  RV$CurrentCovariatesList
    }
    updateSelectInput(session, "ProductName", choices =  sort(lvs), selected = lvs[1] )
    
  })
  
  # update the Product Type list
  observe({
    
    
  })
  
  
 
  
  ################### MAP RENDERING FUNCTIONS  ################# 
  ################### Render the Map  #################
  
  
  observe({
    req(input$SMIPSDrillbtn)
    if (!is.null(input$SMIPSDrillbtn)) {
      SMIPSDrillbtn(input$SMIPSDrillbtn)
    } })
  
  output$mainMap <- renderLeaflet({
    
     req(input$ProductType, RV$CurrentRegion, RV$CurrentExtent)
    
    midx <- (RV$CurrentExtent@xmax - RV$CurrentExtent@xmin)/2 +  RV$CurrentExtent@xmin
    midy <-  RV$CurrentExtent@ymax - abs(RV$CurrentExtent@ymax - RV$CurrentExtent@ymin)/2
    
    lf <- leaflet() %>% clearMarkers() %>%
      addTiles(group = "Map") %>%
      #addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%
      setView(lng = midx, lat = midy, zoom = 7)
    

    sbo <- scaleBarOptions(maxWidth = 200, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE)
    lf <- addLayersControl(lf,
                          # baseGroups = c("Map"),
                           overlayGroups =  c("Maps"),
                           options = layersControlOptions(collapsed = FALSE)
    ) %>%
      addScaleBar(position = "bottomleft", options = sbo) %>%
      addMouseCoordinates()  %>%
      #addHomeButton(ext=defExt)

      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to full extent",
        onClick=JS("function(btn, map){ map.setView({lon: 133.8, lat: -14.98}, 7); }")))
        #onClick=JS("function(btn, map){ map.setZoom(1); }")))
    # addWMSLegend(uri = paste0(wmsServer, '.nc?VERSION=1.3.0&layer=', prod, '&REQUEST=GetLegendGraphic&FORMAT=image/png'),  position =  "bottomright")
    
    
    lf <- if (isTRUE(ALWAYS_DRILL)) lf else addEasyButtonBar(lf, position = "topright" ,
                                                             easyButton( position = "topright" ,
                                                                         states = list(
                                                                           easyButtonState(
                                                                             stateName="sOff",
                                                                             #icon="fa-check",
                                                                             #icon=htmltools::span(class = "star", htmltools::HTML('<img src="Buttons/drill.png"></font>')),
                                                                             icon=htmltools::span(class = "star", htmltools::HTML('<font color="gray">Drill&nbsp;Timeseries&nbsp;<b>Off</b></font>')),
                                                                             title="Click to turn on drilling of a pixel on the map to return a timeseries of soil moisture values",
                                                                             onClick = JS("
              function(btn, map) {
                btn.state('sOn');
                Shiny.onInputChange('SMIPSDrillbtn', 'SmipsDrillOn');
               $('.leaflet-container').css('cursor','crosshair');
              }")
                                                                           ),
                                                                           easyButtonState(
                                                                             stateName="sOn",
                                                                             icon=htmltools::span(class = "star", htmltools::HTML('<font color="green">Drill&nbsp;Timeseries&nbsp;<b>On</b></font>')),
                                                                             title="Click to turn off drilling of a pixel on the map to return a timeseries of soil moisture values",
                                                                             onClick = JS("
              function(btn, map) {
                btn.state('sOff');
                Shiny.onInputChange('SMIPSDrillbtn', 'SmipsDrillOff');
                $('.leaflet-container').css('cursor','');
              }")
                                                                           )
                                                                         )
                                                             ))
    lf <- if (isTRUE(ALWAYS_DRILL))
      htmlwidgets::onRender(lf, "
          function(el, x) {
            var mainMap = this;
            window.mainMap = mainMap;
            $('.leaflet-container').css('cursor','crosshair');
          }")
    else
      htmlwidgets::onRender(lf, "
          function(el, x) {
            var mainMap = this;
            window.mainMap = mainMap;
          }")
    
    
  })
  

  
  #######   Update the map parameters on changed layer #####
  
  observe({
   
    req(input$ProductName, input$Region)
    
    #isolate({
      
    if(input$ProductType =='Suitability'){
     
          fname <- paste0(dataRoot, '/', RV$CurrentRegionRec$region, '/Suitability/', input$ProductName ,'.tif' )
          if(file.exists(fname)){
              Scols <- c("darkgreen", "green", "lightgreen", "orange", "brown")
              brks <- c( 1, 2, 3, 4, 5)
              pal <- colorFactor(Scols, brks,na.color = "transparent")
              RV$MapLegendPallete <- pal
              suitLabs <- c("1", "2", "3", "4", "5")
              RV$MapIsCat <- T
      }
    }else if(input$ProductType =='Attributes') {
      fname <- paste0(dataRoot, '/', RV$CurrentRegionRec$region,'/Attributes/', input$ProductName ,'.tif' )

          if(file.exists(fname)){
          ratName <- str_replace(fname, '.tif', '.csv')
          
          if(file.exists(ratName))
          {
            rat <- read.csv(ratName, stringsAsFactors = F)
            brks <- rat$ID
            acols <- rainbow(length(brks))
            pal <- colorFactor(acols, brks, na.color = "transparent")
            RV$MapLegendPallete <- pal
            RV$MapIsCat <- T
          }else{
            r <- raster(fname)
            brks <- c(minValue(r), maxValue(r))
            pal <- colorNumeric(rainbow(5), brks, na.color = "transparent")
            RV$MapLegendPallete <- pal
            RV$MapIsCat <- F
          }
      }
    }else if(input$ProductType =='Covariates') {
        fname <- paste0(dataRoot, '/', RV$CurrentRegionRec$region,'/Covariates/', input$ProductName ,'.tif' )
        if(file.exists(fname)){
            r <- raster(fname)
            brks <- c(minValue(r), maxValue(r))
            pal <- colorNumeric(rainbow(5), brks, na.color = "transparent")
            RV$MapLegendPallete <- pal
            RV$MapIsCat <- F
        }
    }

      
     print(fname)
      
      if(file.exists(fname)){
        RV$MapDataPath <- fname
        RV$CurrentRaster <- terra::rast(RV$MapDataPath)
        RV$CurrentExtent <- raster::extent(RV$CurrentRegionRec$minx, RV$CurrentRegionRec$maxx, RV$CurrentRegionRec$miny, RV$CurrentRegionRec$maxy )
        
        midx <- (RV$CurrentExtent@xmax - RV$CurrentExtent@xmin)/2 +  RV$CurrentExtent@xmin
        midy <-  RV$CurrentExtent@ymax - abs(RV$CurrentExtent@ymax - RV$CurrentExtent@ymin)/2
       
        
        
        proxy <- leafletProxy("mainMap")   %>% clearControls() %>% leaflet::addLegend(pal = pal, values = brks, position = "topright") %>% setView(lng = midx, lat = midy, zoom = 7)
      
        }else{
        print(paste0('File does not exist - ', fname))
        RV$CurrentRaster <- NULL
        RV$CurrentExtent  <- NULL
        RV$MapDataPath <- NULL
      }
      
 # })
})
  
#####   Redraw the Map #######  
  observe({
    
    req(RV$CurrentRegionRec,  RV$CurrentRaster )
    
    if(is.null(input$mainMap_bounds))
      return()
    
    if(file.exists( RV$MapDataPath)){

    proxy <- leafletProxy("mainMap")  %>% clearGroup("Maps")
    
    input$mainMapTrans
    input$ProductName
    
   # isolate({
   
    rec <- AvailableMapProducts[AvailableMapProducts$prodNames==input$ProductType,]
    bds <- input$mainMap_bounds

    
    zl <- max(input$mainMap_zoom, 1) * 1

       
    we <- max(bds$west, RV$CurrentExtent@xmin)
    ee <- min(bds$east, RV$CurrentExtent@xmax)
    se <-  max(bds$south, RV$CurrentExtent@ymin)
    ne <- min(bds$north, RV$CurrentExtent@ymax)
    
    # if(file.exists(fname)){
    #   cmd <-paste0('gdal_translate ', fname, ' ', tmpfl, ' -ot Int16 -r nearest -q -outsize ', zl,'% ', zl,'% -projwin ', we, ' ', ne, ' ', ee, ' ', se)
    #   op <- system(cmd, intern = T)
    #   r <- raster(tmpfl)
    #   
    #   suppressWarnings(proxy %>% leaflet::addRasterImage(r, group = "Roper Maps",  colors = pal,  project=FALSE, opacity = input$mainMapTrans))
    #   unlink(tmpfl)
    # }else{
    #   print(paste0('File does not exist - ', fname))
    # }
         
            ce <- terra::ext(we, ee, se, ne)
            r <- RV$CurrentRaster
           
           try( terra::window(r) <- ce)
            tr <- terra::spatSample(r, maxcells, method="regular", as.raster=TRUE)
           # crs(tr) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
            tmpfl = tempfile(fileext = ".tif")
            terra::writeRaster(tr , tmpfl, overwrite=T)
            rr <- raster::raster(tmpfl)
           # rr <- raster::raster(tr)  
            #crs(rr) <-CRS(defaultProjection)
            #crs(rr) <-CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
            proxy %>% leaflet::addRasterImage(rr, group = 'Maps', colors = RV$MapLegendPallete,  project=FALSE, opacity = input$mainMapTrans)
            terra::window(r) <- NULL
            unlink(tmpfl)
       # }else{
       #   print(paste0('File does not exist - ', RV$MapDataPath))
        }
   #  })
    
})
  
  
   output$wmsLegend <- renderUI({
     
    
  #  if(input$ProductType=='Suitability'){

      # if(input$ProductType == 'Suitability'){
      #  imageLoc <- paste0('./Legends/suitLegend.png')
      #  
      #  }else{
      #    imageLoc <- paste0('./Legends/', input$ProductName, '_legend.png')
      #  }
      #    div_style <- paste0("box-sizing: content-box; border: 1px solid black; width: 150px; height: 260px; background-image: url(\'", imageLoc, "\');  background-repeat: no-repeat;")
      #  div(style=div_style)
 #   }else{
    #   div(style="visibility:hidden;")
    # }
   })
 
  
  ##############      MAP QUERYING FUNCTIONS  ##############################
   
   output$DrillInfoTable  = renderRHandsontable({
     req(RV$currentDrill)
     rhandsontable(RV$currentDrill,  height = 600, manualColumnResize = T, readOnly = TRUE, rowHeaders = F, colHeaders = NULL) %>%
       hot_cols(renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
           Handsontable.renderers.NumericRenderer.apply(this, arguments);
            if (value == 'VALUE' | value == 'ATTRIBUTE' | value == 'SUITABILITY'  | value == 'LANDUSE'| value == 'LAYER') {
               td.style.background = '#00303b';
               td.style.color= 'white';
               
            } 
           }")
   })
  
  
  
  
  ################  Get data from drilling a map pixel ##########
  observeEvent(input$mainMap_click, {
    
    click <- input$mainMap_click
    
    if(is.null(click))
      return()
    
    req(SMIPSDrillbtn)
    if(SMIPSDrillbtn() == 'SmipsDrillOn')
      
        shinyBS::createAlert(session, "progressAlert", "drillingAlertInstance", title = "", content = paste0('<img src=wait.gif>&nbsp;&nbsp;Retrieving data from location ', sprintf(click$lng, fmt = '%#.4f'), " ", sprintf(click$lat, fmt = '%#.4f') ), append = FALSE)
        df <- drillMap(click$lng,click$lat, dataRoot=dataRoot, region=input$Region, useList=RV$CurrentUseList, attList=RV$CurrentAttList, what=input$DrillReturn, currentSuit=input$ProductName, currentLayer=RV$MapDataPath)
        
        if(is.null(df)){
          shinyalert(
                title = "Oops",
                text = "There is no data for this location.",
                #size = "xs",
                closeOnEsc = TRUE,
                closeOnClickOutside = FALSE,
                html = FALSE,
                type = "info",
                showConfirmButton = TRUE,
                showCancelButton = FALSE,
                confirmButtonText = "OK",
                confirmButtonCol = "#305234",
                timer = 0,
                imageUrl = "",
                animation = TRUE
              )
        }else{
          RV$currentDrill  <- df
          RV$Lat <-  sprintf(click$lat, fmt = '%#.4f')
          RV$Lon <- sprintf(click$lng, fmt = '%#.4f')
        }

         
        shinyBS::closeAlert(session, "drillingAlertInstance")
      
    
    
  })
  
  

  
  output$helpMessage <- renderText({"
<p>Click anywhere on the map to display suitability and attribute information for that location.</p>
"})
  
  
######   Download Data   ##########
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      
      if(input$ProductType =='Suitability'){
        fname <- paste0(RV$CurrentRegion, '_', input$ProductName ,'.tif' )
      }else if(input$ProductType =='Attributes') {
        fname <- paste0(RV$CurrentRegion, '_', input$ProductName ,'.tif' )
      }
      fname

    },
    content = function(file) {
      
      if(input$ProductType =='Suitability'){
        fname2 <- paste0(dataRoot, '/', input$Region, '/Suitability/', input$ProductName ,'.tif' )
      }else if(input$ProductType =='Attributes') {
        fname2 <- paste0(dataRoot, '/', input$Region, '/Attributes/', input$ProductName ,'.tif' )
      }
     file.copy(fname2, file)
    }
  )
  
  
  output$downloadDrill <- downloadHandler(
    
    filename = function() {

        fname <- paste0(str_replace_all(input$DrillReturn, ' ', '_'), '_for_', RV$Lon, '_', RV$Lat, '.csv')
        fname
      
    },
    content = function(file) {
        write.table(RV$currentDrill, file, col.names = F, row.names = F, quote = F, sep=',')
    }
  )
  
  
  
  output$htmlviewer <- renderText({
    try(
        if(input$ProductType =='Attributes'){
          
        #return(paste('<iframe style="height:600px; width:100%" src="', paste0('http://esoil.io/TERNLandscapes/Public/Products/NthAust/Roper/Reports/ModelReport_A2b1.html'), '"></iframe>', sep = ""))
        html <- paste0('http://esoil.io/TERNLandscapes/Public/Products/NthAust/', input$Region, '/Reports/ModelReport_', input$ProductName ,'.html' )
        includeHTML(html)
        
        }else{
          HTML('<body><br><br><br><br><H1>Model reports are only displayed when you have the "Attributes" selected in the Product Type dropdown box.</H1><body')
        }
    )
  })
  
  output$suitframeworkviewer  = renderRHandsontable({

    rhandsontable(suitFramework,  height = 650, manualColumnResize = T, readOnly = TRUE, rowHeaders = F) %>%
      hot_cols(renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
           Handsontable.renderers.NumericRenderer.apply(this, arguments);
            if (col > 13 & value == 1) {
               td.style.background =  'darkgreen';
               td.style.color= 'white';
            } 
            if (col > 13 & value == 2) {
               td.style.background =  'green';
               td.style.color= 'white';
            } 
            if (col > 13 & value == 3) {
               td.style.background =  'lightgreen';
               td.style.color= 'white';
            } 
            if (col > 13 & value == 4) {
               td.style.background =  'orange';
               td.style.color= 'white';
            } 
            if (col > 13 & value == 5) {
               td.style.background =  'brown';
               td.style.color= 'white';
            } 
           }")
    
  })
  
  
  
  session$onSessionEnded(function() {
    
   
    logSession(startDateTime=sessionStartTime, endDateTime=as.character(Sys.time()))
    
    cat("Session Ended\n")
  })
  
}



#########   UI   #############


ui <- navbarPage(windowTitle = 'Nth Aust Land Suitability Draft Products Viewer', theme = 'bootstrap.css', fluid = FALSE, inverse = TRUE,
                 title = HTML('<a href="https://www.csiro.au/" class="navbar-left" target=_blank>&nbsp;<img src="Logos/CSIRO-logo-inverted.svg" width="60" height="60"></a>&nbsp;&nbsp;<span>&nbsp;&nbsp;Northern Australia Land Suitability Draft Products Viewer</span>'),
                
                 header = tagList(
                   tags$head( # Start <head>
                     tags$link(rel = "stylesheet", type = "text/css", href = "Styles/csiro.css"),
                     # tags$style(HTML("label {  font-size: 12px; }")),
                     #tags$head(tags$script(src = "message-handler.js")),
                     useShinyjs(),
                     useShinyalert(),
                     #add_busy_spinner(spin = "flower", margins = c(0, 0), position='full-page', color = 'red',height = "80px", width = "80px"),
                     tags$link(rel = "icon", href = "favicon.ico"),
                    # tags$style(appCSS),
                     #includeHTML(("GA.html")),
                     tags$style('
#crosshair-button {
position: relative;
top: 10px;
left: 70px;
}')
                   ), 
                   

                   
                   # End </head>
                   # Start <body>
                   # Header above panes (but below the nav)
                   
                 ), # End header taglist
                 # Start tab panel list:
                 #tabPanel("Map View",
                          sidebarLayout(fluid = FALSE,  
                                        sidebarPanel(width = 4, height='850px', 
                                                     div(class = 'container-fluid',
                                                         
                                                         fluidRow(htmlOutput("helpMessage")),
                                                         fluidRow( HTML('&nbsp;<br>')),
                                                         fluidRow(selectInput("Region", "Region ", choices = NULL, selected = 'Roper', width='40%')),
                                                         fluidRow(selectInput("ProductType", "Map Product Type ", choices = NULL, selected = 'Suitability', width='40%')),
                                                         fluidRow(selectInput("ProductName", "Product Name", choices = NULL, selected = 'Suitability', width='100%')),
                                                         fluidRow(selectInput("DrillReturn", "Drill Info to Return", choices = drillChoices, selected = 'Current Layer', width='85%')),
                                                       #  fluidRow(dateInput('mainMapDate', label = 'Map Date', format = 'dd-mm-yyyy', value = maxDate, width = 130)),
                                                         
                                                       
                                                       div(style="height: 27px;",
                                                           sliderInput("mainMapTrans",NULL,min = 0,max = 1, step=0.05,value = 1,ticks=F, label = 'Map Transparency', width='75%')),
                                                       
                                                         
                                                        # fluidRow( column(12, actionLink("init", "Download Map Data", icon = icon("download")))),
                                                         fluidRow( HTML('&nbsp;<br>')),
                                                        # fluidRow( HTML('&nbsp;<br>')),
                                                        # fluidRow(column(12, dateRangeInput('moistureDateRange',label = 'Timeseries Date Range',start = maxDate-TsNumDays, end = maxDate),)),
                                                         
                                                         # conditionalPanel('output.showTSLink == "true"',
                                                         #                  fluidRow( column(12, actionLink("initTS", "Download Pixel Drill Data", icon = icon("download"))))
                                                         # ),
                                                        fluidRow( HTML('&nbsp;<br><br><br><br>')),
                                                        fluidRow(downloadBttn(outputId = "downloadData", label = 'Download Current Raster (geoTiff)', style = "jelly", color = "success")),
                                                        fluidRow( HTML('&nbsp;<br>')), 
                                                        fluidRow(downloadBttn(outputId = "downloadDrill", label = 'Download Current Drill Data (csv)', style = "jelly", color = "success")),
                                                     )
                                        ),
                                        
                                       
                                        mainPanel( width = 8,
                                                   
                                                   tabsetPanel(type = "tabs",
                                                   tabPanel("Map",
                                                   
                                                   fluidRow( column(12,
                                                                    
                                                                    tags$style(' 
                                                                    
                                                                    .easy-button-button .button-state{
                                                                      display: block;
                                                                      width: 100%;
                                                                      height: 100%;
                                                                      position: relative;
                                                                      }
                                                                      
                                                                      .bttn-jelly.bttn-success {
                                                                          background: #003b29;
                                                                          color: #fff;
                                                                          bottom: 12px;
                                                                          left: 20px
                                                                      }
                                                                      
                                                                      .bttn-jelly.bttn-md {
                                                                          FONT-WEIGHT: 100;
                                                                          font-size: 13px;
                                                                          font-family: inherit;
                                                                          padding: 5px 12px;
                                                                      }
                                                                    '),
                                                                    
                                                                    
                                                                  #  if (isTRUE(SHOW_WELCOME_MESSAGE)) div( class = "panel panel-default", div(class = "panel-heading", HTML("Welcome to SMIPS")), div( class = "panel-body", htmlOutput("welcomeMessage"), style=paste0('color:', 'green', '; width: 850px;'))) else div()
                                                   )),
                                                   
                                                   fluidRow(
                                                     column(12,  
                                                            leafletOutput("mainMap", height = 600)
                                                            )),
                                                    # absolutePanel(htmlOutput("wmsLegend"), style=" top: 1px; right:-150px; height:250px;")) ,
                                                    fluidRow(column(12, bsAlert("progressAlert"))),
                                                   fluidRow( column(12, rHandsontableOutput("DrillInfoTable")))
                                                   
                                        ),
                                        
                                        tabPanel("Model Info", "",
                                                 htmlOutput('htmlviewer')
                                                
                                        ),
                                        tabPanel("Suitability Framework", "",
                                                 
                                                 rHandsontableOutput("suitframeworkviewer")
                                                   
                                        ),
                                        
                                        
                                        tabPanel("About", "",
                                                 includeHTML(paste0( "www/StaticPages/About.html"))
                                        )
                                        ))),
                 
                 footer = div(class = 'footer'
                   #           hr(),
                   #            fluidRow( column(12,
                   #                             HTML('<div style="margin: 0px auto 0px auto; width:450px;"><img src="Logos/TERN-NCRIS-digital-Primary.jpg" width="50%"></img></div>')
                   #            )),
                   #            #Trick to escape the fixed-size container for the full-width lower-footer
                   #            HTML('</div><!--close footer--></div><!--close row--></div><!--close container-->
                   # <div class="container-fluid"><div class="row lower-footer"><div class="col-sm-12"><p>&nbsp;<br/>&nbsp;<br/></p>'
                   #                 )
                 )
) 





shinyApp(ui = ui, server = server)


