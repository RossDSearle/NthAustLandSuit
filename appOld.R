library(raster)
library(stringr)
library(shiny)
library(leaflet)
#library(leaflet.extras)
#library(htmltidy)
library(XML)
library(xml2)
library(dygraphs)
library(xts)
library(mapview)
library(leaflet.extras)
library(htmltools)
library(RCurl)
library(jsonlite)
library(shinyBS)
library(htmltools)
library(rhandsontable)
library(RColorBrewer)
library(zip)
library(shinycssloaders)
library(shinyalert)
library(shinyjs)
library(lubridate)
library(raster)
library(httr)
library(shinyMobile)

#product <- 'Openloop_Volumetric_SM'
useWMS = T

machineName <- as.character(Sys.info()['nodename'])
if (machineName=='soils-discovery') {
  deployDir <<-'/srv/shiny-server/SMIPS'
} else if (machineName=="lw-94-cdc") {
  deployDir <<-'/home/som05d/CODE/SMIPS_Website'
} else {
  deployDir <<-'C:/Users/sea084/Dropbox/RossRCode/Git/Shiny/SMIPS_Website'
}

setwd(deployDir)
source('./SMIPS_Config.R')
source('./appFunctions.R')
source('./appUtils.R')
source("./helpers.R")

#productsDF <- read.csv(paste0(deployDir, '/ConfigFiles/RasterServices_config.csv'), stringsAsFactors=F)
productsDF <- fromJSON(configURL)
productsDF <- productsDF[!startsWith(productsDF$Name, "!"),]
productsDF <- productsDF[order(productsDF$ProductOrder),]
defaultMap= productsDF$Name[2]

server <- function(input, output,session) {
  
  
  RV <- reactiveValues()
  
  RV$currentDownloadTS <- NULL
  RV$AvailableMapProducts <- c(  productsDF$Name)
  RV$RecreateSlider <- TRUE
  RV$Log <- logInfo
  
  SMIPSDrillbtn <- if (isTRUE(ALWAYS_DRILL)) reactiveVal('SmipsDrillOn') else reactiveVal('SmipsDrillOff')
  
  
  ##############      DATA DOWNLOAD FUNCTIONALITY    ############################################ 
  observeEvent(input$initTS, {
    if (is.null(RV$currentTS)) {
      showModal(
        modalDialog(
          title = 'Oops!',
          p("You need to click on the map and generate a pixel timeseries before you can download it.")
        )
      )
    } else {
      
      shinyjs::runjs("document.getElementById('downloadTSData').click();")
    }
  })
  
  output$downloadTSData <- downloadHandler(
    filename = function() {
      product <- input$ProductType
      lon <- sprintf(RV$currentSiteInfo$Longitude, fmt = '%#.4f')
      lat <- sprintf(RV$currentSiteInfo$Latitude, fmt = '%#.4f')
      tsname <- paste0(product, '_', lon, '_', lat,'.csv')
      paste0(tsname)
    },
    content = function(file) {
      
      df <- data.frame(date=index(RV$currentTS), coredata(RV$currentTS))
      colnames(df) <- c('Date', 'Value')
      write.csv( df, file, row.names = FALSE)
    }
  )
  
  
  observeEvent(input$init, {
    if (input$ProductType == 'None') {
      showModal(
        modalDialog(
          title = 'Oops!',
          p("You don't have any maps displayed. You need to select a map to display before you can download it.")
        )
      )
    } else {
      shinyjs::runjs("document.getElementById('downloadRasterData').click();")
    }
  }) 
  output$downloadRasterData <- downloadHandler(
    
    filename = function() {
      
      product <- input$ProductType
      mDate <- input$moistureMapDate
      rname <- paste0(product, '_', mDate , '.tif')
      
    },
    content = function(file) {
      
      if(input$ProductType != 'None'){
        
        shinyBS::createAlert(session, "alert", "waitalert", title = "", content = paste0("<div id='zs1'><img src=wait.gif>&nbsp;&nbsp;Downloading data", " .....</div>"), append = FALSE, dismiss = F)
        
        product <- input$ProductType
        mDate <- input$moistureMapDate
        prodCode <- productsDF[productsDF$Name==product,]$ProductCode
        rname <- paste0(product, '_', mDate , '.tif')
        
        url <-  paste0('http://esoil.io/thredds/wcs/SMIPSall/SMIPSv0.5.nc?SERVICE=WCS&VERSION=1.0.0&REQUEST=GetCoverage&FORMAT=GeoTIFF_Float&COVERAGE=', prodCode, '&CRS=OGC:CRS84&TIME=', mDate, 'T00:00:00Z')
        #res <- paste0('&RESX=', pres, '&RESY=', pres)
        bbox <- paste0(input$moistureMap_bounds$west, ',', input$moistureMap_bounds$south, ',',input$moistureMap_bounds$east, ',',input$moistureMap_bounds$north )
        
        outFile <- paste0(tempfile(), '.tif')
        download.file(URLencode(paste0(url, '&BBOX=', bbox)), outFile, mode = 'wb', quiet = T)
        shinyBS::closeAlert(session, "waitalert")
        file.copy(outFile, file)
      }else{
      }
      
    }
  )
  
  
  ################### MAP FUNCTIONS  ################# 
  ################### Render the Map  #################
  
  
  observe({
    req(input$SMIPSDrillbtn)
    if (!is.null(input$SMIPSDrillbtn)) {
      SMIPSDrillbtn(input$SMIPSDrillbtn)
    } })
  
  output$moistureMap <- renderLeaflet({
    
   # req(input$ProductType)
    
    lf <- leaflet() %>% clearMarkers() %>%
      addTiles(group = "Map") %>%
      addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Satelite Image") %>%
      setView(lng = 135, lat = -28, zoom = 4)
    
    if (isTRUE(SHOW_GPS_BUTTON)) {
      lf <- addControlGPS(lf, options = gpsOptions(setView=T, autoCenter=T, maxZoom=10))
    }
    sbo <- scaleBarOptions(maxWidth = 200, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE)
    lf <- addLayersControl(lf,
                           baseGroups = c("Map", "Satelite Image"),
                           overlayGroups = ifelse(isTRUE(ENABLE_SITES), c("Moisture Maps", "All Sensors"), c("Moisture Maps")),
                           options = layersControlOptions(collapsed = FALSE)
    ) %>%
      addScaleBar(position = "bottomleft", options = sbo)
    # addWMSLegend(uri = paste0(wmsServer, '.nc?VERSION=1.3.0&layer=', prod, '&REQUEST=GetLegendGraphic&FORMAT=image/png'),  position =  "bottomright")
    
    
    lf <- if (isTRUE(ALWAYS_DRILL)) lf else addEasyButtonBar(lf, position = "topright" ,
                                                             easyButton( position = "topright" ,
                                                                         states = list(
                                                                           easyButtonState(
                                                                             stateName="sOff",
                                                                             #icon="fa-check",
                                                                             #icon=htmltools::span(class = "star", htmltools::HTML('<img src="Buttons/drill.png"></font>')),
                                                                             icon=htmltools::span(class = "star", htmltools::HTML('<font color="gray">Get&nbsp;Timeseries&nbsp;<b>Off</b></font>')),
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
                                                                             icon=htmltools::span(class = "star", htmltools::HTML('<font color="green">Get&nbsp;Timeseries&nbsp;<b>On</b></font>')),
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
            var moistureMap = this;
            window.moistureMap = moistureMap;
            $('.leaflet-container').css('cursor','crosshair');
          }")
    else
      htmlwidgets::onRender(lf, "
          function(el, x) {
            var moistureMap = this;
            window.moistureMap = moistureMap;
          }")
    
    
  })
  
  
  
  ###################### Use a separate observer to recreate wms maps as needed  ######
  observe({
    
    req(input$ProductType)
    
    mDate <- input$moistureMapDate
    wmsyear <- paste0( substr(mDate, 1, 4))
    rec <- productsDF[productsDF$Name==input$ProductType,][1,]
    org <- rec$Organisation
    prod <- rec$ProductCode
    wmsServer <- rec$Server
    #wmsStyle <- rec$wmsStyle
    wmsVersion <- rec$wmsVersion
    attrib <- rec$Attribution
    
    trans <- input$moistureMapTrans
    if (is.null(trans)) { trans <- 1.0 } # Trans defaults to NULL if the slider is not created yet
    proxy <- leafletProxy("moistureMap", data = RV$sensorLocs) %>%
      clearGroup("Moisture Maps")
    if(input$ProductType == 'None'){
      if (RV$RecreateSlider == FALSE) {
        proxy <- removeControl(proxy,"overlay_transparency_slider")
        RV$RecreateSlider <- TRUE
      }
    }else{
      
      dbits <- str_split(mDate, '-')
      dt <- paste0(dbits[[1]][3], '-', dbits[[1]][2], '-', dbits[[1]][1])
      
      #proxy %>% clearControls()
      
      if(useWMS) {
        
        if(org == 'CSIRO'){
          #wmsurl <- paste0(wmsServer, '.nc?TIME=', mDate, 'T00:00:00.000Z&numcolorbands=9&colorscalerange=0,1&belowmincolor=transparent&abovemaxcolor=extend')
          wmsurl <- paste0(wmsServer, '.nc?TIME=', mDate, 'T00:00:00.000Z&belowmincolor=transparent')
          
        }else if(org == 'BoM'){
          wmsurl <- paste0(wmsServer, '/', prod, '_', wmsyear, '.nc?time=', mDate)
          
        }else if(org == 'ANU'){
          #http://dapds00.nci.org.au/thredds/wms/ub8/au/OzWALD/daily/OzWALD.daily.Ssoil.2018.nc?&SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap&layers=Ssoil&CRS=CRS:84&bbox=112.0,-44,154.0,-10.0&format=image/png&time=2018-06-13T00:00:00.000Z&styles=&TRANSPARENT=true&width=400&height=300&style=boxfill/anu_wald_darkblues&transparent=true
          wmsServer <- sub("<prod>", prod, wmsServer, fixed=TRUE)
          wmsServer <- sub("<year>", wmsyear, wmsServer, fixed=TRUE)
          wmsurl <- paste0(wmsServer, '.nc?time=', mDate, 'T00:00:00.000Z&colorscalerange=0,1800&belowmincolor=transparent&abovemaxcolor=extend')
        }
        
        proxy <- addWMSTiles(proxy, wmsurl, group= "Moisture Maps",
                             layers = prod,
                             #options = WMSTileOptions(format = "image/png", transparent = T, opacity = trans,  styles = wmsStyle, version = wmsVersion),
                             options = WMSTileOptions(format = "image/png", transparent = T, opacity = trans, version = wmsVersion),
                             attribution = attrib)
        
      }else{
        bds <- input$moistureMap_bounds
        url <- paste0(SMIPSAPIServer, '/RasterWindow?bbox=',max(bds$west, Ausminx),'%3B', min(bds$east, Ausmaxx),'%3B', max(bds$south, Ausminy),'%3B', min(bds$north, Ausmaxy) ,'&date=', dt, '&rows=',wmsnumrows,'&cols=', wmsnumcols)
        mr <- getRasterFromAPI(url)
        pal <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), seq(0, 1, .1),na.color = "transparent")
        pal_rev <- colorNumeric(c("#FFFFCC", "#41B6C4", "#0C2C84"), seq(0, 1, .1),na.color = "transparent", reverse = TRUE)
        
        proxy <- addRasterImage(proxy, mr, colors = pal, opacity = 0.8, group = "Moisture Maps") %>%
          leaflet::addLegend(pal = pal_rev, values = seq(0, 1, .1), title = "Moisture Map", labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
      }
      if (RV$RecreateSlider == TRUE) {
        sliderCtrl <- div(id = "slider_move_catcher", sliderInput("moistureMapTrans", width=220, label = HTML('<div style="width: 180px; max-height: 10px; position: absolute; margin: 0; left: 70px; top: 20px; text-shadow: 0px 0px 2px white; color: #444; font-size: 13px;">Overlay Opacity</div>'), ticks = F, min = 0.0,  max = 1.0, value = trans, animate = FALSE),
                          onmouseover = "window.moistureMap.dragging.disable();Shiny.onInputChange('SMIPSDrillbtn', 'SmipsDrillOff'); $('.leaflet-container').css('cursor','');", onmouseout = "window.moistureMap.dragging.enable();Shiny.onInputChange('SMIPSDrillbtn', 'SmipsDrillOn');$('.leaflet-container').css('cursor','crosshair');")
        proxy <- addControl(proxy, position = "bottomright", layerId = "overlay_transparency_slider", className = "info", sliderCtrl)
        RV$RecreateSlider <- FALSE
      }
    }
  })     
  
  
  
  output$wmsLegend <- renderUI({
    if(input$ProductType != 'None'){
      rec <- productsDF[productsDF$Name==input$ProductType,][1,]
      org <- rec$Organisation
      prod <- rec$ProductCode
      wmsServer <- rec$Server
      wmsStyle <- rec$wmsStyle
      wmsVersion <- rec$wmsVersion
      if (org == "CSIRO") {
        wmsServer <- paste0(wmsServer, ".nc")
        wmsStyle <- "sm_blues"
      } else {
        parts <- strsplit(wmsStyle, '/', fixed=TRUE)[[1]]
        
        wmsStyle <- parts[length(parts)]
      }
      
      imageLoc <- paste0(wmsServer, "?VERSION=", wmsVersion, "&SERVICE=WMS&REQUEST=GetLegendGraphic&COLORBARONLY=true&WIDTH=2&HEIGHT=648&LAYER=",prod,"&PALETTE=",wmsStyle,"&FORMAT=image%2Fpng")
      #imageLoc <- './Legends/ANU_Ssoil_Legend.png'
      div_style <- paste0("box-sizing: content-box; border: 1px solid black; width: 18px; height: 648px; background-image: url(\'", imageLoc, "\');")
      div(style=div_style)
    } else {
      div()
    }
  })
  
  
  # output$wmsLegend <- renderImage({
  #   if(input$ProductType != 'None'){
  #
  #     # rec <- productsDF[productsDF$Name==input$ProductType,][1,]
  #     #  org <- rec$Organisation
  #     #  prod <- rec$ProductCode
  #     #  cproduct <- paste0(org, '_', prod)
  #     # legendurl <- paste0(deployDir, '/www/Legends/','CSIRO_Volumetric-Moisture', '_Legend.png')
  #
  #     legendurl <- 'http://esoil.io/thredds/wms/SMIPSall/SMIPSv0.5.nc?VERSION=1.3.0&SERVICE=WMS&REQUEST=GetLegendGraphic&LAYER=Openloop_Wetness_Index&FORMAT=image%2Fpng'
  #
  #     #tf <- tempfile(fileext = '.png')
  #     download.file(legendurl, destfile = '/tmp/a.png', mode = 'wb')
  #     # legendurl <- 'http://dapds00.nci.org.au/thredds/wms/ub8/au/OzWALD/daily/OzWALD.daily.Ssoil.2017.nc?REQUEST=GetLegendGraphic&LAYER=Ssoil&PALETTE=nrm_temperature_reverse'
  #
  # list(
  #    src = "C:/Users/sea084/Dropbox/RossRCode/Git/Shiny/SMIPS/www/Legends/ANU_Ssoil_Legend.png",
  #   #src = 'http://esoil.io/thredds/wms/SMIPSall/SMIPSv0.5.nc?VERSION=1.3.0&SERVICE=WMS&REQUEST=GetLegendGraphic&COLORBARONLY=True&LAYER=Openloop_Wetness_Index&FORMAT=image%2Fpng',
  #   filetype = "image/png",
  #   alt = "Legend for the currently displayed map" )
  # }
  #   else{
  #     return(list(
  #       #src = "/srv/shiny-server/SMIPS/www/Legends/CSIRO_Wetness-Index_Legend.png",
  #       src = "",
  #       filetype = "image/png",
  #       alt = "" ))
  #    }
  #
  # }, deleteFile = FALSE)
  
  
  
  ##############      MAP QUERYING FUNCTIONS  ##############################
  ################## Render the Chart from a map drill  ##################
  output$dummyDygraph <- renderDygraph({})
  output$moisitureChart1 <- renderDygraph({
    
    
    if(!is.null(RV$currentTS)){
      
      isolate({
        
        maxVal <- max(RV$currentTS)
        
        dygraph(RV$currentTS ,  main=paste0(input$ProductType), ylab=RV$currentSiteInfo$DataType, width=50, height=100) %>%
          dyAxis("y", label = RV$currentSiteInfo$DataType, valueRange = c(0, maxVal)) %>%
          dyOptions(axisLineWidth = 1.5, fillGraph = T, drawGrid = T, titleHeight = 25) %>%
          dyRangeSelector()
        
      })
    }
  })
  output$showDyGraph <- reactive({ifelse(is.null(RV$currentTS), "false", "true")})
  output$showTSLink <- reactive({ifelse(is.null(RV$currentTS), "false", "true")})
  
  ################## Render the Info Table from a map drill  ##################
  output$SensorDrillInfoTable = renderRHandsontable({
    
    req(RV$currentSiteInfo)
    d <- RV$currentSiteInfo
    
    flds <- names(d)
    vls <- do.call( rbind, d)[,1]
    newdf <- data.frame(flds, vls, row.names=NULL, stringsAsFactors = F)
    
    
    if(nrow(newdf) > 0){
      colnames(newdf) <- c('.', 'Pixel Drill Information') 
      rhandsontable(newdf,   manualColumnResize = T, readOnly = TRUE, rowHeaders = F, width = 850) %>%
        hot_cols(colWidths= c(100, 750))
    }else{
      return(NULL)
    }
  })
  
  
  
  
  
  
  ################  Get data from drilling a map pixel ##########
  observeEvent(input$moistureMap_click, {
    
    click <- input$moistureMap_click
    
    if(is.null(click))
      return()
    
    req(SMIPSDrillbtn)
    if(SMIPSDrillbtn() == 'SmipsDrillOn')
      
      if(input$ProductType != 'None'){
        
        cproduct <- productsDF[productsDF$Name==input$ProductType,]$ProductCode
        
        minDateShiny <- input$moistureDateRange[[1]][1]
        maxDateShiny  <- input$moistureDateRange[[2]][1]
        sd <- str_split(minDateShiny, '-')
        minDate <- paste0(sd[[1]][3], '-', sd[[1]][2], '-', sd[[1]][1])
        ed <- str_split(maxDateShiny, '-')
        maxDate <- paste0(ed[[1]][3], '-', ed[[1]][2], '-', ed[[1]][1])
        
        shinyBS::createAlert(session, "progressAlert", "drillingAlertInstance", title = "", content = paste0('<img src=wait.gif>&nbsp;&nbsp;Retrieving timeseries data from <font color=green>', input$ProductType, '</font> at location ', sprintf(click$lng, fmt = '%#.4f'), " ", sprintf(click$lat, fmt = '%#.4f') ), append = FALSE)
        urlSMIPS <- paste0('http://esoil.io/SMIPS_API/SMIPS/TimeSeries?product=', input$ProductType ,'&longitude=',click$lng ,'&latitude=', click$lat ,'&sdate=', minDate ,'&edate=', maxDate)
        
        
        resp <- getURL(URLencode(urlSMIPS))
        
        is_error <- grepl('"error":', substr(resp, 1, 9), fixed=TRUE)
        if (isTRUE(is_error))
          return()
        
        ts <- convertJSONtoTS(resp, "%Y-%m-%dT%H:%M:%S")
        
        SI <- fromJSON(resp)
        
        if(!is.null(SI) > 0){
          
          RV$currentTS <- ts
          
          sli <- list()
          # sli$SiteID <- SI$SiteID[1]
          # sli$SiteName <- SI$SiteName[1]
          # sli$Provider <- SI$Provider [1]
          # sli$Backend <- SI$Backend[1]
          # sli$Access  <- SI$Access [1]
          sli$Longitude <- SI$Longitude[1]
          sli$Latitude  <- SI$Latitude[1]
          #sli$Active  <- "True"
          #sli$Owner <- SI$Provider [1]
          #sli$Contact <- SI$Contact[1]
          sli$ProviderURL <- urlSMIPS
          sli$StartDate <- originDate
          sli$EndDate <- format(Sys.Date(), format = "%d-%m-%Y")
          sli$Units <- SI$Units
          #sli$Description <- SI$Description[1]
          #sli$DataType <- 'Modelled Soil-Moisture'
          
          RV$currentSiteInfo <- sli
          
        }else{
          RV$currentSiteInfo <- NULL
          RV$currentTS <- NULL
        }
        
        shinyBS::closeAlert(session, "drillingAlertInstance")
      }
    
    
  })
  
  
  ############################    DYNAMIC IU FUNCTIONS   ##############
  ################ Update List Choices   ###############################
  # update the MapProduct List choices depending on login  
  observe({
    updateSelectInput(session, "ProductType", choices =  RV$AvailableMapProducts, selected = defaultMap )
    updateSelectInput(session, "MapProductsListForDownload", choices =  RV$AvailableMapProducts[-1] )
  })
  
  # update the site SelectedSiteDataDownload  list  
  observe({
    req(RV$sensorLocs)
    updateSelectInput(session, "SelectedSiteDataDownload",choices = RV$sensorLocs$SiteName )
  })
  
  
  
  output$helpMessage <- renderText({"
<p>Click anywhere on the map to display a modelled soil moisture timeseries chart for the selected modelled product. The timeseries will be displayed below the map.</p>
"})
  
  
  output$siteTimeSeriesTable  = renderRHandsontable({
    req(RV$currentDownloadTS)
    rhandsontable(RV$currentDownloadTS,  height = 600, manualColumnResize = T, readOnly = TRUE, rowHeaders = F)
  })
  
  
  
  if (isTRUE(ENABLE_SITES)) {
    observe ({
      if((input$toggleSensorSettingsInfo  %% 2) == 1){
        updateActionButton(session, "toggleSensorSettingsInfo", label = "Hide Sensor Settings")
      }else{
        updateActionButton(session, "toggleSensorSettingsInfo", label = "Show Sensor Settings")
      }
    })
  }
}


ui <- navbarPage(windowTitle = 'National Soil Moisture Info', theme = 'bootstrap.css', fluid = FALSE, inverse = TRUE,
                 title = HTML('<a href="https://www.csiro.au/" class="navbar-left" target=_blank>&nbsp;<img src="Logos/CSIRO-logo-inverted.svg" width="60" height="60"></a>&nbsp;&nbsp;<a href="https://www.tern.org.au/" class="navbar-left" target=_blank><img src="Logos/tern1-inverted.png" height="65"></img></a><span>&nbsp;&nbsp;National Soil Moisture Info</span>'),
                 header = tagList(
                   tags$head( # Start <head>
                     tags$link(rel = "stylesheet", type = "text/css", href = "Styles/csiro.css"),
                     # tags$style(HTML("label {  font-size: 12px; }")),
                     #tags$head(tags$script(src = "message-handler.js")),
                     useShinyjs(),
                     tags$style(appCSS),
                     tags$style('
#crosshair-button {
position: relative;
top: 10px;
left: 70px;
}')
                   ), 
                   
                   tags$style(
                     
                     HTML(".alert {
                         background-color: #F5F5F5;
                         padding: 0px; margin-bottom: 10px;
                         color: black;
                         height: 50px;
                     } 
             
             .alert-info {
             border-color: #F5F5F5;
             }"
                     )
                   )
                   
                   # End </head>
                   # Start <body>
                   # Header above panes (but below the nav)
                   
                 ), # End header taglist
                 # Start tab panel list:
                 tabPanel("Map View",
                          sidebarLayout(fluid = FALSE,
                                        sidebarPanel(width = 3,
                                                     div(class = 'container-fluid',
                                                         
                                                         fluidRow(htmlOutput("helpMessage")),
                                                         fluidRow( HTML('&nbsp;<br>')),
                                                         fluidRow(selectInput("ProductType", "Map Product Type ", c('None'), selected = defaultMap, width = "100%")),
                                                         fluidRow(dateInput('moistureMapDate', label = 'Map Date', format = 'dd-mm-yyyy', value = Sys.Date()-defaultDisplayDate, width = 130)),
                                                         
                                                         
                                                         fluidRow( column(12, actionLink("init", "Download Map Data", icon = icon("download")))),
                                                         fluidRow( HTML('&nbsp;<br>')),
                                                         fluidRow( HTML('&nbsp;<br>')),
                                                         fluidRow(column(12, dateRangeInput('moistureDateRange',label = 'Timeseries Date Range',start = Sys.Date()-TsNumDays, end = Sys.Date()),)),
                                                         
                                                         conditionalPanel('output.showTSLink == "true"',
                                                                          fluidRow( column(12, actionLink("initTS", "Download Pixel Drill Data", icon = icon("download"))))
                                                         ),
                                                         
                                                         downloadButton("downloadRasterData", "Download", style = "visibility: hidden;"),
                                                         downloadButton("downloadTSData", "DownloadTS", style = "visibility: hidden;"),
                                                         #fluidRow(div(style = "text-align:left;", uiOutput("wWCS")))
                                                         fluidRow(div(style = "valign:top; height:50px;background-color: #F5F5F5;", bsAlert("alert"))),
                                                         fluidRow( HTML('&nbsp;<br>')),
                                                         fluidRow( HTML('&nbsp;<br>')),
                                                     )
                                        ),
                                        
                                        
                                        mainPanel( width = 9,
                                                   
                                                   
                                                   fluidRow( column(12,
                                                                    
                                                                    tags$style(' .leaflet-bar button,
.leaflet-bar button:hover {
background-color: #fff;
border: none;
border-bottom: 1px solid #ccc;
width: 120px;
height: 30px;
line-height: 30px;
display: block;
text-align: left;
text-decoration: none;
color: black;
}

.leaflet-bar button {
background-position: 50% 50%;
background-repeat: no-repeat;
overflow: hidden;
display: block;
}

.leaflet-bar button:hover {
background-color: #f4f4f4;
}

.leaflet-bar button:first-of-type {
border-top-left-radius: 10px;
border-top-right-radius: 10px;
}

.leaflet-bar button:last-of-type {
border-bottom-left-radius: 10px;
border-bottom-right-radius: 10px;
border-bottom: none;
}

.leaflet-bar.disabled,
.leaflet-bar button.disabled {
cursor: default;
pointer-events: none;
opacity: .4;
}

.easy-button-button .button-state{
display: block;
width: 100%;
height: 100%;
position: relative;
}


.leaflet-touch .leaflet-bar button {
width: 120px;
height: 30px;
line-height: 30px;

.leaflet-container.crosshair-cursor-enabled {
cursor:crosshair;
}

}'
                                                                    ),
                                                                    
                                                                    if (isTRUE(SHOW_WELCOME_MESSAGE)) div( class = "panel panel-default", div(class = "panel-heading", HTML("Welcome to SMIPS")), div( class = "panel-body", htmlOutput("welcomeMessage"), style=paste0('color:', 'green', '; width: 850px;'))) else div()
                                                   )),
                                                   
                                                   fluidRow(
                                                     column(12, withSpinner( leafletOutput("moistureMap", height = 650))),
                                                     absolutePanel(htmlOutput("wmsLegend"), style="right:-5px; height:650px;")) ,
                                                   fluidRow(column(12, bsAlert("progressAlert"))),
                                                   dygraphOutput("dummyDygraph", height = "0px"), #Dummy dygraph to ensure loading of dygraph libraries at boot time, even when none are shown
                                                   div(style="visibility:hidden;",verbatimTextOutput("showDyGraph")), # Dummy invisible output of showDygraph to ensure its included in the conditional render
                                                   
                                                   conditionalPanel('output.showDyGraph == "true"',
                                                                    fluidRow( column(12, withSpinner( dygraphOutput("moisitureChart1", height = "200px")))),
                                                                    fluidRow( HTML("<BR><BR>")),
                                                                    fluidRow( column(12, rHandsontableOutput("SensorDrillInfoTable")))
                                                   ),
                                                   div(style="visibility:hidden;",verbatimTextOutput("showTSLink")), 
                                        ))),
                 
                 tabPanel("About", "",
                          includeHTML2(paste0( "www/StaticPages/About.html"))
                 ),
                 footer = div(class = 'footer',
                              hr(),
                              fluidRow( column(12,
                                               HTML('<div style="margin: 0px auto 0px auto; width:450px;"><img src="Logos/TERN-NCRIS-digital-Primary.jpg" width="50%"></img></div>')
                              )),
                              #Trick to escape the fixed-size container for the full-width lower-footer
                              HTML('</div><!--close footer--></div><!--close row--></div><!--close container-->
                   <div class="container-fluid"><div class="row lower-footer"><div class="col-sm-12"><p>&nbsp;<br/>&nbsp;<br/></p>')
                 )
) 





shinyApp(ui = ui, server = server)


