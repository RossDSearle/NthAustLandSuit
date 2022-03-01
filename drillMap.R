library(stringr)
library(XML)


DrillSuits <- function(lon=NULL, lat=NULL, dataRoot, region, useList){
  cnt=0
  sodf <- data.frame(item=character(), value=character(), stringsAsFactors = F)
  for(i in 1:length(useList)){
    
    use <- useList[i]
    inR <- paste0(dataRoot,'/', region, '/Suitability/', use ,'.tif' )
    
    r <- terra::rast(inR)
    vals <- terra::extract(r, data.frame(lon, lat))
    v <- vals[1,2]
    
    # cmd <- paste0('gdallocationinfo -geoloc -xml ', inR, ' ', lon, ' ', lat)
    # s <- system(cmd, intern = T)
    # xmlStr <- xmlParse(s)
    # 
    # p <- xpathApply(xmlStr , '/Report/BandReport/Value')
    # v <- as.numeric(xmlValue(p))
    
    if(length(v)==0)
      return(NULL)
    
    cnt=cnt+1
    
    sodf[cnt, 1] <- use
    sodf[cnt, 2] <- as.character(v)
    
  }
  return(sodf)
  
}


DrillCurrentLayer <- function(lon=NULL, lat=NULL, dataRoot, currentLayer){
  
  sodf <- data.frame(item=character(), value=character(), stringsAsFactors = F)
    inR <- currentLayer
    
    print(inR)
    
    r <- terra::rast(inR)
    vals <- terra::extract(r, data.frame(lon, lat))
    v <- vals[1,2]
    
    # cmd <- paste0('gdallocationinfo -geoloc -xml ', inR, ' ', lon, ' ', lat)
    # s <- system(cmd, intern = T)
    # xmlStr <- xmlParse(s)
    # 
    # p <- xpathApply(xmlStr , '/Report/BandReport/Value')
    # v <- as.numeric(xmlValue(p))
    
    if(length(v)==0)
      return(NULL)
    
    fn <- str_remove(basename(currentLayer), '.tif')
    
    sodf[1, 1] <- fn
    sodf[1, 2] <- as.character(v)
    
  return(sodf)
  
}



DrillAttributes<- function(lon=NULL, lat=NULL, dataRoot, region, attList){
  cnt=0
  
  sodf <- data.frame(item=character(), value=character(), stringsAsFactors = F)
  for(i in 1:length(attList)){
    
    att <- attList[i]
    inR <- paste0(dataRoot, '/', region, '/Attributes/', att ,'.tif' )
    
    r <- terra::rast(inR)
    vals <- terra::extract(r, data.frame(lon, lat))
    v <- vals[1,2]
    
    # print(inR)
    # cmd <- paste0('gdallocationinfo -geoloc -xml ', inR, ' ', lon, ' ', lat)
    # s <- system(cmd, intern = T)
    # xmlStr <- xmlParse(s)
    # 
    # p <- xpathApply(xmlStr , '/Report/BandReport/Value')
    # v <- as.numeric(xmlValue(p))
    
    if(length(v)==0)
      return(NULL)
    
    cnt=cnt+1
    
    sodf[cnt, 1] <- att
    sodf[cnt, 2] <- sprintf(v, fmt = '%#.2f')
    
  }
  return(sodf)
  
}



drillMap <- function( lon=NULL, lat=NULL, dataRoot, region, useList, attList, what, currentSuit=NULL,  currentLayer=NULL){

  
  if(what=='Current Layer'){
    
    sdf <- DrillCurrentLayer(lon, lat, dataRoot, currentLayer)
    if(is.null(sdf)){return(NULL)}
    
    odf <- data.frame(item=character(), value=character(), stringsAsFactors = F)
    odf[1,1] <- 'LAYER'
    odf[1,2] <- 'VALUE'
    
    fdf <- rbind(odf, sdf)
    return(fdf)
    
  }else if(what=='Everything'){
   
    odf <- data.frame(item=character(), value=character(), stringsAsFactors = F)
    sdf <- DrillSuits(lon, lat, dataRoot, region, useList)
    if(is.null(sdf)){return(NULL)}
    
    odf[1,1] <- 'LANDUSE'
    odf[1,2] <- 'SUITABILITY'
   
    fdf <- rbind(odf, sdf)
    
    adf <- DrillAttributes(lon, lat, dataRoot, region, attList)
    adfH <- data.frame(item=c('', 'ATTRIBUTE'), value=c('', 'VALUE'), stringsAsFactors = F)
    
    tdf <- rbind(fdf, adfH, adf)
    return(tdf)
    
  }else if(what=='All Suitabilites'){
    
    odf <- data.frame(item=character(), value=character(), stringsAsFactors = F)
    odf[1,1] <- 'LANDUSE'
    odf[1,2] <- 'SUITABILITY'
    sdf <- DrillSuits(lon, lat, dataRoot, region, useList)
    fdf <- rbind(odf, sdf)
    return(fdf)
    
  }else if(what=='All Attributes'){
    odf <- data.frame(item=character(), value=character(), stringsAsFactors = F)
    odf[1,1] <- 'ATTRIBUTE'
    odf[1,2] <- 'VALUE'
    sdf <- DrillAttributes(lon, lat, dataRoot, region, attList)
    fdf <- rbind(odf, sdf)
    return(fdf)
  }
  else if(what=='Current Suitability and All Attributes'){
    odf <- data.frame(item=character(), value=character(), stringsAsFactors = F)
    
    useList <- currentSuit
    sdf <- DrillSuits(lon, lat, dataRoot, region, useList)
    if(is.null(sdf)){return(NULL)}
    
    odf[1,1] <- 'LANDUSE'
    odf[1,2] <- 'SUITABILITY'
    
    fdf <- rbind(odf, sdf)
    
    adf <- DrillAttributes(lon, lat, dataRoot, region, attList)
    adfH <- data.frame(item=c('', 'ATTRIBUTE'), value=c('', 'VALUE'), stringsAsFactors = F)
    
    tdf <- rbind(fdf, adfH, adf)
    return(tdf)
  }
    
    
 
}




