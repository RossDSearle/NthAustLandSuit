library(tictoc)
library(raster)
library(jsonlite)
library(lubridate)
library(stringr)
library(XML)
library(xts)


getMaxDate <- function(smipsRoot){
  
  
  fls <- list.files(paste0(smipsRoot, '/totalbucket'), recursive = T, full.names = T)
  f <- basename(tail(fls, 1))
  bits <- str_split(f, '_')
  s <- bits[[1]][4]
  d <- str_sub(s, 7,8)
  m <- str_sub(s, 5,6)
  y <- str_sub(s, 1,4)
  
  dt <- as.Date(paste0(d, '-', m, '-', y), format="%d-%m-%Y")
  
  return(dt)
  
}

drillSMIPS <- function( lon=NULL, lat=NULL, product=NULL, sdate=format(Sys.Date(),  "%d-%m-%Y"), edate=format(Sys.Date(),  "%d-%m-%Y"), smipsRoot){

  dts <- seq.Date(as.Date(sdate, "%d-%m-%Y"), as.Date(edate, "%d-%m-%Y"), by='day')
  b <- vector(mode = "numeric", length = length(dts))
  ots <- xts(x=b, order.by = dts)

    for(i in 1:length(dts)){
     #print(i)
      yr <- lubridate::year(dts[i])
      mth <- str_pad(lubridate::month(dts[i]), side = 'left', width=2, pad='0')
      dy <- str_pad(lubridate::day(dts[i]), side = 'left', width=2, pad='0')
      
      inR <- paste0(smipsRoot, '/', product, '/', yr, '/smips_', product, '_mm_', yr, mth, dy, '.tif')
  
      if(file.exists(inR)){
          
          cmd <- paste0('gdallocationinfo -geoloc -xml ', inR, ' ', lon, ' ', lat)
          s <- system(cmd, intern = T)
          xmlStr <- xmlParse(s)
          
          p <- xpathApply(xmlStr , '/Report/BandReport/Value')
          v <- as.numeric(xmlValue(p))
          ots[dts[i]] <- v
      }
    
    }
  return(ots)
}




logDownload <- function(dateTime,	product,	type,	minX,	minY,	maxX,	maxY,	size){
  
  logDir <- '/mnt/data/APILogs/Roper'
 
  lf <- paste0(logDir, '/Roper_Download_Log.csv')
  if(!file.exists(lf)){
    cat(file = lf, sep = '', 	'dateTime,product,type,minX,minY,maxX,maxY,size\n', append = F)
  }
  cat(file = lf, sep = '', dateTime,',',	product,',',	type,',',	minX,',',	minY,',',	maxX,',',	maxY,',',	size, '\n', append = T)
}

logSession <- function(startDateTime, endDateTime){
  
  logDir <- '/mnt/data/APILogs/Roper'
  
  lf <- paste0(logDir, '/Roper_Session_Log.csv')
  if(!file.exists(lf)){
    cat(file = lf, sep = '', 	'startDateTime,endDateTime\n', append = F)
  }
  cat(file = lf, sep = '', startDateTime, ',', endDateTime, '\n', append = T)
}

