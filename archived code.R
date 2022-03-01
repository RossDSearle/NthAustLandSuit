getSMIPSrasterCSIRO_OpenDAP <- function(product, dt, minx, miny, maxx, maxy){
  
  wmsnumrows <- 348
  wmsnumcols <- 412
  
  xext = maxx - minx
  yext = maxy - miny
  
  #stridex <- ceiling(xext / ( AusRes * wmsnumcols))
  stridey <- ceiling(yext / ( AusRes * wmsnumrows))
  
  ll <- getcellsForALatLon(minx, miny)
  ur <- getcellsForALatLon(maxx, maxy)
  
  subcols <- ceiling( c((ur$colNum-1) - ll$colNum) / stridey )
  subrows <- ceiling( c((ll$rowNum-1) - ur$rowNum) / stridey )
  
  dayNum = as.numeric(as.Date(paste(dt), "%Y-%m-%d") - as.Date(paste(originDate), "%d-%m-%Y"))
  
  
  url <- paste0('http://esoil.io/thredds/dodsC/SMIPSall/SMIPSv0.5.nc.ascii?',product, '%5B',dayNum ,'%5D%5B', ur$rowNum-1, ':', stridey, ':', ll$rowNum-1, '%5D%5B', ll$colNum-1, ':', stridey, ':', ur$colNum-1, '%5D')
  
  #url <- paste0('http://esoil.io/thredds/dodsC/SMIPSall/SMIPSv0.5.nc.ascii?',product,%5B1%5D%5B0:1:10%5D%5B0:1:10%5D
  #  url <-  "http://esoil.io/thredds/dodsC/SMIPSall/SMIPSv0.5.nc.ascii?Openloop_Wetness_Index%5B0:1:0%5D%5B0:1:0%5D%5B0:1:0%5D" 
  #  http://esoil.io/thredds/dodsC/SMIPSall/SMIPSv0.5.nc
  
  
  d1 <- getURI(url)
  
  odData1 <- read.table(text=d1, skip=12, nrows = subrows , sep = ',')
  odData2 <- odData1[,-1]
  m1 <- as.matrix(odData2)
  
  r <- raster(nrows=nrow(odData2), ncols=ncol(odData2), xmn=minx, xmx=maxx, ymn=miny, ymx=maxy, crs=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),  vals=m1)
  
  return(r)
}
