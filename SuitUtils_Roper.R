
#################################
###  Author : Ross Searle         
###  Date : Wed Jul 29 11:26:09 2020                      
###  Project : Roper Land Suitability Assessment
###  Purpose : Contains all the suitability assessment crunching for the Roper project.
###            This is the main file used fro processing
#################################

library(raster)
library(stringr)
library(rgdal)
library(readxl)



rasterOptions(datatype="FLT4S", timer=TRUE, format='GTiff',progress="text",chunksize=1e+08,maxmemory=1e+09, overwrite=TRUE) # maxmemory = max no of cells to read into memory



getSuitList <- function(suitFramework){
 rawn <- colnames(suitFramework)[15:ncol(suitFramework)]
 #rawn2 <- str_replace_all(rawn, ',', '')
 #rawn3 <- str_replace_all(rawn2, ' ', '-')
 return(rawn)
}

getLimitationsPaths <- function(mapFile){
  df <- as.data.frame(read_excel(paste0( mapFile), col_names = T))
  return(df)
}

readSuitFramework <- function(suitFile, sheet, classFile ){
  print(suitFile)
  df <- read_excel(paste0(suitFile), sheet = sheet, col_names = F)
  ncols <- dim(df)[2]
  nrows <- dim(df)[1]
  startCol = 6
  
  cropNumbers <- as.numeric(unlist(as.list(df[1, 6:(ncols)])))
  cropTypes <- str_replace(str_trim(unlist(as.list(df[3, 6:(ncols)]))), ' ', '-')
  cropNames <- str_replace(str_trim(unlist(as.list(df[4, 6:(ncols)]))), ' ', '-')
  cropSeasons <- str_replace(str_trim(unlist(as.list(df[5, 6:(ncols)]))), ' ', '-')
  cropIrrigTypes <- str_replace(str_trim(unlist(as.list(df[6, 6:(ncols)]))), ' ', '-')
  
  fullNames <- paste(cropNames, cropSeasons, cropIrrigTypes, sep='!')
  
  suBclasses <- df[9:nrows, ]
  namesPre <- c('LIM_CODE',	'LIM_Name',	'LIM_VALUE_Desc_FGARA',	'LIM_VALUE',	'LIM_VALUE_Description')
  colnames(suBclasses) <- c(namesPre, fullNames)
  
  
  drops<-c(1,2,3,5)
  
  limClasses <- as.data.frame(read_excel(classFile), col_names = T)
  subClasses <- as.data.frame(suBclasses)
  suits <- merge(x = limClasses, y = subClasses[-drops] , by.x="LIM_VALUE", by.y = "LIM_VALUE", all.x=T, all.y=F)
  
  return (suits)
  
}


getChunkInfo <- function(chunkLines, tot){
  
  remainder <- tot%%chunkLines
  
  if(remainder > 0){
    chunks <- ceiling(tot/chunkLines)
    starts <- seq.int(from=1,length=chunks,by=chunkLines)
    nlines <- rep(chunkLines, length(starts))
    nlines[length(nlines)] <- remainder
    
  }else{
    chunks <- tot/chunkLines
    starts <- seq.int(from=1,length=chunks,by=chunkLines)
    nlines <- rep(chunkLines, length(starts))
  }
  
  return(list(chunks=length(starts), starts=starts, nlines=nlines))
}

makeReps <- function(att, i, limRoot){

  templateR <- raster(paste0(limRoot, '/Maps/Rock2w/Rock2w.tif'))  # an arbitrary template raster
  rp <- paste0(limRoot, '/ClimateAndReliefMasked/', singleRastersDF[singleRastersDF$rnames==att,]$rnames, '.tif' )
  print(rp)
  r <- raster(rp)
  chk <- getChunkInfo(20, nrow(r))
  #print(chk$starts[i])
  #print(chk$nlines[i])
  
  vAll <- getValues(r, row=chk$starts[i], nrows=chk$nlines[i] )
  
  idxvals <- getValues(templateR, row=chk$starts[i], nrows=chk$nlines[i] )
  idx <- which(!is.na(idxvals))
  
  v<-vAll[idx]
  ids <- idx
 # print(head(v))
  d <- cbind(ids, v)
  for (j in 1:499) {
    d <- cbind(d, as.data.frame(v))
  }
  colnames(d)[1] <- 'valsi'
  h <- seq(2, 501, 1)
  colnames(d)[2:501] <- paste0('v', h)
  return(as.matrix(d))
}


singleRasters <- c('Climate-annual_rainfall', 'HotDry','HotWet', 'Frost','Cool', 'Slope','Elevation')
rnames <- c('Rain', 'tmax_DryGreaterThan40_Avg', 'tmax_WetGreaterThan40_Avg', 'tmin_LessThan2Count_Avg', 'tmin_MonthsLessThan15_Avg', 'slopedeg_1s_Roper','Relief_demcrop')
singleRastersDF <- data.frame(singleRasters, rnames, stringsAsFactors = F)

calcSubClasses <- function(k, lim, uses, suitFramework, limRastersList, limInfo, limRoot){
  
  outRoot <- '/datasets/work/af-digiscapesm/work/Ross/Roper'
  
  print('Loading data')
  limMappping1 <- limRastersList[limRastersList$Limitation == lim,]
  print(paste0('Attribute = ', limMappping1[1,3]))
  if(limMappping1[1,3] %in% singleRastersDF$rnames ){
    limVals1 <- makeReps(att=limMappping1[1,3], i=as.integer(k), limRoot)
  }else{
    limVals1 <- readRDS(paste0(limRoot, '/Maps/', limMappping1[1,3], '/Chunks/AllCellVals_', k, '.rds'))
  }
  
  
  cellids <-  limVals1[,1]
  warr1 <- limVals1[,-1]
  print(str(warr1))
  
  if(limInfo$NumQuals[1] == 2)
  {
    print(paste0('Attribute = ', limMappping1[2,3]))
    if(limMappping1[2,3] %in% singleRastersDF$rnames){
      limVals2 <- makeReps(att=limMappping1[2,3], i=as.integer(k), limRoot)
    }else{
      limVals2 <- readRDS(paste0(limRoot, '/Maps/', limMappping1[2,3], '/Chunks/AllCellVals_', k, '.rds'))
    }

    warr2 <- limVals2[,-1]

  }
  
  
  
  if(limInfo$NumQuals[1] == 3)
  {
    print(paste0('Attribute = ', limMappping1[2,3]))
    f2 <- paste0(limRoot, '/Maps/', limMappping1[2,3], '/Chunks/AllCellVals_', k, '.rds')
    if(file.exists(f2)){
      attVals2 <- readRDS(paste0(limRoot, '/Maps/', limMappping1[2,3], '/Chunks/AllCellVals_', k, '.rds'))
    }else{
      attVals2 <- fst::read_fst(paste0(limRoot, '/Attributes/', limMappping1[2,3], '/Chunks/AllCellVals_', k, '.fst'))
    }
    
    print(paste0('Attribute = ', limMappping1[3,3]))
    f3 <- paste0(limRoot, '/Maps/', limMappping1[3,3], '/Chunks/AllCellVals_', k, '.rds')
    if(file.exists(f3)){
      attVals3 <- readRDS(paste0(limRoot, '/Maps/', limMappping1[3,3], '/Chunks/AllCellVals_', k, '.rds'))
    }else{
      attVals3 <- fst::read_fst(paste0(limRoot, '/Maps/', limMappping1[3,3], '/Chunks/AllCellVals_', k, '.fst'))
    }
    
    # attVals2 <- readRDS(paste0(limRoot, '/Attributes/', limMappping1[2,2],  '/Chunks/AllCellVals_', k, '.rds'))
     warr2 <- attVals2[,-1]
    # 
    # attVals3<- readRDS(paste0(limRoot, '/Attributes/', limMappping1[3,2],  '/Chunks/AllCellVals_', k, '.rds'))
     warr3 <- attVals3[,-1]
  }
  
  print( paste0('Loading took - '))
  toc()
  
  
  print(paste0('Number of land uses = ', length(uses)))
  
  for(j in 1:length(uses)){
    
    print(paste0('Doing calcs for landuse ', uses[j]))
    print(paste0('Number ', j, ' of ', length(uses)))
    tic()
    
    # fname <- paste0(outDir, '/subclasses_', k, '.rds')
    landuse <- uses[j]
    outDir <- paste0(outRoot, '/Subclasses/',  lim, '/', uses[j],  '/Chunks')
    
    vname <- paste0(outDir, '/subclass_Suit_', k, '.fst')
    if(!file.exists(vname)){
    
    
    if(!dir.exists(outDir)){dir.create(outDir,recursive = T)}
    #outfile <- paste0(outDir, '/subclasses_', uses[j], '.rds')
    
     outMatrix = matrix( -1, nrow=nrow(limVals1), ncol(limVals1)-1)

    
    alllims <- data.frame(limInfo$LIM_VALUE,limInfo$LIM_Name, limInfo[,5:14], limInfo[landuse])
    colnames(alllims)[13] <- landuse
    validInds <- which(!is.na(alllims[landuse]))
    lims <- alllims[validInds,] 
    
    
    if(nrow(lims) > 0){
      
      for (i in 1:nrow(lims))
      {
        print(paste0('Lim ', i))
        
        if( lims$limInfo.LIM_Name[1] == "Climate-annual_rainfall"){
          inds <- which(warr1 > lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Climate-heat_dry_season_and_perennial"){
          
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 < lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Climate-heat_wet_season_and_perennial"){
          
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 < lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Climate-frost"){
          
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 < lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Climate-temp_variation"){
          
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 < lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Flooding"){
          stop("Not implemented yet!")
        }
        else if( lims$limInfo.LIM_Name[1] == "Erosion"){
          
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i] & warr2 >= lims$QualVal2_min[i] & warr2 <= lims$QualVal2_max[i])
          outMatrix[inds] <- as.integer(lims[landuse][i,])
          
        }
        else if( lims$limInfo.LIM_Name[1] == "Wetness"){
          
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i] & warr2 >= lims$QualVal2_min[i] & warr2 <= lims$QualVal2_max[i])
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Moisture_availability_1.5"){
          
          inds <- which(warr1 > lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Moisture_availability_1.0"){
          
          inds <- which(warr1 > lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
          
        }
        else if( lims$limInfo.LIM_Name[1] == "Moisture_availability_0.6"){
          inds <- which(warr1 > lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Nutrient_balance"){
          
          inds <- which(warr1 > lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Soil_depth"){
          # debugging cahnged > to >=
          inds <- which(warr1 > lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Rockiness"){
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Microrelief"){
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Irrigation_efficiency-low_rate_methods"){
          
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Irrigation_efficiency-high_rate_methods"){
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Salinity_surface"){
          
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Acid_Sulfate_Soil_Potential"){
          
          #stop("Not implemented yet!") ###############################################################
          
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i]) 
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Moisture_availability_RAINFED_CROPPING_100"){
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 < lims$QualVal1_max[i] & warr2 >= lims$QualVal2_min[i] & warr2 < lims$QualVal2_max[i])
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Moisture_availability_RAINFED_CROPPING_150"){
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 < lims$QualVal1_max[i] & warr2 >= lims$QualVal2_min[i] & warr2 < lims$QualVal2_max[i])
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Moisture_availability_RAINFED_CROPPING_60"){
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 < lims$QualVal1_max[i] & warr2 >= lims$QualVal2_min[i] & warr2 < lims$QualVal2_max[i])
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        
        else if( lims$limInfo.LIM_Name[1] == "Surface_Condition"){
          
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i] & warr2 >= lims$QualVal2_min[i] & warr2 <= lims$QualVal2_max[i] & warr3 >= lims$QualVal3_min[i] & warr3 <= lims$QualVal3_max[i])
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        else if( lims$limInfo.LIM_Name[1] == "Surface_Texture"){
          
           print(lims[1,1:12])
          if(i==5){ # special case for this particular rule - Pa5
            
            fname4 <- paste0(limRoot, '/Maps/', limMappping1[4,3], '/Chunks/AllCellVals_', k, '.rds')
            limVals4 <- readRDS(fname4)
            warr4 <- limVals4[,-1]
            
            fname5 <- paste0(limRoot, '/Maps/', limMappping1[5,3], '/Chunks/AllCellVals_', k, '.rds')
            limVals5 <- readRDS(fname5)
            warr5 <- limVals5[,-1]
            
            inds <- which(warr4 >= lims$QualVal1_min[i] & warr4 <= lims$QualVal1_max[i] & exp(warr5) >= lims$QualVal2_min[i] & exp(warr5) <= lims$QualVal2_max[i])
            outMatrix[inds] <- as.integer(lims[landuse][i,])
            
            
          }else{
            inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i] & warr2 >= lims$QualVal2_min[i] & warr2 <= lims$QualVal3_max[i] & warr3 >= lims$QualVal3_min[i] & warr3 <= lims$QualVal3_max[i])
            outMatrix[inds] <- as.integer(lims[landuse][i,])
          }
        }
        else if( lims$limInfo.LIM_Name[1] == "Surface_Infiltration"){
          
          inds <- which(warr1 >= lims$QualVal1_min[i] & warr1 <= lims$QualVal1_max[i] & warr2 >= lims$QualVal2_min[i] & warr2 <= lims$QualVal2_max[i] )
          outMatrix[inds] <- as.integer(lims[landuse][i,])
        }
        
        else{
          
          stop("No limitation processing implemented yet for this limitation!")
        }
      }
      
      
    }
    print( paste0('Calcs took - '))
    toc()
    #print('Saving outputs')
    #tic()
    fOut <- cbind(as.integer(cellids), outMatrix)
    #threads_fst(nr_of_threads = NULL)
    #fst::write_fst(as.data.frame(fOut), paste0(outDir, '/subclasses_', lim, '_', k, '.fst'), compress = 90)
    #print( paste0('Saving took - '))
    #toc()
    #df2 <- data.frame(sapply(fOut, function(x) as.integer(x)))
    doSubClassSummary(rawSubs=fOut, outDir, lim, landuse, k)
    
    print('Done')
      
    }else{
      print(paste0("File exists - ", vname))
    }
  }
  
}



doSubClassSummary <- function(rawSubs, outRoot, lim, use, k){
  
  
  print(outRoot)
  print(paste0("Starting subclass summary for ", use, '....'))
  tictoc::tic()
    #inDir <- paste0(outRoot, '/Subclasses/', lim, '/', use,  '/Chunks')
    vname <- paste0(outRoot, '/subclass_Suit_', k, '.fst')
    
    #infile <- paste0(inDir,  '/subclasses_', k, '.rds')
    #print(infile) 
    #fOut <- readRDS(infile)
    fOut <- rawSubs
    
    #### do the subclass suitability summary
    inds <- fOut[,1]
    sumData <- apply(fOut[,-1], 1, getCategoricalPredictionSummary)
    b <- do.call(rbind, sumData)
    sumDataDF2 <- data.frame(inds, modalVal = unlist(b[,1]), ci = unlist(b[,2]))
    
    print(vname)
    fst::write_fst(sumDataDF2, paste0( vname), compress = 90)
    
    print(paste0("Finished subclass summary for ", use, '....'))
    print("Time taken = ")
    tictoc::toc()
  
}



makeBlockFile <- function(outPath, att, templateR, covDir, rootDir, numLayers){
  
  outRasterDir <- paste0(outPath , 'Attributes/', att)
  chunkRasterDir <- paste0(outRasterDir, '/Chunks')
  if(!dir.exists(chunkRasterDir)){dir.create(chunkRasterDir, recursive = T)}
  bsFile <- (paste0(chunkRasterDir, '/blocks.dat'))
  if(file.exists(bsFile)){
    unlink(bsFile)
  }
  
  bs <- blockSize(templateR, n=numLayers)
  saveRDS(bs, bsFile)
  return(bsFile)
}



calculateOverallSuitabilitesModal <- function (limRoot, landuse, currentLims, outDir, k){

  vname <- paste0(outDir, '/suit_', k, '.fst')
  if(!file.exists(vname)){

      for(i in 1:length(currentLims)){
    
        rdsPath <- paste0(limRoot, '/Subclasses/', currentLims[i], '/', landuse, '/Chunks/subclass_Suit_', k, '.fst')
        
        print(rdsPath)
        subs <- read_fst(rdsPath)
        
        if(i==1){
          suitdf <- subs[,1]
          cisdf <- subs[,1]
          }

        suitdf<- cbind(suitdf, subs$modalVal)
        cisdf<- cbind(cisdf, subs$ci)
      }
      
      colnames(suitdf) <- c('cid',  as.character(currentLims))
      colnames(cisdf) <- c('cid',  as.character(currentLims))
      
      A <- suitdf[,-1]
      B <- cisdf[,-1]
      dfIn <- as.data.frame(cbind(A,B)) ### this is a hack to send two dfs to the apply function - seems to work ok though even if I do say so myself
      
      suitvals <- apply(dfIn,1,getMaxLimStats, colsr=ncol(A))
      outVals <- do.call(rbind, suitvals)
      outdf <- data.frame( cids=suitdf[,1], modalVal = unlist(outVals[,1]), ci = unlist(outVals[,2]), cnt = unlist(outVals[,3]))
      
      #vname <- paste0(outDir, '/suit_', k, '.fst')
      print(paste0('vname = ' , vname))
      write_fst(outdf, vname)
      
      
  }else{
    paste0("File exists - ", vname)
  }
      

}


getMaxLimStats <- function(allV, colsr){
  
  vv <- allV[1:colsr]
  ve <- allV[(colsr+1):(colsr*2)]
  
  mval <- max(vv)
  limInds <- which(vv==mval)
  cis <- ve[limInds]
  maxCI <- max(cis)
  maxLimCnt <- length(cis)
  
  v = list(suit=mval, confInd=maxCI, maxLimCnt=maxLimCnt)
}


makeSuitMap <- function(){  ##### to be deleted
  
  covDir <- '/datasets/work/lw_rowra/work/3_Land_suitability/0_Working/Uta/Roper'
  outDir <- '/datasets/work/af-digiscapesm/work/Ross/Roper'
  
  templateR <- raster(paste0('/datasets/work/af-digiscapesm/work/Ross/Roper/Boundaries/RoperMask.tif'))
  outRasterDir <- paste0(outDir , '/Suitability_V2/Maps') 
  chunkRasterDir <- paste0(outDir , '/Suitability_V2/Chunks')

    generateSuitRaster(dataDir = outDir, templateR, use, bs)
    
    
    print('Masking model raster.....')
    rasterDir <- paste0(outDir, '/Attributes/',  att,  '/Chunks')
    outRaster <- paste0(outDir, '/Attributes/',  att, '/',  att, '.tif')
    inr1 <- raster(outRaster) 
    r1 <- mask(inr1, mr)
    
    print('Masking uncertainty raster.....')
    outRaster2 <- paste0(outDir, '/Attributes/',  att, '/',  att, '_Uncert.tif')
    inr2 <- raster(outRaster2) 
    r2 <- mask(inr1, mr)
    
    print('Writing masked rasters.....')
    writeRaster(r1, filename = outRaster, overwrite=T)
    writeRaster(r2, filename = outRaster2, overwrite=T)
    
    print(paste0('Fishished mapping ', att))
    
  
}




generateSuitRaster <- function(dataDir = NULL, templateR = NULL, use = NULL){
  
  suffix <-'_Uncert'
  #outDir <- paste0(rootDir, '/Outputs/Suitability/', use, '/Chunks')
  
  rasterDir <- paste0(dataDir, '/Suitability_V2/Chunks/',  use  )
  outRaster <- paste0(dataDir, '/Suitability_V2/Maps/',  use, '.tif')
  
  chks <- getChunkInfo(20, nrow(templateR))
  
 
  print(paste0("creating - ", outRaster))
  
  outRasterCI <- paste0(dataDir, '/Suitability_V2/Maps/', use, suffix, '.tif')
  
  if(!file.exists(outRasterCI)){
  
  predR<-raster(templateR)
  crs(predR) <- CRS("+proj=longlat +datum=WGS84")
  predR<-writeStart(predR,filename=outRaster,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
  predCI<-raster(templateR)
  crs(predCI) <- CRS("+proj=longlat +datum=WGS84")
  predCI<-writeStart(predCI,filename=outRasterCI,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
  
  print(paste0("Creating Raster ", outRaster) ) 
  
  pb <- txtProgressBar(min=1, max=chks$chunks, style=3)
  
  for (i in 1:chks$chunks)
  {
    setTxtProgressBar(pb, i)
    bname = paste0(rasterDir, '/suit_', i, '.fst')
    
    if(file.exists(bname)){
    # print(bname)
    
        csize<-chks$nlines[i] * ncol(templateR)
        b <- rep(NAvalue(templateR), csize )
        c <- rep(NAvalue(templateR), csize )
        
        blockVals <- read_fst(bname)
        #cat(paste(i, " ", sep=''))
        b[blockVals$cids] <- blockVals$modalVal
        c[blockVals$cids] <- blockVals$ci
        predR <- writeValues(predR, b, chks$starts[i])
        predCI <- writeValues(predCI, c, chks$starts[i])
    }
    
  }
  
  close(pb)
  
  print('Writing rasters to disk.....')
  predR<-writeStop(predR)
  predCI<-writeStop(predCI)
  #plot(predR, main=paste0(att ))
  #plot(predCI, main=paste0(att ))
  
  
  print("Done generating raw suitability maps")
  print("")
  print("Applying masks")
  
  #mr <- raster(paste0(rootDir, '/Boundaries/bdyBoxWithNaRiver.tif'))
  mr <- templateR
  print('Masking Suitability raster.....')
  inr1 <- raster(outRaster) 
  r1 <- mask(inr1, mr, filename=paste0(dataDir, '/Suitability_V2/Maps/',  use, '_m.tif'))
  
  print('Masking uncertainty raster.....')
  inr2 <- raster(outRasterCI) 
  r2 <- mask(inr2, mr,filename=paste0(dataDir, '/Suitability_V2/Maps/', use, suffix, '_m.tif'))
  
 
  print("Finished applying masks")
  
  }else{
    print(paste0('File already exists - ', outRasterCI))
  }
}


generateSubclassRaster <- function(rootDir = NULL, templateR = NULL, lim = NULL, use = NULL, bs ){
  
  suffix <-'_Uncert'

  
  rasterDir <- paste0(rootDir, '/Outputs/Subclasses/', lim, '/',  use,  '/Chunks')
  outRaster <- paste0(rootDir, '/Outputs/SubclassMaps/',  use, '/',  lim, '.tif')
  
  if(!dir.exists(dirname(outRaster))){dir.create(dirname(outRaster), recursive = T)}
  print(paste0("creating - ", outRaster))
  
  outRasterCI <- paste0(rootDir, '/Outputs/SubclassMaps/',  use, '/',  lim, suffix, '.tif')
  
  predR<-raster(templateR)
  crs(predR) <- CRS("+proj=longlat +datum=WGS84")
  predR<-writeStart(predR,filename=outRaster,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
  predCI<-raster(templateR)
  crs(predCI) <- CRS("+proj=longlat +datum=WGS84")
  predCI<-writeStart(predCI,filename=outRasterCI,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
  
  print(paste0("Creating Raster ", outRaster) ) 
  
  pb <- txtProgressBar(min=1, max=length(bs$row), style=3)
  
  for (i in 1:bs$n)
  {
    setTxtProgressBar(pb, i)
    bname = paste0(rasterDir, '/subclass_Suit_', i, '.fst')
    
    if(file.exists(bname)){
      # print(bname)
      
      csize<-bs$nrows[i] * ncol(templateR)
      b <- rep(NAvalue(templateR), csize )
      c <- rep(NAvalue(templateR), csize )
      
      blockVals <- read_fst(bname)
      #cat(paste(i, " ", sep=''))
      b[blockVals$cids] <- blockVals$modalVal
      c[blockVals$cids] <- blockVals$ci
      predR <- writeValues(predR, b, bs$row[i])
      predCI <- writeValues(predCI, c, bs$row[i])
    }
    
  }
  
  close(pb)
  
  print('Writing rasters to disk.....')
  predR<-writeStop(predR)
  predCI<-writeStop(predCI)
  #plot(predR, main=paste0(att ))
  #plot(predCI, main=paste0(att ))
  
  
  print("Done")
}


generateSubclassRasterPara <- function(rootDir = NULL, templateR = NULL, lims = NULL, use = NULL, bs ){
  
  suffix <-'_Uncert'
  
  lim <- lims[m]
  
  rasterDir <- paste0(rootDir, '/Outputs/Subclasses/', lim, '/',  use,  '/Chunks')
  outRaster <- paste0(rootDir, '/Outputs/SubclassMaps/',  use, '/',  lim, '.tif')
  
  if(!dir.exists(dirname(outRaster))){dir.create(dirname(outRaster), recursive = T)}
  print(paste0("creating - ", outRaster))
  
  outRasterCI <- paste0(rootDir, '/Outputs/SubclassMaps/',  use, '/',  lim, suffix, '.tif')
  
  predR<-raster(templateR)
  crs(predR) <- CRS("+proj=longlat +datum=WGS84")
  predR<-writeStart(predR,filename=outRaster,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
  predCI<-raster(templateR)
  crs(predCI) <- CRS("+proj=longlat +datum=WGS84")
  predCI<-writeStart(predCI,filename=outRasterCI,overwrite=TRUE,NAflag=-9999,datatype="FLT4S")
  
  print(paste0("Creating Raster ", outRaster) ) 

  
  for (i in 1:bs$n)
  {

    bname = paste0(rasterDir, '/subclass_Suit_', i, '.fst')
    
    if(file.exists(bname)){
    
      csize<-bs$nrows[i] * ncol(templateR)
      b <- rep(NAvalue(templateR), csize )
      c <- rep(NAvalue(templateR), csize )
      
      blockVals <- read_fst(bname)
      b[blockVals$inds] <- blockVals$modalVal
      c[blockVals$inds] <- blockVals$ci
      predR <- writeValues(predR, b, bs$row[i])
      predCI <- writeValues(predCI, c, bs$row[i])
    }
    
  }
  
  close(pb)
  
  print('Writing rasters to disk.....')
  predR<-writeStop(predR)
  predCI<-writeStop(predCI)
  
  
  print("Done")
  
  return(outRaster)
}



checkSubclassFiles <- function(subClassDir, limitations, reqCnt){
  
  for (i in 1:length(limitations)) {
    
  }
  
}
