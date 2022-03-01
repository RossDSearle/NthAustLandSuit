library(raster)
library(stringr)

rasterOptions(progress = 'text')

rootDir <- '/datasets/work/lw-soildatarepo/work/http/Products/NthAust'


region <- 'Roper'


####   Suitabilities


### Uncertainty rasters

fls <- list.files(paste0(rootDir, '/', region, '/Suitability_COGs_Raw'), pattern='.tif$', full.names = T, recursive=F)

outDir <- paste0(rootDir, '/', region, '/Suitability')
if(!dir.exists(outDir)){dir.create(outDir)}

uncertFiles <- fls[grepl('Uncert', fls)]

for (i in 1:length(uncertFiles)) {
  print(i)
  f <- basename(uncertFiles[i])
  outname <- paste0(outDir, '/',  str_remove(f, '_m_COG' ))
  file.copy(uncertFiles[i], outname)
}



####  Suitability rasters

suitFiles <- fls[!grepl('Uncert', fls)]

for (i in 1:length(suitFiles)) {
  print(i)
  f <- basename(suitFiles[i])
  outname <- paste0(outDir, '/',  str_remove(f, '_m_COG' ))

  r <- raster(suitFiles[i])
  #rat <- ratify(r)
  #rat <- unique(r)
  #cats <- levels(rr)
  ratDF <- data.frame(ID=c(1,2,3,4,5))
  ratName <- str_replace(outname, '.tif', '.csv')
  write.csv(cats, ratName, row.names = F)
  raster::writeRaster(r, outname, datatype='INT1U', overwrite=T)
}



####  Attribute rasters

attfls <- list.files(paste0(rootDir, '/', region, '/Attributes_COGs_Raw'), pattern='.tif$', full.names = T, recursive=F)

outDir <- paste0(rootDir, '/', region, '/Attributes')
if(!dir.exists(outDir)){dir.create(outDir)}

for (i in 1:length(attfls)) {
  print(i)
  f <- basename(attfls[i])
  outname <- paste0(outDir, '/',  str_remove(f, '_COG' ))
  r <- raster(attfls[i])
  nums <- unique(r[1:100000])

  if(length(nums) > 255){
    raster::writeRaster(r, outname, overwrite=T)
  }else{

    rat <- ratify(r)
    cats <- level(rat)
    ratName <- str_replace(outname, '.tif', '.csv')
    write.csv(cats, ratName, row.names = F)
    raster::writeRaster(r, outname, datatype='INT1U', overwrite=T)
  }
}








outpath <- '/datasets/work/lw-soildatarepo/work/http/Products/NthAust/test.tif'

ri <- raster(inpath)
unique(ri)

cmd <- paste0('gdal_translate ', inpath, ' ', outpath, ' -co COMPRESS=NONE -ot Float32 -r nearest -q -outsize 7% 7% -projwin 131.994 -12.8937 135.7076 -16.7781')
op <- system(cmd, intern = T)
r <- raster(outpath)

unique(r)
plot(r)


ratR <- ratify(ri)
dataType(ratR)
raster::writeRaster(ri, '/datasets/work/lw-soildatarepo/work/http/Products/NthAust/Roper/Attributes_COGs/rat_COGs.tif', datatype='INT1U')
rr <- raster('/datasets/work/lw-soildatarepo/work/http/Products/NthAust/Roper/Attributes_COGs/rat.tif')
rr
dataType(rr)
