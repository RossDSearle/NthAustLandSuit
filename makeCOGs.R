library(stringr)

rootDir <- '/datasets/work/lw-soildatarepo/work/Ross/Roper/Maps'


fls <- list.files(paste0(rootDir, '/Suitability'), pattern = '_m.tif$', full.names = T, recursive = F)

for (i in 1:length(fls)) {
  #for (i in 1:10) {
  cat(paste0(i, ' '))
  oname <- paste0(rootDir, '/Suitability_COGs/', str_remove(basename(fls[i]), '.tif'), '_COG.tif')
  cmd <- paste0('gdal_translate ', fls[i], ' ', oname,' -co TILED=YES -co COPY_SRC_OVERVIEWS=YES -co COMPRESS=LZW' )
  system(cmd)
}



rootDir <- '/datasets/work/lw-soildatarepo/work/Ross/Roper/Maps'


fls <- list.files(paste0(rootDir, '/Attributes'), pattern = '.tif$', full.names = T, recursive = F)

for (i in 1:length(fls)) {
  #for (i in 1:10) {
  cat(paste0(i, ' '))
  oname <- paste0(rootDir, '/Attributes_COGs/', str_remove(basename(fls[i]), '.tif'), '_COG.tif')
  cmd <- paste0('gdal_translate ', fls[i], ' ', oname,' -co TILED=YES -co COPY_SRC_OVERVIEWS=YES -co COMPRESS=LZW' )
  system(cmd)
}