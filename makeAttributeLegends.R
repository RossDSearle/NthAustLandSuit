roperRoot <- '/datasets/work/lw-soildatarepo/work/Ross/Roper'
fls <- list.files(paste0(roperRoot, '/Maps/Attributes_COGs'), recursive = F, full.names = T)

scl <- 0.5
legHeight = 270
legWidth = 120

for (i in 1:length(fls)) {
    print(i)
    att <- str_remove(basename(fls[i]),'_COG.tif')
    png(paste0('/srv/shiny-server/Roper/www/Legends/', att, '_legend.png'), height=legHeight, width = legWidth)
    par(mar = c(0, 0, 0, 0))
   
    r <- raster(fls[i])
    plot(r, legend.only=T)
    dev.off()

}