### Hopefully all the relevant parameters for the App are stored in here


defaultRegion = 'Victoria'
defaultProductType = 'Attributes'
defaultProjection = '+init=epsg:4326'


maxcells=200000 # Max number of cells in the raster generated for the Leaflet map
trans=1 # Default map transperency

SHOW_WELCOME_MESSAGE=F

prodDirs <- c('1', '2', '3')
prodNames <- c('Suitability', 'Attributes', 'Covariates' )
AvailableMapProducts <- data.frame(prodDirs, prodNames)

dataRoot <- '/datasets/work/lw-soildatarepo/work/http/Products/NthAust'
suitFrameworkPath <- paste0(dataRoot, '/All/SuitabilityFramework')

regions <- c('Roper', 'Victoria', 'Southern_Gulf')
minX <- c(131.994, 128.999, 136.382)
minY <- c(-16.778, -18.400, -21.492)
maxX <- c(135.707, 132.549, 140.951)
maxY <- c(-12.893, -14.690, -16.174)

regionsDF <- data.frame(region = regions, minx = minX, miny=minY, maxx=maxX, maxy=maxY, stringsAsFactors = F)

drillChoices <- c('Current Layer', 'Everything', 'All Suitabilites', 'All Attributes', 'Current Suitability and All Attributes')
ALWAYS_DRILL=T
