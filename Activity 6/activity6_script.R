# install.packages(c("raster","sp","rgdal","rgeos","plyr"))

library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)

#read in shapefiles
#readOGR in rgdal does this
g1966 <- readOGR("Y:\\Data\\activities\\a06\\GNPglaciers\\GNPglaciers_1966.shp")
plot(g1966, col="black", axes=T)
str(g1966)
