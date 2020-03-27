# install.packages(c("raster","sp","rgdal","rgeos","plyr"))

library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)

#read in shapefiles
#readOGR in rgdal does this
g1966 <- readOGR("~/Documents/GitHub/GEOG331/Activity 6/a06/GNPglaciers/GNPglaciers_1966.shp")

g1998 <- readOGR("~/Documents/GitHub/GEOG331/Activity 6/a06/GNPglaciers/GNPglaciers_1998.shp")

g2005 <- readOGR("~/Documents/GitHub/GEOG331/Activity 6/a06/GNPglaciers/GNPglaciers_2005.shp")

g2015 <- readOGR("~/Documents/GitHub/GEOG331/Activity 6/a06/GNPglaciers/GNPglaciers_2015.shp")

# plot(g1966, col="black", axes=T)

str(g1966)

#data stores all accompanying info/measurements for each spatial object
head(g2015@data)

#polygons stores the coordinates for drawing the polygons
g2015@polygons[[1]]

g1966@proj4string

# allows you to map vector data and show different colors for a data value
spplot(g1966, "GLACNAME")

#check glacier names for 1966
g1966@data$GLACNAME

#check glacier names for 2015
g2015@data$GLACNAME

#fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))

#read in rgb imagery from landsat
redL <- raster("~/Documents/GitHub/GEOG331/Activity 6/a06/glacier_09_05_14/l08_red.tif")
greenL <- raster("~/Documents/GitHub/GEOG331/Activity 6/a06/glacier_09_05_14/l08_green.tif")
blueL <- raster("~/Documents/GitHub/GEOG331/Activity 6/a06/glacier_09_05_14/l08_blue.tif")

#check coordinate system
redL@crs

#make a brick that stacks all layers
rgbL <- brick(redL, greenL, blueL)

#plot with color
#show axes for reference
#add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)
#add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#Subset the plot below to zoom in on closer on a few glaciers
plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#set up years to read in
ndviYear <- seq(2003,2016)

#read all files into a list
NDVIraster <- list() 
for(i in 1:length(ndviYear)){
  NDVIraster[[i]] <- raster(paste0("~/Documents/GitHub/GEOG331/Activity 6/a06/NDVI/NDVI_",ndviYear[i],".tif"))
  
}

str(NDVIraster[[1]])

#get projection
NDVIraster[[1]]@crs

## Answer 3 ##

par(mai=c(1,1,1,1))
plot(g1966, col="black", axes=T)
plot(NDVIraster[[1]], add=T, border=NA)

dev.off()
par(mai=c(0.5,0.5,0.5,0.5))
par(mfrow= c(1,2))
plot(NDVIraster[[1]], axes=T)
plot(g1966, col="black", axes=T)

#reproject the glaciers
#use the NDVI projection
#spTransform(file to project, new coordinate system)
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)

## Answer 4 ##

par(mai=c(0.3,0.3,0.3,0.3))
plot(NDVIraster[[13]], axes=F)
#add polygons to plot
plot(g2015p, border="black", add=TRUE, axes=F)

#calculate area for all polygons
#add directly into data table for each shapefile
g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)

gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full")
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")

dev.off()
plot(c(1966,1998,2005,2015), 
     c(gAll$a1966m.sq[1],gAll$a1998m.sq[1], gAll$a2005m.sq[1],gAll$a2015m.sq[1]),
     type="b", 
     pch=19, col=rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim=c(0,2000000),
     ylab="Area of glacier (meters squared)",
     xlab="Year")

for(i in 2:39){
  points(c(1966,1998,2005,2015), 
         c(gAll$a1966m.sq[i],gAll$a1998m.sq[i], gAll$a2005m.sq[i],gAll$a2015m.sq[i]),
         type="b", 
         pch=19, col=rgb(0.5,0.5,0.5,0.5))
  
} 

## Answer 5 ##

for(i in 1:39){
  gAll$a2015per.change[i]<- (abs(area(g2015p)[i]-area(g1966p)[i])/area(g1966p)[i])*100
  g2015p@data$a2015per.change[i]<- (abs(area(g2015p)[i]-area(g1966p)[i])/area(g1966p)[i])*100
}

spplot(g2015p, "a2015per.change")