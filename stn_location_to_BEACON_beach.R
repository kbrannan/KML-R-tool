## load packages
library(sp)
library(maptools)
library(foreign)
library(rgdal)
library(plotKML)
library(rgeos)

## folders
tmp.stn.dir <- "//deqhq1/tmdl/TMDL_WR/MidCoast/GIS/BacteriaTMDL/Beaches/Layers"
tmp.BEACON.dir <- "//deqhq1/tmdl/TMDL_WR/MidCoast/GIS/BacteriaTMDL/Beaches/EPA_BEACON"

## shapefiles and dbf files
tmp.stn.shp <- "stations_in_group_areas"
tmp.BEACON.beach.ext.shp <- "rad_beach_l"
tmp.BEACON.dbf <-"beach_attributes.dbf"



##
## get the sampling location data
## Get stations shapefile
tmp.sp.stn <- readShapePoints(paste0(tmp.stn.dir,"/",tmp.stn.shp), proj4string = CRS("+proj=longlat +datum=NAD83"), verbose = FALSE,repair=FALSE)
## doa a little cleaning and and reformating
tmp.sp.stn@data <- data.frame(site=as.character(tmp.sp.stn@data[,grep("site",names(tmp.sp.stn@data))]), data_sourc=tmp.sp.stn@data[,grep("data_sourc",names(tmp.sp.stn@data))], lat=tmp.sp.stn@data[,grep("lat",names(tmp.sp.stn@data))],lon=tmp.sp.stn@data[,grep("lon",names(tmp.sp.stn@data))])


## get USEPA BEACON spatial data
tmp.BEACON.shp.dir <- dwnldUnzpBEACONsp(tmp.BEACON.dir)
## linear extents of all beaches in BEACON data set
tmp.sp.BEACON.all <- readShapeLines(fn=paste0(tmp.BEACON.shp.dir,"/",tmp.BEACON.beach.ext.shp), proj4string = CRS("+init=epsg:4269"))
## get beaches identified as being in Oregon
tmp.sp.BEACON.OR <- tmp.sp.BEACON.all[grep("^OR",tmp.sp.BEACON.all@data$SRC_FEATID),]
## clean-up
rm(tmp.sp.BEACON.all)


## get BEACONS dbf file for beach attributes
tmp.BEACON.attr.all <- read.dbf(file=paste0(tmp.BEACON.shp.dir,"/",tmp.BEACON.dbf))
## get beaches in OR
tmp.BEACON.attr.or <- tmp.BEACON.attr.all[tmp.BEACON.attr.all$BEACH_STAT == "OR",]
## clean up
rm(tmp.BEACON.attr.all)

## google CRS is "+init=epsg:4326"
tmp.sp.stn.on.beach.ext.KML <- spTransform(
  snapPointsToLines(points=tmp.sp.stn.ft,lines=tmp.sp.beach.ext.or.ft,maxDist=300),
  CRS("+init=epsg:4326"))
tmp.sp.beach.ext.or.KML <- spTransform(tmp.sp.beach.ext.or,CRS("+init=epsg:4326"))

##
## get the BEACON beach extents that are in the in Mid-Coast
## out the stations and the Oregon Beacon beach extentsin the same CRS
tmp.sp.beach.ext.or.KML <- spTransform(tmp.sp.beach.ext.or,CRS("+init=epsg:4326"))
tmp.sp.stn.KML <- spTransform(tmp.sp.stn,CRS("+init=epsg:4326"))
## create a polygon from the bounding box of the stations layer to use for clipping
clip.bbox <- cbind(floor(bbox(tmp.sp.stn.KML)[,1]),ceiling(bbox(tmp.sp.stn.KML)[,2]))
clip.rec <- rbind(c(clip.bbox[1,1],clip.bbox[2,2]),c(clip.bbox[1,2],clip.bbox[2,2]),c(clip.bbox[1,2],clip.bbox[2,1]),c(clip.bbox[1,1],clip.bbox[2,1]),c(clip.bbox[1,1],clip.bbox[2,2]))
clip.sp.rec <- SpatialPolygons(list(Polygons(list(Polygon(clip.rec)),1)))
proj4string(clip.sp.rec) <- CRS("+init=epsg:4326")
## select the beach extents that are in the stations layer bounding block
## this is "clipping" in the geometric set sense
tmp.vec.in <- as.vector(gIntersects(clip.sp.rec,tmp.sp.beach.ext.or.KML,byid=TRUE))
## beach extents in Mid-Coast by selecting rows of Oregeon beach extents
tmp.sp.beach.ext.or.mc.KML <- tmp.sp.beach.ext.or.KML[tmp.vec.in == TRUE,]
##
## snap sample location to beach extents
## transform to Oregon Lambert projection
tmp.sp.beach.ext.or.mc.ft <- spTransform(tmp.sp.beach.ext.or.mc.KML,CRS("+init=epsg:2992"))
tmp.sp.stn.ft <- spTransform(tmp.sp.stn,CRS("+init=epsg:2992"))
## check distances of stations to beach extent lines
shortest.dists <- numeric(nrow(tmp.sp.stn.ft))
for (i in seq_len(nrow(tmp.sp.stn.ft))) {
  shortest.dists[i] <- gDistance(tmp.sp.stn.ft[i,], tmp.sp.beach.ext.or.mc.ft)
}
df.shortest.dists <- data.frame(site=tmp.sp.stn.ft$site,dist_ft=round(shortest.dists,0))
summary(df.shortest.dists)

# there are 18 sites that are >= 100 ft from a beach extent
length(df.shortest.dists$dist_ft[df.shortest.dists$dist_ft > 1000])
df.shortest.dists[df.shortest.dists$dist_ft > 1000,]



tmp.sp.stn.on.ext.ft <- snapPointsToLines(points=tmp.sp.stn.ft,lines=tmp.sp.beach.ext.or.mc.ft,maxDist=5280)
tmp.sp.stn.on.ext.KML <- spTransform(tmp.sp.stn.on.ext.ft,CRS("+init=epsg:4326"))


kml(tmp.sp.stn.KML,labels=site, size=10, scale=1)
plotKML(tmp.sp.stn.KML,file="tmp.sp.stn.KML.kml")
plotKML(tmp.sp.stn.KML)


plotKML(tmp.sp.beach.ext.or.mc.KML)
plotKML(tmp.sp.stn.on.ext.KML)
## plot in google earth
plotKML(tmp.sp.stn.on.beach.ext.KML)
plotKML(tmp.sp.beach.ext.or.KML)
