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

## files
tmp.stn.csv <- "stn_loc.csv"
tmp.BEACON.beach.ext.shp <- "rad_beach_l"
tmp.BEACON.dbf <-"beach_attributes.dbf"

##
## get the sampling location data
tmp.stn.locs <- read.csv(paste0(tmp.stn.dir,"/",tmp.stn.csv))
## create and save stations shapefile
tmp.sp.stn <- SpatialPointsDataFrame(coords=tmp.stn.locs[,c("lon","lat")], data=tmp.stn.locs[,c("site","data_sourc")], proj4string = CRS("+proj=longlat +datum=NAD83"))
writeOGR(tmp.sp.stn, dsn=".", layer= "beach_stn_location", driver = "ESRI Shapefile")
## clean up
rm(tmp.stn.locs)

##
## get USEPA BEACON spatial data
tmp.BEACON.shp.dir <- dwnldUnzpBEACON.kmb.sp(tmp.BEACON.dir)
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

##
## get the BEACON beach extents that are in the in Mid-Coast
tmp.sp.BEACON.OR.mc  <- subsetBasedOnExtent.kmb.sp(tmp.sp.stn,tmp.sp.BEACON.OR)
## clean up
rm(tmp.sp.BEACON.OR)


##
## get distances of sampling locations to the beach lines
tmp.df.dist.stn.beach.line <- lineardistance.kmb.sp(tmp.sp.stn,tmp.sp.BEACON.OR.mc)
##
## snap sample location to beach extents
tmp.sp.stn.snap <- snapPointsToLine.sp.kmb(tmp.sp.stn,tmp.sp.BEACON.OR.mc,maxDist=NA)


##
## tranform to CRS used by Google Earth ("+init=epsg:4326")
tmp.sp.stn.GE <- spTrans



kml(tmp.sp.stn.KML,labels=site, size=10, scale=1)
plotKML(tmp.sp.stn.KML,file="tmp.sp.stn.KML.kml")
plotKML(tmp.sp.stn.KML)


plotKML(tmp.sp.beach.ext.or.mc.KML)
plotKML(tmp.sp.stn.on.ext.KML)
## plot in google earth
plotKML(tmp.sp.stn.on.beach.ext.KML)
plotKML(tmp.sp.beach.ext.or.KML)
