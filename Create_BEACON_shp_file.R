## load packages
library(sp)
library(maptools)
library(foreign)
library(rgdal)
library(plotKML)
library(rgeos)
library(brew)
library(RColorBrewer)

## my-functions
source("BEACON_functions.R")

## parameters
tmp.BEACON.dir <- "//deqhq1/tmdl/TMDL_WR/MidCoast/GIS/BacteriaTMDL/Beaches/EPA_BEACON"
tmp.BEACON.shp.dir <- paste0(tmp.BEACON.dir,"/rad_beach_20140804_shp/rad_beach_20140804")
tmp.BEACON.mc.beach.shp <- "BEACON_beaches_mc"
tmp.BEACON.beach.ext.shp <- "rad_beach_l"
tmp.BEACON.dbf <-"beach_attributes.dbf"
tmp.stn.shp <- "beach_stn_location"

##
## get the sampling location data
tmp.sp.stn <- readShapePoints(fn=paste0("./",tmp.stn.shp), proj4string = CRS("+proj=longlat +datum=NAD83"))

## 
## projections for beaches
tmp.BEACON.proj <- CRS("+init=epsg:4269")
## linear extents of all beaches in BEACON data set
tmp.sp.BEACON.all <- readShapeLines(fn=paste0(tmp.BEACON.shp.dir,"/",tmp.BEACON.beach.ext.shp), proj4string = tmp.BEACON.proj)
## get BEACONS dbf file for beach attributes
tmp.BEACON.attr.all <- read.dbf(file=paste0(tmp.BEACON.shp.dir,"/",tmp.BEACON.dbf))
## add the attribute data to spatial data frame
tmp.sp.BEACON.all.attr <- merge(tmp.sp.BEACON.all,tmp.BEACON.attr.all, by.x="SRC_FEATID",by.y="BEACH_ID")
##
## get the BEACON beach extents that are in the in Mid-Coast
tmp.sp.BEACON.attr.mc  <- subsetBasedOnExtent.kmb.sp(tmp.sp.stn,tmp.sp.BEACON.all.attr)
##
## write shpae file for BEACON beaches based on sample locations for the Mid-Coast
writeSpatialShape(tmp.sp.BEACON.attr.mc, paste0("./",tmp.BEACON.mc.beach.shp))

## done