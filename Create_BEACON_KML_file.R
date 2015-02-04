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
tmp.BEACON.mc.beach.shp <- "BEACON_beaches_mc"
tmp.BEACON.kml.template <- "beach_merge_multi_line_brew.kmlt"
tmp.KML.template.file.dir <- "./kml_templates"
tmp.BEACON.mc.beach.kml <- "BEACON_beaches_mc.kml"
tmp.KML.file.dir <- "./kml_files"

## 
## projections for beaches
tmp.BEACON.proj <- CRS("+init=epsg:4269")
## linear extents of all beaches in BEACON data set
tmp.sp.BEACON.mc <- readShapeLines(fn=paste0("./",tmp.BEACON.mc.beach.shp), proj4string = tmp.BEACON.proj)
##
## tranform to CRS used by Google Earth ("+init=epsg:4326")
tmp.sp.BEACON.mc.GE <- spTransform(tmp.sp.BEACON.mc,CRS("+init=epsg:4326"))
rm(tmp.sp.BEACON.mc)
##
## get beach names
tmp.names <- unique(as.character(tmp.sp.BEACON.mc.GE@data[,"BEACH_NAME"]))
tmp.names <- tmp.names[order(tmp.names)]

##
## create data frame for names and line coordinates
df.beach.lines <- data.frame(beach.name=tmp.names,coords="0,0,0", stringsAsFactors=FALSE)

##
## get coordinates of lines for each beach and write a string of 3-tuples for all the points of each line
for(ii in 1:length(tmp.names)) {
  num.rows <- grep(as.character(tmp.names[ii]),as.character(tmp.sp.BEACON.mc.GE@data$BEACH_NAME))
  df.beach.lines$coords[ii] <- multlinesCoordsString(tmp.sp.BEACON.mc.GE[num.rows,])
  rm(num.rows)  
}
##
## generate colors for kml elements lines
line.colors <- hex2kml(rainbow(length(tmp.names),alpha=0.45))

##
## write KML files for Google Earth
brew(file=paste0(tmp.KML.template.file.dir,"/",tmp.BEACON.kml.template),output=paste0("./",tmp.KML.file.dir,"/",tmp.BEACON.mc.beach.kml))

## done