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

## folders
tmp.stn.dir <- "//deqhq1/tmdl/TMDL_WR/MidCoast/GIS/BacteriaTMDL/Beaches/Layers"
tmp.BEACON.dir <- "//deqhq1/tmdl/TMDL_WR/MidCoast/GIS/BacteriaTMDL/Beaches/EPA_BEACON"
tmp.BEACON.shp.dir <- paste0(tmp.BEACON.dir,"/rad_beach_20140804_shp/rad_beach_20140804")


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
## 
## projections for beaches
tmp.BEACON.proj <- CRS("+init=epsg:4269")
## linear extents of all beaches in BEACON data set
tmp.sp.BEACON.all <- readShapeLines(fn=paste0(tmp.BEACON.shp.dir,"/",tmp.BEACON.beach.ext.shp), proj4string = tmp.BEACON.proj)
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
## add the attribute data to spatial data frame
tmp.sp.BEACON.OR.mc.add.attr <- merge(tmp.sp.BEACON.OR.mc,tmp.BEACON.attr.or, by.x="SRC_FEATID",by.y="BEACH_ID")
## clean up
rm(tmp.sp.BEACON.OR.mc,tmp.BEACON.attr.or)
##
## get mid points of the beach lines to use for labels
tmp.sp.BEACON.OR.mc.add.attr.mdpnts <- line.mid.points.kmb.sp(tmp.sp.BEACON.OR.mc.add.attr)
##
## get distances of sampling locations to the beach lines
tmp.df.dist.stn.beach.line <- lineardistance.kmb.sp(tmp.sp.stn,tmp.sp.BEACON.OR.mc.add.attr)
##
## snap sample location to beach extents
tmp.sp.stn.snap <- snapPointsToLine.sp.kmb(tmp.sp.stn,tmp.sp.BEACON.OR.mc.add.attr,maxDist=NA)
##
##
## tranform to CRS used by Google Earth ("+init=epsg:4326")
tmp.sp.stn.GE <- spTransform(tmp.sp.stn,CRS("+init=epsg:4326"))
tmp.sp.stn.snap.GE <- spTransform(tmp.sp.stn.snap,CRS("+init=epsg:4326"))
tmp.sp.BEACON.OR.mc.add.attr.GE <- spTransform(tmp.sp.BEACON.OR.mc.add.attr,CRS("+init=epsg:4326"))
tmp.sp.BEACON.OR.mc.add.attr.mdpnts.GE <- spTransform(tmp.sp.BEACON.OR.mc.add.attr.mdpnts,CRS("+init=epsg:4326"))

##
##
## generate colors for kml elements
## lines
line.colors <- hex2kml(heat.colors((length(tmp.sp.BEACON.OR.mc.add.attr.GE@data[,1])),alpha=0.65))

##
## delete from here
## save for quick access during development
rdata <- "kml_data.RData"
if(length(ls(all = TRUE)) > 2) save(list = ls(all = TRUE),file=rdata)
if(length(ls(all = TRUE)) < 2) load(file=rdata)
## delete to here

##
## write KML files for Google Earth
brew(file="./kml_templates/stn_placemark_brew.kmlt",output="stn_placemark_brew.kml")
brew(file="./kml_templates/beach_line_brew.kmlt",output="beach_line_brew.kml")

tmp.sp.BEACON.OR.mc.add.attr.GE@data$BEACH_NAME <- sapply(as.character(tmp.sp.BEACON.OR.mc.add.attr.GE@data$BEACH_NAME),simpleCap,USE.NAMES=FALSE)

str(junk)
junk[1]


##
## view compare orginial and snap sample locations
plotKML(tmp.sp.stn.GE)
plotKML(tmp.sp.stn.snap.GE)
plotKML(tmp.sp.BEACON.OR.mc.GE)




##
## scratch space
tmp.sp.BEACON.OR.mc.GE@lines[[1]]@Lines[[1]]@coords

paste(tmp.sp.BEACON.OR.mc.GE@lines[[1]]@Lines[[1]]@coords[,1],tmp.sp.BEACON.OR.mc.GE@lines[[1]]@Lines[[1]]@coords[,2],"0",collapse=",")

ii <- 5
for(ii in 1:5) {
  junk<-data.frame(x=tmp.sp.BEACON.OR.mc.GE@lines[[ii]]@Lines[[1]]@coords[,1],y=tmp.sp.BEACON.OR.mc.GE@lines[[ii]]@Lines[[1]]@coords[,2],z=0)
  k <- " "
  for(jj in 1:length(junk[,1])) k <- paste(k,paste(junk[jj,1:3],collapse=","),sep=",")
  print(substr(k,3,nchar(k)))
}



paste(paste(junk[1,1:3],collapse=","),paste(junk[2,1:3],collapse=","),sep=",")

paste(tmp.sp.BEACON.OR.mc.GE@lines[[ii]]@Lines[[1]]@coords[,1:2],"0",collapse=",")


plotKML(tmp.sp.beach.ext.or.mc.KML)
plotKML(tmp.sp.stn.on.ext.KML)
## plot in google earth
plotKML(tmp.sp.stn.on.beach.ext.KML)
plotKML(tmp.sp.beach.ext.or.KML)
