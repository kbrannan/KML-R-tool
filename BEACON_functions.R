dwnldUnzpBEACON.kmb.sp <- function(dir.sp = "//deqhq1/tmdl/TMDL_WR/MidCoast/GIS/BacteriaTMDL/Beaches/EPA_BEACON") {
  ## get spatial data from EPS beach monitoring site BEACON
  ## http://watersgeo.epa.gov/beacon2/about.html
  ## downloaded shapefiles USEPA Geospatial downloads at
  ## http://water.epa.gov/scitech/datait/tools/waters/data/downloads.cfm#BEACH Datasets
  ## direct link address for shapefiles is:
  ## http://www.epa.gov/waters/data/rad_beach_20140804_shp.zip
  ## the data is dowloaded and unziped to the directory specified by the user (dir.sp)
  ## the downloaded zipfile is saved
  ## dir.BEACON.shp <- the sub-folder under dir.sp where the shapefiles from BEACON are located

  
  ## get beaches in OR
  chr.destfile <- paste0(dir.sp,"/","rad_beach_20140804_shp.zip")
  download.file(url="http://www.epa.gov/waters/data/rad_beach_20140804_shp.zip", destfile = chr.destfile)
  unzip(zipfile=chr.destfile,exdir=dir.sp)
  
  ## get folder for the shapefiles
  ## commands assume that the BEACON shalefile folder will have the longest name
  dir.BEACON.shp <- names(sort(sapply(X=list.dirs(path=dir.sp, full.names=TRUE,recursive=TRUE),FUN=nchar),decreasing=TRUE)[1])
  return(dir.BEACON.shp)
}

subsetBasedOnExtent.kmb.sp <-function(sp.select.ext,sp.select.from) {
  ##
  ## put both sp in CRS used by google earth of "+init=epsg:4326"
  sp.select.ext.KML <- spTransform(sp.select.ext,CRS("+init=epsg:4326"))
  sp.select.from.KML <- spTransform(sp.select.from,CRS("+init=epsg:4326"))
  ##
  ## create a polygon from the bounding box of the extent layer to use for selecting
  #select.bbox <- cbind(floor(bbox(sp.select.ext.KML)[,1]),ceiling(bbox(sp.select.ext.KML)[,2]))
  #select.bbox <- cbind((bbox(sp.select.ext.KML)[,1]),(bbox(sp.select.ext.KML)[,2]))
  select.bbox <- cbind(floor(100*bbox(sp.select.ext.KML)[,1])/100,ceiling(100*bbox(sp.select.ext.KML)[,2])/100)
  select.rec <- rbind(c(select.bbox[1,1],select.bbox[2,2]),c(select.bbox[1,2],select.bbox[2,2]),c(select.bbox[1,2],select.bbox[2,1]),c(select.bbox[1,1],select.bbox[2,1]),c(select.bbox[1,1],select.bbox[2,2]))
  select.sp.rec <- SpatialPolygons(list(Polygons(list(Polygon(select.rec)),1)))
  proj4string(select.sp.rec) <- CRS("+init=epsg:4326")
  ##
  ## select the features that are in the bounding rectangle and return index of features
  select.vec <- as.vector(gIntersects(select.sp.rec,sp.select.from.KML,byid=TRUE))
  ## select features using indexes
  selected.sp <- sp.select.from[select.vec == TRUE,]
  return(selected.sp)
}

line.mid.points.kmb.sp <- function(sp.lines) {
  
  ## functions do not work for coords in geographic units 
  ## transform to Oregon Lambert projection (+init=epsg:2992)
  sp.lines.ft  <- spTransform(sp.lines ,CRS("+init=epsg:2992"))
  sp.mid.points.ft <- SpatialLinesMidPoints(sp.lines.ft)
  sp.mid.points <- spTransform(sp.mid.points.ft,CRS(proj4string(sp.lines)))
  return(sp.mid.points)
}

lineardistance.kmb.sp <- function(sp.points,sp.lines) {
  
  ## functions do not work for coords in geographic units 
  ## transform to Oregon Lambert projection (+init=epsg:2992)
  sp.points.ft <- spTransform(sp.points,CRS("+init=epsg:2992"))
  sp.lines.ft  <- spTransform(sp.lines ,CRS("+init=epsg:2992"))
  
  ## check distances of stations to beach extent lines
  shortest.dists <- numeric(nrow(sp.points.ft@data))
  for (ii in 1:length(shortest.dists)) {
    shortest.dists[ii] <- gDistance(sp.points.ft[ii,], sp.lines.ft)
  }
  df.shortest.dists <- data.frame(site=sp.points.ft@data$site,dist_ft=round(shortest.dists,0))
  return(df.shortest.dists)
}

snapPointsToLine.sp.kmb <- function(sp.points,sp.lines,maxDist=NA) {

  ## functions do not work for coords in geographic units 
  ## transform to Oregon Lambert projection (+init=epsg:2992)
  sp.points.ft <- spTransform(sp.points,CRS("+init=epsg:2992"))
  sp.lines.ft  <- spTransform(sp.lines ,CRS("+init=epsg:2992"))
  ## snap all points no matter how fair away to the lines
  if(is.na(maxDist)) {
    sp.points.snap.ft <- snapPointsToLines(points=sp.points.ft,lines=sp.lines.ft)
  }
  ## snap points within maxDist from the lines to the lines
  if(is.numeric(maxDist)) {
    sp.points.snap.ft <- snapPointsToLines(points=sp.points.ft,lines=sp.lines.ft,maxDist=maxDist)
  }
  ## set projection of snapped points
  proj4string(sp.points.snap.ft) <- proj4string(sp.points.ft)
  ## transform snapped points to CRS of orginial points
  sp.points.snap <- spTransform(sp.points.snap.ft,CRS(proj4string(sp.points)))
  return(sp.points.snap)
}

lineCoordsString <- function(cur.lines) {
  df.coords<-data.frame(x=cur.lines@Lines[[1]]@coords[,1],y=cur.lines@Lines[[1]]@coords[,2],z=0)
  k <- " "
  for(ii in 1:length(df.coords[,1])) k <- paste(k,paste(df.coords[ii,1:3],collapse=","),sep=",")
  return(substr(k,3,nchar(k)))
}

multlinesCoordsString <- function(sp.Lines) {
  tmp.Lines00 <- unlist(coordinates(sp.Lines), recursive=FALSE) # unlist to the first level each item is coordinates of a line
  tmp.Lines01 <- do.call(rbind,tmp.Lines00) ## single matrix with all of the points of the lines
  num.order <- order(-tmp.Lines01[,2]) ## north to south
  tmp.Lines02 <- cbind(tmp.Lines01[num.order,],rep(0,length(tmp.Lines01[,1]))) ## add 0 for elevation (z)
  return(paste(t(tmp.Lines02[,1:3]),collapse=",")) ## create strin that is the 3-tuples for each point of the line
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  return(paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)),sep = "", collapse = " "))
  
}