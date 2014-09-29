getBEACONsp <- function(dir.sp = "//deqhq1/tmdl/TMDL_WR/MidCoast/GIS/BacteriaTMDL/Beaches/EPA_BEACON") {
  ## get spatial data from EPS beach monitoring site BEACON
  ## downloaded shapefiles USEPA Geospatial downloads at
  ## http://water.epa.gov/scitech/datait/tools/waters/data/downloads.cfm#BEACH Datasets
  ## direct link address for shapefiles is:
  ## http://www.epa.gov/waters/data/rad_beach_20140804_shp.zip

  
  ## get beaches in OR
  chr.destfile <- paste0(dir.sp,"/","rad_beach_20140804_shp.zip")
  download.file(url="http://www.epa.gov/waters/data/rad_beach_20140804_shp.zip", destfile = chr.destfile)
  unzip(zipfile=chr.destfile,exdir=dir.sp)
}
  