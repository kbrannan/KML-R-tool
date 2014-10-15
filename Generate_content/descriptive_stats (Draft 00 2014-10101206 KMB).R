## load packages
library(brew)

##
## parameter values
chr.dir.csv.files <- "//deqhq1/tmdl/TMDL_WR/MidCoast/GIS/BacteriaTMDL/rscripts/KML-R-tool/Generate_content/csv_data_files"
chr.dir.html.template.files <- "//deqhq1/tmdl/TMDL_WR/MidCoast/GIS/BacteriaTMDL/rscripts/KML-R-tool/html_desc_stat_table_templates"
chr.dir.html.files <- "//deqhq1/tmdl/TMDL_WR/MidCoast/GIS/BacteriaTMDL/rscripts/KML-R-tool/Generate_content/html_desc_stat_files"
chr.html.template.file <- "desc_stat.htmlt"
num.crit <- 158
num.start.year <- 2000
num.end.year   <- 2014
tmp.years <- c("All",as.character(num.start.year:num.end.year))
##
## get csv data information and set data types in files
chr.csv.file.names <- list.files(path=chr.dir.csv.files, pattern="[0-9]\\.csv$",full.names=TRUE)
col.classes <- c(rep("character",5),rep("numeric",3))


##
## create html docs of descriptive statistics for data from each sampling location
for(ii in 1:length(chr.csv.file.names)) {
  tmp.stats <- data.frame(year= tmp.years,
                          N=rep(NA,length(tmp.years)),
                          max=rep(NA,length(tmp.years)),
                          median=rep(NA,length(tmp.years)),
                          NaboveCrit=rep(NA,length(tmp.years)),
                          dte.start=as.POSIXct(rep(NA,length(tmp.years))),
                          dte.end=as.POSIXct(rep(NA,length(tmp.years)))
                          , stringsAsFactors=FALSE)
  ## get data for sampling location from the csv file
  ## this ensures the same data used in the plot is provided
  tmp.data <- read.csv(file=chr.csv.file.names[ii], colClasses = col.classes)
  ## convert date.time from character to POSIXct date-time class
  tmp.data$date <- as.POSIXct(tmp.data$date.time)
  ## all data
  tmp.stats[tmp.stats$year == "All",]$N <- length(tmp.data$value)
  tmp.stats[tmp.stats$year == "All",]$max <- max(tmp.data$value)
  tmp.stats[tmp.stats$year == "All",]$median <- median(tmp.data$value)
  tmp.stats[tmp.stats$year == "All",]$NaboveCrit <- length(tmp.data[tmp.data$value >= num.crit,"value"])
  tmp.stats[tmp.stats$year == "All",]$dte.start <- as.POSIXct(min(tmp.data$date.time))
  tmp.stats[tmp.stats$year == "All",]$dte.end <- as.POSIXct(max(tmp.data$date.time))
  ## annual
  for(jj in 2:length(tmp.stats$year)) {
    tmp.data.year <-tmp.data[tmp.data$date.time >= as.POSIXct(paste0(tmp.stats$year[jj],"-01-01 00:00:00")) & tmp.data$date.time < as.POSIXct(paste0(as.numeric(tmp.stats$year[jj])+1,"-01-01 00:00:00")),]
    if(length(tmp.data.year$value) > 0) {
      tmp.stats[tmp.stats$year == tmp.stats$year[jj],]$N <- length(tmp.data.year$value)
      tmp.stats[tmp.stats$year == tmp.stats$year[jj],]$max <- max(tmp.data.year$value)
      tmp.stats[tmp.stats$year == tmp.stats$year[jj],]$median <- median(tmp.data.year$value)
      tmp.stats[tmp.stats$year == tmp.stats$year[jj],]$NaboveCrit <- length(tmp.data[tmp.data.year$value >= num.crit,"value"])
      tmp.stats[tmp.stats$year == tmp.stats$year[jj],]$dte.start <- as.POSIXct(min(tmp.data.year$date.time))
      tmp.stats[tmp.stats$year == tmp.stats$year[jj],]$dte.end <- as.POSIXct(max(tmp.data.year$date.time))
      rm(tmp.data.year)
    }
  
  }
  ## write html files with desc stat tables for each sample location
  brew(file=paste0(chr.dir.html.template.files,"/",chr.html.template.file),output=paste0(chr.dir.html.files,"/desc_stat_",as.character(unique(tmp.data$site)),".html"))
  
  ## clean up
  rm(tmp.data,tmp.stats,jj,kk)
}


## done



