## load packages
library(ggplot2)
library(scales)
library(grid)

##
## parameter values
chr.dir.csv.files <- "//deqhq1/tmdl/TMDL_WR/MidCoast/GIS/BacteriaTMDL/rscripts/KML-R-tool/Generate_content/csv_data_files"
chr.dir.graph.files <- "//deqhq1/tmdl/TMDL_WR/MidCoast/GIS/BacteriaTMDL/rscripts/KML-R-tool/Generate_content/graph_files"
num.crit <- 158

##
## get csv data information and set data types in files
chr.csv.file.names <- list.files(path=chr.dir.csv.files, pattern="[0-9]\\.csv$",full.names=TRUE)
col.classes <- c(rep("character",5),rep("numeric",3))

##
## create boxplot and time-series plot for data from each sampling location
for(ii in 1:length(chr.csv.file.names)) {
  ## get data for sampling location from the csv file
  ## this ensures the same data used in the plot is provided
  tmp.data <- read.csv(file=chr.csv.file.names[ii], colClasses = col.classes)
  ## convert date.time from character to POSIXct date-time class
  tmp.data$date.time <- as.POSIXct(tmp.data$date.time)
  ## sub-set data based on criterion
  tmp.data.below <- tmp.data[tmp.data$value < num.crit,]
  tmp.data.above <- tmp.data[tmp.data$value >= num.crit,]
  ##
  ## create boxplot ggplot object
  p.boxplot <- ggplot(tmp.data) +
    geom_boxplot(aes(site,value)) +
    geom_jitter(data=tmp.data.above,aes(site,value),colour="red", size=6, pch=1) +
    geom_jitter(data=tmp.data.below,aes(site,value),colour="blue", size=3,pch=1) +
    geom_segment(aes(x=0,xend=2,y=158,yend=158),colour="red", size=1) +
    geom_text(aes(label=" SSM=158 #/100 ml",y=170,x=0, hjust=0, vjust=0, colour="red")) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(10^0,10^4)) +
    scale_x_discrete(labels=NULL,breaks=NULL) +
    ggtitle(paste(paste0("Location ID:",unique(tmp.data$site)),paste0("Data Source:",unique(tmp.data$data.source)),sep="  ")) + 
    theme(legend.position="none",
          plot.margin=unit(c(0.5,0.5,0.5,0.5),"in"),
          text=element_text(size=14)) +
    labs(x="",y="Enteroccous (#/100 ml)")
  ## write boxplot to file using the loation id in file name as unique identifier
  png(filename=paste0(chr.dir.graph.files,"/location_",unique(tmp.data$site),"_boxplot.png"),width=11,height=8.5,units="in", res=300)
  print(p.boxplot)
  dev.off()
  ##
  ## create time-series plot as ggplot object
  p.ts <- ggplot(tmp.data) +
    geom_point(data=tmp.data,aes(as.Date(date.time),value),colour="black",size=1) +
    geom_point(data=tmp.data.above,aes(as.Date(date.time),value),colour="red",size=6,pch=1) +
    geom_point(data=tmp.data.below,aes(as.Date(date.time),value),colour="blue",size=3,pch=1) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x)),
                  limits= c(10^0,10^4)) +
    scale_x_date(limits= c(as.Date("2000-01-01"),as.Date("2014-01-01")), breaks=date_breaks(width="1 year"),minor_breaks=date_breaks("1 month"),labels = date_format("%Y")) +
    geom_segment(aes(x=as.Date("1999-01-01"),xend=as.Date("2014-01-01"),y=158,yend=158),colour="red", size=1) +
    geom_text(aes(label=" SSM=158 #/100 ml",y=170,x=as.Date("1999-11-01"), hjust=0, vjust=0, colour="red")) +
    ggtitle(paste(paste0("Location ID:",unique(tmp.data$site)),paste0("Data Source:",unique(tmp.data$data.source)),sep="  ")) + 
    labs(x="",y="Enteroccous (#/100 ml)") + 
    theme(legend.position="none",
          plot.margin=unit(c(0.5,0.5,0.5,0.5),"in"),
          text=element_text(size=14))
  png(filename=paste0(chr.dir.graph.files,"/location_",unique(tmp.data$site),"_ts.png"),width=11,height=8.5,units="in", res=300)
  print(p.ts)
  dev.off()
  ## write time-series plot to file using the loation id in file name as unique identifier
  ## clean up
  rm(tmp.data,tmp.data.above,tmp.data.below, p.boxplot,p.ts)  
}

## done