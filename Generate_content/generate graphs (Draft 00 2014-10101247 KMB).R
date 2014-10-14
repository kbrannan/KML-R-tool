## load packages
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


tmp.data <- read.csv(file=chr.csv.file.names[1], colClasses = col.classes)
tmp.data$date.time <- as.POSIXct(tmp.data$date.time)
tmp.data.below <- tmp.data[tmp.data$value < num.crit,]
tmp.data.above <- tmp.data[tmp.data$value >= num.crit,]

p.boxplot <- ggplot(tmp.data) +
  geom_boxplot(aes(site,value)) +
  geom_jitter(data=tmp.data.above,aes(site,value),colour="red", size=3) +
  geom_jitter(data=tmp.data.below,aes(site,value),colour="blue", size=2) +
  geom_segment(aes(x=0,xend=2,y=158,yend=158),colour="red", size=1) +
  geom_text(aes(label=" SSM=158 #/100 ml",y=170,x=0, hjust=0, vjust=0, colour="red")) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits= c(10^0,10^4)) +
  theme(legend.position="none",
        plot.margin=unit(c(0,0,0,0),"in")) +
  labs(x="",y="Enteroccous (#/100 ml)")

plot(p.boxplot)

p.ts <- ggplot(tmp.data) +
  geom_point(data=tmp.data.above,aes(as.Date(date.time),value),colour="red",size=3,pch=1) +
  geom_point(data=tmp.data.below,aes(as.Date(date.time),value),colour="blue",size=2,pch=1) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits= c(10^0,10^4)) +
  scale_x_date(limits= c(as.Date("2000-01-01"),as.Date("2014-01-01")), breaks=date_breaks(width="1 year"),minor_breaks=date_breaks("1 month"),labels = date_format("%Y")) +
  geom_segment(aes(x=as.Date("1999-01-01"),xend=as.Date("2014-01-01"),y=158,yend=158),colour="red", size=1) +
  geom_text(aes(label=" SSM=158 #/100 ml",y=170,x=as.Date("1999-11-01"), hjust=0, vjust=0, colour="red")) +
  theme(legend.position="none",
        plot.margin=unit(c(0,0,0,0),"in")) +
  labs(x="",y="Enteroccous (#/100 ml)")

plot(p.ts)


  



tmp <- unique(df.exp.data[,c("lat.site","site")])
tmp.fac <- factor(df.exp.data$site,levels=tmp[with(tmp,order(tmp$lat.site,tmp$site)),"site"], ordered=TRUE)
df.tmp <- data.frame(df.exp.data,site.fac=tmp.fac,SSM=factor(158))

p.boxplot <- ggplot(df.tmp) +
  geom_boxplot(aes(site.fac,value)) +
  geom_segment(aes(x=1,xend=31,y=158,yend=158),colour="red", size=1) +
  geom_text(aes(label="SSM=158 #/100 ml",y=170,x=31, hjust=0, size=2,colour="red")) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme(axis.text.x=element_text(angle=0, size=7), 
        axis.text.y=element_text(angle=0,size=7),
        axis.title.x=element_text(size=7),
        legend.position="none",
        plot.margin=unit(c(0,0,0,0),"in")) +
  coord_flip() +
  labs(x="",y="Enteroccous (#/100 ml)")
png(filename=paste0(chr.dir.images,"/boxplot.png"),width=3,height=4,units="in", res=300)
print(p.boxplot)
dev.off()