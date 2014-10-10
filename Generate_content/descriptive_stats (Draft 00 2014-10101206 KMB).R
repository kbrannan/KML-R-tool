## load packages
library(doBy)
library(ggplot2)
library(scales)
library(maptools)
library(rgdal)
library(xtable)
library(knitr)


## load data
chr.dir.data <- "//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Bacteria/Beach"
chr.file.data <- "Beach Ent Data (2014-02-27).RData"


load(file=paste0(chr.dir.data,"/",chr.file.data))

df.data <- df.data[df.data$date.time >= strptime("2000-01-01", format="%Y-%m-%d"), ]

## single sample max criterion
max.crit <- 158

## summary function
sumfun <- function(x,crit=158) {
  n<-length(x)
  n.above<-length(x[x>=crit])
  rate <- 100*(n.above/n)
  c.max <- max(x)
  if(c.max >= crit){
    redc <- 100*(c.max-crit)/c.max
  } else {
    redc <-NA
  }
  return(c(n=n,n.dig=n.above,dig.rate=rate,max.con=c.max,reduc=redc))
}

## summary of descriptive statistics by group and site
tmp.summary <- summaryBy(value~group,data=df.data, FUN=sumfun, crit=max.crit)
tmp.summary <- merge(x=tmp.summary,y=unique(df.data[,c("group","lat.group","lon.group")]))
names(tmp.summary) <- c("group","n","n.above","ratio.above","max.conc","max.reduc","lat.group","lon.group")
tmp.summary.dates <- summaryBy(format(date.time,"%Y-%m-%d")~group,data=df.data, FUN=c(min,max))
names(tmp.summary.dates) <- c("group","start","end")
df.summary <- merge(x=tmp.summary,y=tmp.summary.dates)
write.csv(df.summary,file=paste0(chr.dir.data,"/summary of groups.csv"),row.names=FALSE)
sp.summary <- SpatialPointsDataFrame(coords=cbind(df.summary$lon.group,df.summary$lat.group),data=df.summary,proj4string=CRS('+proj=longlat +datum=NAD83'))
writeOGR(sp.summary,dsn=chr.dir.data,layer="sp.summary",driver="ESRI Shapefile")
rm(list=ls(pattern="^tmp."))
tmp.summary.site <- summaryBy(value~site+lat.site+lon.site+group+lat.group+lon.group+data.source,data=df.data, FUN=sumfun, crit=max.crit)
names(tmp.summary.site) <- c("site","lat.site","lon.site","group","lat.group","lon.group","data.source","n","n.above","ratio.above","max.conc","max.reduc")
tmp.summary.site.dates <- summaryBy(format(date.time,"%Y-%m-%d")~site+group+data.source,data=df.data, FUN=c(min,max))
names(tmp.summary.site.dates) <- c("site","start","end")
df.summary.site <- merge(x=tmp.summary.site,y=tmp.summary.site.dates)
write.csv(df.summary.site,file=paste0(chr.dir.data,"/summary of sites.csv"),row.names=FALSE)
sp.summary.site <- SpatialPointsDataFrame(coords=cbind(df.summary.site$lon.site,df.summary.site$lat.site),data=df.summary.site,proj4string=CRS('+proj=longlat +datum=NAD83'))
writeOGR(sp.summary.site,dsn=chr.dir.data,layer="sp.summary.site",driver="ESRI Shapefile",overwrite=TRUE)
rm(list=ls(pattern="^tmp."))


## get max conc by group and add group 
df.max.group <- summaryBy(value~group,data=df.data, FUN=max)
names(df.max.group) <- c("group","max")
write.csv(df.max.group,file=paste0(chr.dir.data,"/Max Conc by Group.csv"),row.names=FALSE)



## create facor for data where the order is based on the 
## latitude of the station going from North to South
df.group <- df.summary[,c("lat.group","group")]
df.data$group <- factor(df.data$group,levels=df.group[with(df.group, order(lat.group)), ][,2])


## boxplots

# The palette with black:
cbbPalette <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000", "#E69F00")
p1 <- ggplot(data=df.data, aes(x=group,y=value)) +
  geom_boxplot() + 
  labs(list(x="Area",y="Ent conc")) +
  theme(axis.text.x=element_text(angle=45, vjust=0.99,hjust=1)) +
  coord_cartesian(ylim = c(1,10^ceiling(log10(max(df.data$value)))))+
  scale_y_continuous(trans = 'log10',
                     breaks = trans_breaks('log10', function(x) 10^x),
                     labels = trans_format('log10', math_format(10^.x))) +
  geom_hline(yintercept=max.crit, colour="red", size=1.00) + 
  annotate("text",x=1.5,y=1.2*max.crit,label=paste0("SSM = ",max.crit," orgs/100 ml"), colour="red",hjust=0)
  
print(p1)


plot.fn <- paste0(chr.dir.data,"/boxplot-group.png")

png(file=plot.fn, width=11,heigh=8.5, units="in", res=300,bg="transparent")

p1 <- ggplot(data=df.data, aes(x=group,y=value)) +
  geom_boxplot() + 
  labs(list(x="Area",y="Ent conc")) +
  theme(axis.text.x=element_text(angle=45, vjust=0.99,hjust=1)) +
  coord_cartesian(ylim = c(1,10^ceiling(log10(max(df.data$value)))))+
  scale_y_continuous(trans = 'log10',
                     breaks = trans_breaks('log10', function(x) 10^x),
                     labels = trans_format('log10', math_format(10^.x))) +
  geom_hline(yintercept=max.crit, colour="red", size=1.00) + 
  annotate("text",x=length(unique(df.data$group))-1.5,y=1.1*max.crit,label=paste0("SSM = ",max.crit," orgs/100 ml"), colour="red",hjust=0) +
  coord_flip()

print(p1)
dev.off()
