## load packages
## none

##
## parameter values
chr.dir.csv.files <- "//deqhq1/tmdl/TMDL_WR/MidCoast/GIS/BacteriaTMDL/rscripts/KML-R-tool/Generate_content/csv_data_files"
chr.dir.graph.files <- "//deqhq1/tmdl/TMDL_WR/MidCoast/GIS/BacteriaTMDL/rscripts/KML-R-tool/Generate_content/graph_files"

##
## load data csv data
list.files(path=chr.dir.csv.files, pattern="[0-9].csv$")
load(file=paste0(chr.dir.data,"/",chr.file.data))
df.data <- df.data[df.data$date.time >= strptime("2000-01-01", format="%Y-%m-%d"), ]

##
## drop previous grouping data
df.data <- df.data[,-1*grep("group",names(df.data))]

##
# change and add information
df.data[grep("^ent$",df.data[,"parameter"]),"parameter"] <- "enterococcus"
df.data <-data.frame(df.data[,1:grep("value",names(df.data))], units="orgs/100ml", df.data[,grep("value",names(df.data)):length(names(df.data))], stringsAsFactors=FALSE)

##
## write data files in csv format
tmp.sites <- unique(df.data[,"site"])
for(ii in 1:length(tmp.sites)) write.csv(df.data[grep(tmp.sites[ii],df.data[,"site"]),],file=paste0(chr.dir.csv.files,"/data_",tmp.sites[ii],".csv"),row.names=FALSE)

## done