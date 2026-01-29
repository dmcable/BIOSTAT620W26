#####  NCDC data download and processing #####
#####  


library(R.utils)

# weather station information in separate file
# pull latest station information
download.file("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv", "/Users/meredith/Dropbox (Personal)/NCDC/stationlist.csv", method='wget') 

stations <- read.csv("/Users/meredith/Dropbox (Personal)/NCDC/stationlist.csv", stringsAsFactors = F) 

# select stations for download
# continental US only, remove AK, HI, territories and weather stations with missing WBAN numbers
st_us<-stations[stations$CTRY=="US" & 
                  stations$STATE!="AK" &
                  stations$STATE!="HI" &
                  stations$STATE !="" &
                  stations$STATE !="PR" &
                  stations$STATE !="VI" &
                  stations$WBAN<99999 &
                  stations$USAF<999999,]

#check which states are included
table(st_us$STATE)

# Remove Islands, platforms, and buoys
st_us<-st_us[-(grep(c("BUOY|ISLAND|PLATFORM"),st_us$STATION.NAME)),]

# Extract year from station start and end date
st_us$BEGIN_YR <- as.numeric(substr(st_us$BEGIN, 1, 4))
st_us$END_YR <- as.numeric(substr(st_us$END, 1, 4))
st_us$USAF <- as.numeric(st_us$USAF)

# Only take stations that have current data (>=2019)
st_us <- st_us[st_us$END_YR >= 2019,]

# Check number of stations
dim(st_us)

### Begin data download based on station list
met_list_all<-vector('list')

# download zip files, unzip, subset
for (y in 2019){
  y_list<-st_us[st_us$BEGIN_YR<=y & st_us$END_YR>=y,]

  # Download from NOAA ftp 
  for (s in 1:dim(y_list)[1]){
    filename<-paste(sprintf("%06d",y_list[s,1]),"-",sprintf("%05d",y_list[s,2]),"-",y,".gz",sep="")
    download.file(paste("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/", y,"/",filename,sep=""), 
                  paste("/Users/meredith/Dropbox (Personal)/NCDC/national/",filename, sep=""),
                  method='wget') 
  }
  # unzip
  files_gz <- list.files("/Users/meredith/Dropbox (Personal)/NCDC/national",full.names=TRUE,pattern=".gz")
    
    for(i in 1:length(files_gz)){
       gunzip(files_gz[[i]],overwrite=TRUE)
    }
  
  # Extract data from downloaded files
  # Need to define column widths, see ftp://ftp.ncdc.noaa.gov/pub/data/noaa/ish-format-document.pdf
  column_widths <- c(4, 6, 5, 4, 2, 2, 2, 2, 1, 6, 7, 5, 5, 5, 4, 3, 1, 1, 4, 1, 5, 1, 1, 1, 6, 1, 1, 1, 5, 1, 5, 1, 5, 1)

  met_files <- list.files("/Users/meredith/Dropbox (Personal)/NCDC/national/",pattern=paste("*-",y,sep=""),full.names=TRUE,include.dirs = FALSE, recursive=FALSE)
  met_list<-vector('list',length(met_files))

  for (i in 1:length(met_files)) {
    if (file.info(met_files[i])$size>0){
      met_data <- read.fwf(met_files[i], column_widths)
      names(met_data) <- c("ID","USAFID", "WBAN", "year", "month","day", "hour", "min","srcflag", "lat", "lon",
                           "typecode","elev","callid","qcname","wind.dir", "wind.dir.qc","wind.type.code","wind.sp","wind.sp.qc",
                           "ceiling.ht","ceiling.ht.qc","ceiling.ht.method","sky.cond","vis.dist","vis.dist.qc","vis.var","vis.var.qc",
                           "temp","temp.qc", "dew.point","dew.point.qc","atm.press","atm.press.qc")
     
       # change 9999, 99999, 999999 to NA
      met_data$wind.dir<-ifelse(met_data$wind.dir==999,NA,met_data$wind.dir)
      met_data$wind.sp<-ifelse(met_data$wind.sp==9999,NA,met_data$wind.sp)
      met_data$ceiling.ht<-ifelse(met_data$ceiling.ht==99999,NA,met_data$ceiling.ht)
      met_data$vis.dist<-ifelse(met_data$vis.dist==999999,NA,met_data$vis.dist)
      met_data$temp<-ifelse(met_data$temp==9999,NA,met_data$temp)
      met_data$dew.point<-ifelse(met_data$dew.point==9999,NA,met_data$dew.point)
      met_data$atm.press<-ifelse(met_data$atm.press==99999,NA,met_data$atm.press)
      
      # conversions and scaling factors
      met_data$lat <- met_data$lat/1000
      met_data$lon <- met_data$lon/1000
      met_data$wind.sp <- met_data$wind.sp/10
      met_data$temp <- met_data$temp/10
      met_data$dew.point <- met_data$dew.point/10
      met_data$atm.press<- met_data$atm.press/10
      met_data$rh=100*((112-0.1*met_data$temp+met_data$dew.point)/(112+0.9*met_data$temp))^8
      
      #drop some variables
      met_data<-subset(met_data, select=-c(ID,srcflag,typecode,callid,qcname))
      # keep august only for class example
      met_data<-met_data[met_data$month==8, ]
      
      met_list[[i]]<-met_data
    }
    met_list_all[[y]]<- do.call("rbind", met_list) 
  }
}

# combine into one file
met<- do.call("rbind", met_list_all) 


# write csv
write.csv(met_combined,"/Users/meredith/Dropbox (Personal)/NCDC/national/met_all_082019.csv",row.names=FALSE)
# fwrite csv and compress
fwrite(met_combined, file = "/Users/meredith/Dropbox (Personal)/NCDC/national/met_all_082019.gzip", compress="gzip")
# write as RData
save(met_combined, file = "/Users/meredith/Dropbox (Personal)/NCDC/national/met_all_082019.RData")

# visual Check
met_points<-unique(met_combined[,8:9])

library(leaflet)
leaflet(met_points) %>% 
  addProviderTiles('CartoDB.Positron') %>% 
  addCircles(lat=~lat,lng=~lon, opacity=1, fillOpacity=1, radius=500)
