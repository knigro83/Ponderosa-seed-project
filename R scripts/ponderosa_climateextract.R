##Extract PRISM data from ponderosa cone plots ##

library(adehabitatHR)
library(raster)
library(foreign)
setwd("C:/Users/Katie/Google Drive/ArcGIS/pondocones")

# set location of climate data

climdir<-'C:/Users/Katie/Google Drive/ArcGIS/PRISM'

# create list of months of interest

months<-c('01','02','03','04','05','06','07','08','09','10','11','12')

# read in dbf file
dbffile <- "sitecones.dbf"
poly<-shapefile("sitecones.shp")
poly.dbf <- read.dbf(dbffile)

# PRCP DATA extract raster data from overlying polygon
ppt_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/ppt/PRISM_ppt_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
  ppt<-ext.poly[,2]
  CN<-poly.dbf[,1]
  month <- rep(months[j],length(ext.poly[,2]))
  ppt_CN<-cbind(CN,ppt,month)
  ppt_data<-rbind(ppt_data,ppt_CN)}

# TMAX DATA extract raster data from overlying polygon                     
tmax_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/tmax/PRISM_tmax_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
  tmax<-ext.poly[,2]
  CN<-poly.dbf[,1]
  month <- rep(months[j],length(ext.poly[,2]))
  tmax_CN<-cbind(CN,tmax,month)
  tmax_data<-rbind(tmax_data,tmax_CN)}

# TMIN DATA extract raster data from overlying polygon
tmin_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/tmin/PRISM_tmin_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
  tmin<-ext.poly[,2]
  CN<-poly.dbf[,1]
  month <- rep(months[j],length(ext.poly[,2]))
  tmin_CN<-cbind(CN,tmin,month)
  tmin_data<-rbind(tmin_data,tmin_CN)}

# TMEAN DATA extract raster data from overlying polygon

tmean_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/tmean/PRISM_tmean_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
  tmean<-ext.poly[,2]
  CN<-poly.dbf[,1]
  month <- rep(months[j],length(ext.poly[,2]))
  tmean_CN<-cbind(CN,tmean,month)
  tmean_data<-rbind(tmean_data,tmean_CN)}

# VPD DATA extract raster data from overlying polygon

vpdmax_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/vpdmax/PRISM_vpdmax_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
  vpdmax<-ext.poly[,2]
  CN<-poly.dbf[,1]
  month <- rep(months[j],length(ext.poly[,2]))
  vpdmax_CN<-cbind(CN,vpdmax,month)
  vpdmax_data<-rbind(vpdmax_data,vpdmax_CN)}

vpdmin_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/vpdmin/PRISM_vpdmin_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
  vpdmin<-ext.poly[,2]
  CN<-poly.dbf[,1]
  month <- rep(months[j],length(ext.poly[,2]))
  vpdmin_CN<-cbind(CN,vpdmin,month)
  vpdmin_data<-rbind(vpdmin_data,vpdmin_CN)}

## COMBINE ALL OF THE CLIMATE DATA TOGETHER #####
climate_data<-cbind(tmax_data,tmin_data, tmean_data, ppt_data, vpdmax_data, vpdmin_data)

####MAKE SURE THAT THE CLIMATE VARIABLES ARE ALL LINED UP WELL #####
all(climate_data[,1]==climate_data[,4])
all(climate_data[,1]==climate_data[,7])
all(climate_data[,10]==climate_data[,13])
all(climate_data[,10]==climate_data[,16])

all(climate_data[,3]==climate_data[,6])
all(climate_data[,6]==climate_data[,9])
all(climate_data[,9]==climate_data[,12])
all(climate_data[,12]==climate_data[,15])
all(climate_data[,15]==climate_data[,18])

####make climate data into a dataframe and then remove duplicate columns ####

climate_df<-as.data.frame(climate_data)
climate_df<-cbind(climate_df[,1:3],climate_df[5],climate_df[8],climate_df[11],
                  climate_df[14],climate_df[17])
View(climate_df)

####Write the climate data as a csv
setwd("C:/Users/Katie/Google Drive")
write.csv(climate_df,"pondocones_climate.csv")
