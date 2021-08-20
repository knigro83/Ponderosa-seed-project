
##Extract PRISM data from ponderosa cone plots ##

library(adehabitatHR)
library(raster)
library(foreign)
setwd("C:/Users/Katie/Google Drive/ArcGIS/Ponderosa project")

# set location of climate data

climdir<-'C:/Users/Katie/Google Drive/ArcGIS/PRISM'

# create list of months of interest

months<-c('01','02','03','04','05','06','07','08','09','10','11','12')

# read in dbf file
dbffile <- "pondo_sites.dbf"
poly<-shapefile("pondo_sites.shp")
poly.dbf <- read.dbf(dbffile)

# PRCP DATA extract raster data from overlying polygon
ppt_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/ppt/PRISM_ppt_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
  ppt<-ext.poly[,2]
  site<-as.character(poly.dbf[,2])
  month <- rep(months[j],length(ext.poly[,2]))
  ppt_site<-cbind(site,ppt,month)
  ppt_data<-rbind(ppt_data,ppt_site)}

# TMAX DATA extract raster data from overlying polygon                     
tmax_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/tmax/PRISM_tmax_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
  tmax<-ext.poly[,2]
  site<-as.character(poly.dbf[,2])
  month <- rep(months[j],length(ext.poly[,2]))
  tmax_site<-cbind(site,tmax,month)
  tmax_data<-rbind(tmax_data,tmax_site)}

# TMIN DATA extract raster data from overlying polygon
tmin_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/tmin/PRISM_tmin_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
  tmin<-ext.poly[,2]
  site<-as.character(poly.dbf[,2])
  month <- rep(months[j],length(ext.poly[,2]))
  tmin_site<-cbind(site,tmin,month)
  tmin_data<-rbind(tmin_data,tmin_site)}

# TMEAN DATA extract raster data from overlying polygon

tmean_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/tmean/PRISM_tmean_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
  tmean<-ext.poly[,2]
  site<-as.character(poly.dbf[,2])
  month <- rep(months[j],length(ext.poly[,2]))
  tmean_site<-cbind(site,tmean,month)
  tmean_data<-rbind(tmean_data,tmean_site)}

# VPD DATA extract raster data from overlying polygon

vpdmax_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/vpdmax/PRISM_vpdmax_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
  vpdmax<-ext.poly[,2]
  site<-as.character(poly.dbf[,2])
  month <- rep(months[j],length(ext.poly[,2]))
  vpdmax_site<-cbind(site,vpdmax,month)
  vpdmax_data<-rbind(vpdmax_data,vpdmax_site)}

vpdmin_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/vpdmin/PRISM_vpdmin_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
  vpdmin<-ext.poly[,2]
  site<-as.character(poly.dbf[,1])
  month <- rep(months[j],length(ext.poly[,2]))
  vpdmin_site<-cbind(site,vpdmin,month)
  vpdmin_data<-rbind(vpdmin_data,vpdmin_site)}

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
setwd("C:/Users/Katie/Google Drive/Ponderosa seed project/climate data")
write.csv(climate_df,"pondosites_climate.csv")

###########################################################################
############# NOW DO THE SAME FOR EXPERIMENTAL SITES ######################
###########################################################################


##Extract PRISM data from ponderosa cone plots ##

library(adehabitatHR)
library(raster)
library(foreign)

#read in coordinates
import <- ogrListLayers("C:/Users/Katie/Google Drive/Ponderosa seed project/gps data/high.kml")
attr(import, "driver")
attr(import, "nlayers")
high.site = readOGR("C:/Users/Katie/Google Drive/Ponderosa seed project/gps data/high.kml", "high.kml") 

import2 <- ogrListLayers("C:/Users/Katie/Google Drive/Ponderosa seed project/gps data/mid.kml")
attr(import2, "driver")
attr(import2, "nlayers")
mid.site = readOGR("C:/Users/Katie/Google Drive/Ponderosa seed project/gps data/mid.kml", "mid.kml") 


# set location of climate data

climdir<-'C:/Users/Katie/Google Drive/ArcGIS/PRISM'

# create list of months of interest

months<-c('01','02','03','04','05','06','07','08','09','10','11','12')
poly.dbf.h<-high.site@data
poly.dbf.m<-mid.site@data

# PRCP DATA extract raster data from overlying polygon
ppt_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/ppt/PRISM_ppt_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- raster::extract(rast, high.site, fun = mean, na.rm=TRUE, df=TRUE)
  ppt<-ext.poly[,2]
  site<-as.character(poly.dbf.h[,1])
  month <- rep(months[j],length(ext.poly[,2]))
  ppt_site<-cbind(site,ppt,month)
  ppt_data<-rbind(ppt_data,ppt_site)}

for(j in 1:length(months)){
  raster<-paste(climdir,'/ppt/PRISM_ppt_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- raster::extract(rast, mid.site, fun = mean, na.rm=TRUE, df=TRUE)
  ppt<-ext.poly[,2]
  site<-as.character(poly.dbf.m[,1])
  month <- rep(months[j],length(ext.poly[,2]))
  ppt_site<-cbind(site,ppt,month)
  ppt_data<-rbind(ppt_data,ppt_site)}

ppt_data

# TMAX DATA extract raster data from overlying polygon                     
tmax_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/tmax/PRISM_tmax_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- raster::extract(rast, high.site, fun = mean, na.rm=TRUE, df=TRUE)
  tmax<-ext.poly[,2]
  site<-as.character(poly.dbf.h[,1])
  month <- rep(months[j],length(ext.poly[,2]))
  tmax_site<-cbind(site,tmax,month)
  tmax_data<-rbind(tmax_data,tmax_site)}
for(j in 1:length(months)){
  raster<-paste(climdir,'/tmax/PRISM_tmax_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- raster::extract(rast, mid.site, fun = mean, na.rm=TRUE, df=TRUE)
  tmax<-ext.poly[,2]
  site<-as.character(poly.dbf.m[,1])
  month <- rep(months[j],length(ext.poly[,2]))
  tmax_site<-cbind(site,tmax,month)
  tmax_data<-rbind(tmax_data,tmax_site)}

# TMIN DATA extract raster data from overlying polygon
tmin_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/tmin/PRISM_tmin_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- raster::extract(rast, high.site, fun = mean, na.rm=TRUE, df=TRUE)
  tmin<-ext.poly[,2]
  site<-as.character(poly.dbf.h[,1])
  month <- rep(months[j],length(ext.poly[,2]))
  tmin_site<-cbind(site,tmin,month)
  tmin_data<-rbind(tmin_data,tmin_site)}
for(j in 1:length(months)){
  raster<-paste(climdir,'/tmin/PRISM_tmin_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- raster::extract(rast, mid.site, fun = mean, na.rm=TRUE, df=TRUE)
  tmin<-ext.poly[,2]
  site<-as.character(poly.dbf.m[,1])
  month <- rep(months[j],length(ext.poly[,2]))
  tmin_site<-cbind(site,tmin,month)
  tmin_data<-rbind(tmin_data,tmin_site)}

# TMEAN DATA extract raster data from overlying polygon

tmean_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/tmean/PRISM_tmean_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- raster::extract(rast, high.site, fun = mean, na.rm=TRUE, df=TRUE)
  tmean<-ext.poly[,2]
  site<-as.character(poly.dbf.h[,1])
  month <- rep(months[j],length(ext.poly[,2]))
  tmean_site<-cbind(site,tmean,month)
  tmean_data<-rbind(tmean_data,tmean_site)}
for(j in 1:length(months)){
  raster<-paste(climdir,'/tmean/PRISM_tmean_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- raster::extract(rast, mid.site, fun = mean, na.rm=TRUE, df=TRUE)
  tmean<-ext.poly[,2]
  site<-as.character(poly.dbf.m[,1])
  month <- rep(months[j],length(ext.poly[,2]))
  tmean_site<-cbind(site,tmean,month)
  tmean_data<-rbind(tmean_data,tmean_site)}

# VPD DATA extract raster data from overlying polygon

vpdmax_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/vpdmax/PRISM_vpdmax_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- raster::extract(rast, high.site, fun = mean, na.rm=TRUE, df=TRUE)
  vpdmax<-ext.poly[,2]
  site<-as.character(poly.dbf.h[,1])
  month <- rep(months[j],length(ext.poly[,2]))
  vpdmax_site<-cbind(site,vpdmax,month)
  vpdmax_data<-rbind(vpdmax_data,vpdmax_site)}
for(j in 1:length(months)){
  raster<-paste(climdir,'/vpdmax/PRISM_vpdmax_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- raster::extract(rast, mid.site, fun = mean, na.rm=TRUE, df=TRUE)
  vpdmax<-ext.poly[,2]
  site<-as.character(poly.dbf.m[,1])
  month <- rep(months[j],length(ext.poly[,2]))
  vpdmax_site<-cbind(site,vpdmax,month)
  vpdmax_data<-rbind(vpdmax_data,vpdmax_site)}

vpdmin_data<-vector()
for(j in 1:length(months)){
  raster<-paste(climdir,'/vpdmin/PRISM_vpdmin_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- raster::extract(rast, high.site, fun = mean, na.rm=TRUE, df=TRUE)
  vpdmin<-ext.poly[,2]
  site<-as.character(poly.dbf.h[,1])
  month <- rep(months[j],length(ext.poly[,2]))
  vpdmin_site<-cbind(site,vpdmin,month)
  vpdmin_data<-rbind(vpdmin_data,vpdmin_site)}
for(j in 1:length(months)){
  raster<-paste(climdir,'/vpdmin/PRISM_vpdmin_30yr_normal_800mM2_',months[j],'_bil.bil',sep="")
  rast <- raster(raster)
  ext.poly <- raster::extract(rast, mid.site, fun = mean, na.rm=TRUE, df=TRUE)
  vpdmin<-ext.poly[,2]
  site<-as.character(poly.dbf.m[,1])
  month <- rep(months[j],length(ext.poly[,2]))
  vpdmin_site<-cbind(site,vpdmin,month)
  vpdmin_data<-rbind(vpdmin_data,vpdmin_site)}

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
setwd("C:/Users/Katie/Google Drive/Ponderosa seed project/climate data")
write.csv(climate_df,"exp_sites_climate.csv")
