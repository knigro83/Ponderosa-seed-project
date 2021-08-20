### Uses final 2019 plot locations to calculate CWD from Miranda's function ###
### need to run "cwd_function_v1.R" first                                   ### 

library(raster)
library(foreign)
library(reshape2)
library(ggplot2)

##read in PRISM data for all sites
site_climate <- read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/climate data/pondosites_climate.csv")

##read in site data
pondosites <- shapefile("C:/Users/Katie/Google Drive/ArcGIS/Ponderosa project/pondo_sites.shp")
pondosites.dbf <- read.dbf("C:/Users/Katie/Google Drive/ArcGIS/Ponderosa project/pondo_sites.dbf")

##read in aspect and slope rasters
aspect.rast = raster("C:/Users/Katie/Google Drive/ArcGIS/aspect_frontrange.tif")
slope.rast = raster("C:/Users/Katie/Google Drive/ArcGIS/slope_frontrange.tif")
elev.rast = raster("C:/Users/Katie/Google Drive/ArcGIS/dem_frontrange.tif")

#extract aspect and slope for plots from mosaic rasters
aspect <- extract(aspect.rast, pondosites, fun = mean, na.rm=TRUE, df=TRUE)
slope <- extract(slope.rast, pondosites, fun = mean, na.rm=TRUE, df=TRUE)
elev <- extract(elev.rast, pondosites, fun=mean, na.rm=TRUE, df=TRUE)

aspect.sites <- cbind(aspect,pondosites.dbf)[,c(4,2,5,6)]
site.data <- cbind(aspect.sites,slope)[,-5]
site.data <- cbind(site.data, elev)[,-6]
site.data$foldedaspect = paste(abs(180-abs(site.data$aspect_frontrange - 225)))
site.data <- merge(site.data,site_climate,by="site")

##calculate CWD with actual soil data 

##Using polaris data to extract awc
setwd("C:/Users/Katie/Google Drive/ArcGIS")
soildir<-'C:/Users/Katie/Google Drive/ArcGIS/polaris_soildata'
ends<- c('0_5-007','5_15-006','15_30-005','30_60-004','60_100-002','100_200-003')
poly=pondosites
poly.dbf = pondosites.dbf

awc_data <- vector()  
for(i in 1:length(ends)){
  raster<-paste(soildir,'/awc_mean_',ends[i],'.tif',sep="")
  rast <- raster(raster)
  ext.poly <- extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
  awc<-ext.poly[,2]
  PlotID<-poly.dbf[,1]
  awc_ID<-cbind(PlotID,awc)
  awc_data<-cbind(awc_data,awc_ID)}

awc_data = awc_data[,c(1,2,4,6,8,10,12)]
colnames(awc_data) = c("PlotID","awc0-5","awc5-15",
                       "awc15-30","awc_30-60",
                       "awc60-100","awc100-200")

#####****NEED TO FIGURE OUT HOW TO MAKE THESE NOT FACTORS*****#######

awc_data[,2] = awc_data[,2]*50
awc_data[,3] = awc_data[,3]*100
awc_data[,4] = awc_data[,4]*150
awc_data[,5] = awc_data[,5]*300
awc_data[,6] = awc_data[,6]*400
awc_data[,7] = awc_data[,7]*1000
awc.sum = apply(awc_data[,c(2:7)], 1, sum)
awc_data = cbind(awc_data,awc.sum)
awc_data = merge(awc_data, poly.dbf[,1:2], by.x="PlotID",by.y="Id")

final.site.data = merge(site.data, awc_data, by="site")

#run cwd fxn with varying soil awc
cwd_normal<-cwd_function(site=final.site.data$PlotID,slope=final.site.data$slope_frontrange,
                                          latitude=final.site.data$lat,foldedaspect=final.site.data$foldedaspect,
                                          ppt=final.site.data$ppt,tmean=final.site.data$tmean,month=final.site.data$month,
                                          soilawc=final.site.data$awc.sum,type="normal")

cwd.table<-aggregate(cwd_normal$cwd, by=list(Category=cwd_normal$site), FUN=sum)
cwd.table
aet.table<-aggregate(cwd_normal$aet, by=list(Category=cwd_normal$site), FUN=sum)
aet.table

result = merge(as.data.frame(poly.dbf[,1:2]),cwd.table,by.x="Id",by.y="Category")
colnames(result)=c("PlotID","site","cwd")
result2 = merge(result, aet.table, by.x="PlotID",by.y="Category")
colnames(result2)[4]="aet"
geog.vars = cbind(pondosites.dbf[,2:ncol(pondosites.dbf)],slope[,2],aspect[,2],elev[,2])
result3 = merge(result2, geog.vars, by="site")
colnames(result3)[7:9]=c("slope","aspect","elev")
#write.csv(result3, "C:/Users/Katie/Google Drive/Ponderosa seed project/site information/site.vars.csv")

result3 = result3[order(result3$lat),]

ggplot(data=result3, aes(x=cwd,y=lat, color=elev))+
    geom_point(size=2.5)+
    scale_color_gradient(low="red", high="blue")+
    geom_text(aes(label=site),hjust=0,vjust=-1)+
    xlim(c(0,400))+
    ylim(c(38,41))


######################## EXTRACT CWD FOR EXPERIMENTAL SITES ######################################
### need to run "cwd_function_v1.R" first                                   ### 

library(raster)
library(foreign)
library(reshape2)
library(ggplot2)

##read in PRISM data for all sites
site_climate <- read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/climate data/exp_sites_climate.csv")

##read in site spatial files
import <- ogrListLayers("C:/Users/Katie/Google Drive/Ponderosa seed project/gps data/high.kml")
attr(import, "driver")
attr(import, "nlayers")
high.site = readOGR("C:/Users/Katie/Google Drive/Ponderosa seed project/gps data/high.kml", "high.kml") 

import2 <- ogrListLayers("C:/Users/Katie/Google Drive/Ponderosa seed project/gps data/mid.kml")
attr(import2, "driver")
attr(import2, "nlayers")
mid.site = readOGR("C:/Users/Katie/Google Drive/Ponderosa seed project/gps data/mid.kml", "mid.kml") 

##read in aspect and slope rasters
aspect.rast = raster("C:/Users/Katie/Google Drive/ArcGIS/front range topo/aspect_frontrange.tif")
slope.rast = raster("C:/Users/Katie/Google Drive/ArcGIS/front range topo/slope_frontrange.tif")
elev.rast = raster("C:/Users/Katie/Google Drive/ArcGIS/front range topo/dem_frontrange.tif")

#extract aspect and slope for plots from mosaic rasters
aspect <- matrix(c("high","mid",raster::extract(aspect.rast, high.site, fun = mean, na.rm=TRUE, df=TRUE)[,2],
                 raster::extract(aspect.rast, mid.site, fun = mean, na.rm=TRUE, df=TRUE)[,2]),2,2)
slope <- matrix(c("high","mid",raster::extract(slope.rast, high.site, fun = mean, na.rm=TRUE, df=TRUE)[,2],
                  raster::extract(slope.rast, mid.site, fun = mean, na.rm=TRUE, df=TRUE)[,2]),2,2)
elev <- matrix(c("high","mid",raster::extract(elev.rast, high.site, fun = mean, na.rm=TRUE, df=TRUE)[,2],
                 raster::extract(elev.rast, mid.site, fun = mean, na.rm=TRUE, df=TRUE)[,2]),2,2)
aspect<-as.data.frame(aspect)
colnames(aspect)=c("site","aspect")
slope<-as.data.frame(slope)
colnames(slope)=c("site","slope")
elev<-as.data.frame(elev)
colnames(elev)=c("site","elev")

site.data<- merge(aspect,slope,by="site")
site.data<- merge(site.data, elev, by="site")
site.data$aspect<-as.numeric(as.character(site.data$aspect))
site.data$slope<-as.numeric(as.character(site.data$slope))
site.data$elev<-as.numeric(as.character(site.data$elev))

site.data$foldedaspect = paste(abs(180-abs(site.data$aspect - 225)))
site.data <- merge(site.data,site_climate,by="site")

##calculate CWD with actual soil data 

##Using polaris data to extract awc
setwd("C:/Users/Katie/Google Drive/ArcGIS")
soildir<-'G:/POLARIS data' #this is my passport 
ends<- c('0_5-001','5_15','15_30','30_60','60_100','100_200')

poly.dbf.h<-high.site@data
poly.dbf.m<-mid.site@data

awc_data_high <- vector()  
for(i in 1:length(ends)){
  raster<-paste(soildir,'/awc_mean_',ends[i],'.tif',sep="")
  rast <- raster(raster)
  ext.poly <- raster::extract(rast, high.site, fun = mean, na.rm=TRUE, df=TRUE)
  awc<-ext.poly[,2]
  awc_data_high<-cbind(awc_data_high,awc)}
awc_data_mid <- vector()  
for(i in 1:length(ends)){
  raster<-paste(soildir,'/awc_mean_',ends[i],'.tif',sep="")
  rast <- raster(raster)
  ext.poly <- raster::extract(rast, mid.site, fun = mean, na.rm=TRUE, df=TRUE)
  awc<-ext.poly[,2]
  awc_data_mid<-cbind(awc_data_mid,awc)}

awc_data_high<-as.data.frame(awc_data_high)
awc_data_mid<-as.data.frame(awc_data_mid)

colnames(awc_data_high) = c("awc0-5","awc5-15",
                       "awc15-30","awc_30-60",
                       "awc60-100","awc100-200")
colnames(awc_data_mid) = c("awc0-5","awc5-15",
                            "awc15-30","awc_30-60",
                            "awc60-100","awc100-200")

awc_data_high$site<- "high"
awc_data_mid$site<- "mid"
awc_data=rbind(awc_data_high,awc_data_mid)

awc_data[,1] = awc_data[,1]*50
awc_data[,2] = awc_data[,2]*100
awc_data[,3] = awc_data[,3]*150
awc_data[,4] = awc_data[,4]*300
awc_data[,5] = awc_data[,5]*400
awc_data[,6] = awc_data[,6]*1000
awc.sum = apply(awc_data[,c(1:6)], 1, sum)
awc_data = cbind(awc_data,awc.sum)

final.site.data = merge(site.data, awc_data, by="site")
site.coords<- as.data.frame(matrix(c("high","mid",high.site@coords[1],mid.site@coords[1],high.site@coords[2],mid.site@coords[2]),2,3))
colnames(site.coords)=c("site","long","lat")
final.site.data = merge(final.site.data,site.coords,by="site")
final.site.data$foldedaspect<-as.numeric(final.site.data$foldedaspect)
final.site.data$long<-as.numeric(as.character(final.site.data$long))
final.site.data$lat<-as.numeric(as.character(final.site.data$lat))

#run cwd fxn with varying soil awc
cwd_normal<-cwd_function(site=final.site.data$site,slope=final.site.data$slope,
                         latitude=final.site.data$lat,foldedaspect=final.site.data$foldedaspect,
                         ppt=final.site.data$ppt,tmean=final.site.data$tmean,month=final.site.data$month,
                         soilawc=final.site.data$awc.sum,type="normal")

cwd.table<-aggregate(cwd_normal$cwd, by=list(Category=cwd_normal$site), FUN=sum)
cwd.table
cwd.table$site=c("high","mid")
cwd.table<-cwd.table[,-1]
colnames(cwd.table)=c("cwd","site")
aet.table<-aggregate(cwd_normal$aet, by=list(Category=cwd_normal$site), FUN=sum)
aet.table
aet.table$site=c("high","mid")
aet.table<-aet.table[,-1]
colnames(aet.table)=c("aet","site")
aet.table

site_chars<-merge(final.site.data,cwd.table,by="site")
site_chars<-merge(site_chars,aet.table,by="site")
site_chars
L = site_chars$lat*0.0174532925
S = site_chars$slope*0.0174532925
A = site_chars$foldedaspect
site_chars$heatload <- paste(0.339+0.808*(cos(L)*cos(S))-0.196*(sin(L)*sin(S))-0.482*(cos(A)*sin(S)))
site_chars$heatload <- as.numeric(as.character(site_chars$heatload))

str(site_chars)
distm(high.site@coords[,1:2],mid.site@coords[,1:2], fun=distGeo)
