library(sp)
library(rgdal)
library(maptools)
library(raster)
library(sf)

jun30 <- raster("C:/Users/Katie/Google Drive/ArcGIS/Ponderosa project/PRISM_daily/PRISM_ppt_provisional_4kmD2_20200630_bil/PRISM_ppt_provisional_4kmD2_20200630_bil.bil")
jun29 <- raster("C:/Users/Katie/Google Drive/ArcGIS/Ponderosa project/PRISM_daily/PRISM_ppt_provisional_4kmD2_20200629_bil/PRISM_ppt_provisional_4kmD2_20200629_bil.bil")
jun28 <- raster("C:/Users/Katie/Google Drive/ArcGIS/Ponderosa project/PRISM_daily/PRISM_ppt_provisional_4kmD2_20200628_bil/PRISM_ppt_provisional_4kmD2_20200628_bil.bil")

import <- ogrListLayers("C:/Users/Katie/Google Drive/Ponderosa seed project/gps data/high.kml")
attr(import, "driver")
attr(import, "nlayers")
high.site = readOGR("C:/Users/Katie/Google Drive/Ponderosa seed project/gps data/high.kml", "high.kml") 

import2 <- ogrListLayers("C:/Users/Katie/Google Drive/Ponderosa seed project/gps data/mid.kml")
attr(import2, "driver")
attr(import2, "nlayers")
mid.site = readOGR("C:/Users/Katie/Google Drive/Ponderosa seed project/gps data/mid.kml", "mid.kml") 

day=formatC(seq(01,31,1),width=2,flag=0)

##August
high.precip.aug=matrix(0,length(day),1)
mid.precip.aug=matrix(0,length(day),1)

for(i in 1:length(day)){
temp.high = extract(raster(paste("C:/Users/Katie/Google Drive/ArcGIS/Ponderosa project/PRISM_daily/PRISM_ppt_early_4kmD2_202008",day[i],"_bil/PRISM_ppt_early_4kmD2_202008",day[i],"_bil.bil",sep="")),
                      high.site)
temp.mid = extract(raster(paste("C:/Users/Katie/Google Drive/ArcGIS/Ponderosa project/PRISM_daily/PRISM_ppt_early_4kmD2_202008",day[i],"_bil/PRISM_ppt_early_4kmD2_202008",day[i],"_bil.bil",sep="")),
                 mid.site)
high.precip.aug[i,1]=temp.high
mid.precip.aug[i,1]=temp.mid}

both.aug = cbind(mid.precip.aug,high.precip.aug)
colnames(both.aug)=c("mid.aug","high.aug")
both.aug

##September
high.precip.sep=matrix(0,length(day),1)
mid.precip.sep=matrix(0,length(day),1)

for(i in 1:length(day)){
  temp.high = extract(raster(paste("C:/Users/Katie/Google Drive/ArcGIS/Ponderosa project/PRISM_daily/PRISM_ppt_early_4kmD2_202009",day[i],"_bil/PRISM_ppt_early_4kmD2_202009",day[i],"_bil.bil",sep="")),
                      high.site)
  temp.mid = extract(raster(paste("C:/Users/Katie/Google Drive/ArcGIS/Ponderosa project/PRISM_daily/PRISM_ppt_early_4kmD2_202009",day[i],"_bil/PRISM_ppt_early_4kmD2_202009",day[i],"_bil.bil",sep="")),
                     mid.site)
  high.precip.sep[i,1]=temp.high
  mid.precip.sep[i,1]=temp.mid}

both.sep = cbind(mid.precip.sep,high.precip.sep)
colnames(both.sep)=c("mid.sep","high.sep")
both.sep


##July
high.precip.jul=matrix(0,length(day),1)
mid.precip.jul=matrix(0,length(day),1)

for(i in 1:length(day)){
  temp.high = extract(raster(paste("C:/Users/Katie/Google Drive/ArcGIS/Ponderosa project/PRISM_daily/PRISM_ppt_early_4kmD2_202007",day[i],"_bil/PRISM_ppt_early_4kmD2_202007",day[i],"_bil.bil",sep="")),
                      high.site)
  temp.mid = extract(raster(paste("C:/Users/Katie/Google Drive/ArcGIS/Ponderosa project/PRISM_daily/PRISM_ppt_early_4kmD2_202007",day[i],"_bil/PRISM_ppt_early_4kmD2_202007",day[i],"_bil.bil",sep="")),
                     mid.site)
  high.precip.jul[i,1]=temp.high
  mid.precip.jul[i,1]=temp.mid}

both.jul = cbind(mid.precip.jul,high.precip.jul)
colnames(both)=c("mid.jul","high.jul")
both.jul

cbind(c(extract(jun30, high.site),
extract(jun30,mid.site),
extract(jun29, high.site),
extract(jun29,mid.site),
extract(jun28, high.site),
extract(jun28,mid.site)),
rep(c("high","mid"),3))


