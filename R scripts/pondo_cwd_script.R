###
### Calculate and compare CWD for ponderosa plots
###

### need to run "cwd_function_v1.R" first  
### example script with soil awc specified as one number across all sites:
### cwd_normal_data<-cwd_function(site=data$site,slope=data$slope,latitude=data$latitude,foldedaspect=data$foldedaspect,ppt=data$ppt,tmean=data$tmean,month=data$month,soilawc=300,type="normal")
library(raster)
library(foreign)
library(reshape2)
library(ggplot2)

#read in data
pondocones.climate <- read.csv("C:/Users/Katie/Google Drive/pondocones_climate.csv")
sitecones = shapefile("C:/Users/Katie/Google Drive/ArcGIS/pondocones/sitecones.shp")
aspect.rast = raster("C:/Users/Katie/Google Drive/ArcGIS/aspect_frontrange.tif")
slope.rast = raster("C:/Users/Katie/Google Drive/ArcGIS/slope_frontrange.tif")
dbf = read.dbf("C:/Users/Katie/Google Drive/ArcGIS/pondocones/sitecones.dbf")
sitecwd = read.dbf("C:/Users/Katie/Google Drive/ArcGIS/pondocones/sitecwd.dbf")

#extract aspect and slope for plots from mosaic rasters
aspect <- extract(aspect.rast, sitecones, fun = mean, na.rm=TRUE, df=TRUE)
slope <- extract(slope.rast, sitecones, fun = mean, na.rm=TRUE, df=TRUE)

#get data together for cwd function
sites <- cbind(aspect, as.data.frame(dbf[,"Field1"]))
new.sites <- cbind(sites, slope[,2])
new.sites = new.sites[,-1]
colnames(new.sites)=c("aspect","PlotID","slope")
coords <- dbf[,c(1,6,7)]
final.sites <- merge(new.sites, coords, by.x="PlotID", by.y="Field1")
final.sites$foldedaspect = paste(abs(180-abs(final.sites$aspect - 225)))
final.sites = merge(final.sites, pondocones.climate, by.x="PlotID", by.y="CN")

#run cwd function
cwd_normal_final.sites<-cwd_function(site=final.sites$PlotID,slope=final.sites$slope,
                              latitude=final.sites$lat,foldedaspect=final.sites$foldedaspect,
                              ppt=final.sites$ppt,tmean=final.sites$tmean,month=final.sites$month,
                              soilawc=300,type="normal")

#clean cwd results 
cwd.table<-na.omit(aggregate(cwd_normal_final.sites$cwd, by=list(Category=cwd_normal_final.sites$site), FUN=sum))
cwd.table
result = merge(as.data.frame(dbf[,1:2]),cwd.table,by.x="Field1",by.y="Category")
colnames(result)=c("PlotID","site","cwd")

#compare cwd from calculation, andreas' data & bioclim data
comp.table = merge(result, sitecwd[,c("Field1","CWD_1981_2","RASTERVALU")], by.x="PlotID", by.y="Field1")
colnames(comp.table)=c("PlotID","site","cwd_calc","cwd_wion","cwd_bioclim")
comp.table

#get data in order for plotting
plot.data=comp.table[,c(1,2,3,5,4)]
plot.data=melt(plot.data, id.vars=c("PlotID","site"))
plot.data$site <- factor(plot.data$site, levels = c("FC","Borden","Linhart","BH","HM1","HM2","HM3","HM4",
                                                    "HM5","HM6","MGa","MGb","WT2","WT1"))
#plot cwds for each site and calculation method 
ggplot(plot.data,aes(site, value, fill=variable))+
    geom_bar(stat="Identity",position=position_dodge(),col="black")+
    scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
    
    theme_minimal()

##try calculating CWD with actual soil data 

##Using polaris data to extract awc
setwd("C:/Users/Katie/Google Drive/ArcGIS")
soildir<-'C:/Users/Katie/Google Drive/ArcGIS/polaris_soildata'
ends<- c('0_5-007','5_15-006','15_30-005','30_60-004','60_100-002','100_200-003')
poly=sitecones
poly.dbf = dbf

awc_data <- vector()  
for(i in 1:length(ends)){
  raster<-paste(soildir,'/awc_mean_',ends[i],'.tif',sep="")
  rast <- raster(raster)
  ext.poly <- extract(rast, poly, fun = mean, na.rm=TRUE, df=TRUE)
  awc<-ext.poly[,2]
  PlotID<-poly.dbf[,1]
  awc_ID<-cbind(PlotID,awc)
  awc_data<-cbind(awc_data,awc_ID)}
awc_data = as.data.frame(awc_data[,c(1,2,4,6,8,10,12)])
colnames(awc_data) = c("PlotID","awc0-5","awc5-15",
                       "awc15-30","awc_30-60",
                       "awc60-100","awc100-200")
awc_data[,2] = awc_data[,2]*50
awc_data[,3] = awc_data[,3]*100
awc_data[,4] = awc_data[,4]*150
awc_data[,5] = awc_data[,5]*300
awc_data[,6] = awc_data[,6]*400
awc_data[,7] = awc_data[,7]*1000
awc_data$awc.sum = apply(awc_data[,c(2:7)], 1, sum)

final.sites.soil = merge(final.sites, awc_data, by="PlotID")

#run cwd fxn with varying soil awc
cwd_normal_final.sites.soil<-cwd_function(site=final.sites.soil$PlotID,slope=final.sites.soil$slope,
                                     latitude=final.sites.soil$lat,foldedaspect=final.sites.soil$foldedaspect,
                                     ppt=final.sites.soil$ppt,tmean=final.sites.soil$tmean,month=final.sites.soil$month,
                                     soilawc=final.sites.soil$awc.sum,type="normal")

cwd.table2<-na.omit(aggregate(cwd_normal_final.sites.soil$cwd, by=list(Category=cwd_normal_final.sites.soil$site), FUN=sum))
cwd.table2

result2 = merge(as.data.frame(dbf[,1:2]),cwd.table2,by.x="Field1",by.y="Category")
colnames(result2)=c("PlotID","site","cwd")

#compare cwd from calculation, andreas' data & bioclim data
comp.table2 = merge(result2, comp.table[,c(1,3:5)], by="PlotID")
colnames(comp.table2)=c("PlotID","site","cwd_calc_soil","cwd_calc","cwd_wion","cwd_bioclim")

#get data in order for plotting
plot.data2=comp.table2[,c(1,2,4,3,6,5)]
plot.data2=melt(plot.data2, id.vars=c("PlotID","site"))
plot.data2$site <- factor(plot.data2$site, levels = c("FC","Borden","Linhart","BH","HM1","HM2","HM3","HM4",
                                                    "HM5","HM6","MGa","MGb","WT2","WT1"))
#plot cwds for each site and calculation method 
ggplot(plot.data2,aes(site, value, fill=variable))+
  geom_bar(stat="Identity",position=position_dodge(),col="black")+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","yellowgreen"))+
  theme_minimal()

#write csv comparing cwd for all sites & measurement types
#write.csv(comp.table2, "C:/Users/Katie/Google Drive/Ponderosa seed project/cwd_comparison.csv")

