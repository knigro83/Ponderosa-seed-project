### analysis of ponderosa field data
### created 9/3/20
### edited significantly 02/03/21

library(ggplot2)
library(ggthemes)
library(paletteer)
library(ggpubr)
library(maditr)
library(lme4)
library(dplyr)
library(geosphere)
library(truncnorm)
library(ggeffects)
library(visreg)
library(data.table)
library(ggExtra)
library(tidyverse)
library(reshape2)
library(MuMIn)
library(lsmeans)
library(multcomp)
library(multcompView)
library(car)
library(Hmisc)
library(lubridate)
library(sp)
library(rgdal)
library(maptools)
library(raster)
library(sf)
library(DescTools)

germ.data <- read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/data/pondo_field_germ_data_entered.csv")
colnames(germ.data)

for(i in 1:nrow(germ.data)){
  germ.data$num.died[i] = max(germ.data[i,c(8,9,11,13,15,17,19,21,23,25,27,29)])-germ.data[i,"seedling.cnt.11.19"]
}

options(na.action="na.omit")

sum.moms.plots = as.data.frame(aggregate(seedling.cnt.11.19~moms+plot,data=germ.data,function(x) sum(x)))
sum.moms.plots = cbind(sum.moms.plots,as.data.frame(aggregate(num.died~moms+plot, data=germ.data,function(x) sum(x))))
sum.moms.plots = sum.moms.plots[,-c(4,5)]
sum.moms.plots$seeds.sown = 30

##look at number of seeds germinated per family
num.germ<- aggregate(seedling.cnt.11.19+num.died~moms,sum.moms.plots, function(x)sum(x))
par(mfrow=c(1,1))
plot(num.germ)

#look at week of first emergence
germ.data$uid.full<- paste(germ.data$plot,germ.data$quad,germ.data$row,germ.data$column,sep=".")
wk1<- germ.data[germ.data$seedling.cnt.5.27>0,]
wk2 <- germ.data[which(germ.data$seedling.cnt.6.3.4>0&
                         germ.data$seedling.cnt.5.27==0),]
wk3<- germ.data[which(germ.data$seedling.cnt.6.10.12>0&
                        germ.data$seedling.cnt.5.27==0&
                        germ.data$seedling.cnt.6.3.4==0),]
wk4<- germ.data[which(germ.data$seedling.cnt.6.16.18>0&
                        germ.data$seedling.cnt.5.27==0&
                        germ.data$seedling.cnt.6.3.4==0&
                        germ.data$seedling.cnt.6.10.12==0),]
wk5 <- germ.data[which(germ.data$seedling.cnt.6.23.25>0&
                         germ.data$seedling.cnt.5.27==0&
                         germ.data$seedling.cnt.6.3.4==0&
                         germ.data$seedling.cnt.6.10.12==0&
                         germ.data$seedling.cnt.6.16.18==0),]
wk6 <- germ.data[which(germ.data$seedling.cnt.6.30.7.1>0&
                         germ.data$seedling.cnt.5.27==0&
                         germ.data$seedling.cnt.6.3.4==0&
                         germ.data$seedling.cnt.6.10.12==0&
                         germ.data$seedling.cnt.6.16.18==0&
                         germ.data$seedling.cnt.6.23.25==0),]
wk7 <- germ.data[which(germ.data$seedling.cnt.7.7.8>0&
                         germ.data$seedling.cnt.5.27==0&
                         germ.data$seedling.cnt.6.3.4==0&
                         germ.data$seedling.cnt.6.10.12==0&
                         germ.data$seedling.cnt.6.16.18==0&
                         germ.data$seedling.cnt.6.23.25==0&
                         germ.data$seedling.cnt.6.30.7.1==0),]
wk8 <- germ.data[which(germ.data$seedling.cnt.7.15>0&
                         germ.data$seedling.cnt.5.27==0&
                         germ.data$seedling.cnt.6.3.4==0&
                         germ.data$seedling.cnt.6.10.12==0&
                         germ.data$seedling.cnt.6.16.18==0&
                         germ.data$seedling.cnt.6.23.25==0&
                         germ.data$seedling.cnt.6.30.7.1==0&
                         germ.data$seedling.cnt.7.7.8==0),]
wk9 <- germ.data[which(germ.data$seedling.cnt.7.22.23>0&
                         germ.data$seedling.cnt.5.27==0&
                         germ.data$seedling.cnt.6.3.4==0&
                         germ.data$seedling.cnt.6.10.12==0&
                         germ.data$seedling.cnt.6.16.18==0&
                         germ.data$seedling.cnt.6.23.25==0&
                         germ.data$seedling.cnt.6.30.7.1==0&
                         germ.data$seedling.cnt.7.7.8==0&
                         germ.data$seedling.cnt.7.15==0),]
wk10 <- germ.data[which(germ.data$seedling.cnt.7.29>0&
                          germ.data$seedling.cnt.5.27==0&
                          germ.data$seedling.cnt.6.3.4==0&
                          germ.data$seedling.cnt.6.10.12==0&
                          germ.data$seedling.cnt.6.16.18==0&
                          germ.data$seedling.cnt.6.23.25==0&
                          germ.data$seedling.cnt.6.30.7.1==0&
                          germ.data$seedling.cnt.7.7.8==0&
                          germ.data$seedling.cnt.7.15==0&
                          germ.data$seedling.cnt.7.22.23==0),]
wk11<- germ.data[which(germ.data$seedling.cnt.8.6>0&
                         germ.data$seedling.cnt.5.27==0&
                         germ.data$seedling.cnt.6.3.4==0&
                         germ.data$seedling.cnt.6.10.12==0&
                         germ.data$seedling.cnt.6.16.18==0&
                         germ.data$seedling.cnt.6.23.25==0&
                         germ.data$seedling.cnt.6.30.7.1==0&
                         germ.data$seedling.cnt.7.7.8==0&
                         germ.data$seedling.cnt.7.15==0&
                         germ.data$seedling.cnt.7.22.23==0&
                         germ.data$seedling.cnt.7.29==0),]
germ.data$wk.emerg<- ifelse(germ.data$uid.full %in% wk1$uid.full, 1,
                            ifelse(germ.data$uid.full %in% wk2$uid.full,2,
                                   ifelse(germ.data$uid.full %in% wk3$uid.full,3,
                                          ifelse(germ.data$uid.full %in% wk4$uid.full,4,
                                                 ifelse(germ.data$uid.full %in% wk5$uid.full, 5,
                                                        ifelse(germ.data$uid.full %in% wk6$uid.full,6,
                                                               ifelse(germ.data$uid.full %in% wk7$uid.full, 7,
                                                                      ifelse(germ.data$uid.full %in% wk8$uid.full, 8,
                                                                             ifelse(germ.data$uid.full %in% wk9$uid.full,9,
                                                                                    ifelse(germ.data$uid.full %in% wk10$uid.full, 10,
                                                                                           ifelse(germ.data$uid.full %in% wk11$uid.full,11,0)))))))))))
plot(wk.emerg~moms,germ.data[germ.data$wk.emerg>0,])
aggregate(wk.emerg~moms, germ.data[germ.data$wk.emerg>0,], min)
##seed count exceptions
exception.fams8 = c("WT1T11","WT2T3","HM6T6","WT2T2","WT1T12")
exception.fams9 = "WT1T9"

sum.moms.plots[sum.moms.plots$moms %in% exception.fams8,]$seeds.sown = 24
sum.moms.plots[sum.moms.plots$moms %in% exception.fams9,]$seeds.sown = 27
sum.moms.plots[which(sum.moms.plots$moms == "BHT6" &
                       sum.moms.plots$plot == "HH1"),]$seeds.sown = 29
sum.moms.plots[which(sum.moms.plots$moms == "WT1T14" &
                       sum.moms.plots$plot == "HH3"),]$seeds.sown = 28
sum.moms.plots[which(sum.moms.plots$moms == "LHT123M" &
                       sum.moms.plots$plot == "HH5"),]$seeds.sown = 29
sum.moms.plots[which(sum.moms.plots$moms == "BHT10" &
                       sum.moms.plots$plot == "HH5"),]$seeds.sown = 29
sum.moms.plots[which(sum.moms.plots$moms == "BDT2" &
                       sum.moms.plots$plot == "HH6"),]$seeds.sown = 29

sum.moms.plots$yr1rec=sum.moms.plots$seedling.cnt.11.19/sum.moms.plots$seeds.sown
sum.moms.plots$pop = substr(sum.moms.plots$moms,0,3)
sum.moms.plots$site = substr(sum.moms.plots$plot,0,2)
sum.moms.plots$seeds.dead = sum.moms.plots$seeds.sown-sum.moms.plots$seedling.cnt.11.19

###make data frame with each seed as a row
M=matrix(0,0,4)
for(i in 1:nrow(sum.moms.plots)){
  M.temp = matrix(c(rep(as.character(sum.moms.plots[i,c("moms")]),sum.moms.plots[i,"seeds.sown"]),rep(as.character(sum.moms.plots[i,c("plot")]),
             sum.moms.plots[i,"seeds.sown"]),
             rep(1,sum.moms.plots[i,"seedling.cnt.11.19"]+sum.moms.plots[i,"num.died"]),
             rep(0,sum.moms.plots[i,"seeds.sown"]-(sum.moms.plots[i,"seedling.cnt.11.19"]+sum.moms.plots[i,"num.died"])),
             rep(1,sum.moms.plots[i,"seedling.cnt.11.19"]),
             rep(0,sum.moms.plots[i,"seeds.sown"]-sum.moms.plots[i,"seedling.cnt.11.19"])),sum.moms.plots[i,"seeds.sown"],4)
  M = rbind(M,M.temp)
  }
M<-as.data.frame(M)
field.data.long<-M
colnames(field.data.long)=c("mom","plot","germ","surv")
field.data.long$germ<-as.numeric(as.character(field.data.long$germ))
field.data.long$surv<-as.numeric(as.character(field.data.long$surv))
aggregate(germ~mom+plot,field.data.long,function(x)sum(x))$germ ==
  sum.moms.plots[,"seedling.cnt.11.19"]+sum.moms.plots[,"num.died"]
aggregate(surv~mom+plot,field.data.long,function(x)sum(x))$surv ==
  sum.moms.plots[,"seedling.cnt.11.19"]
field.data.long$pop = substr(field.data.long$mom,0,3)
field.data.long$site = substr(field.data.long$plot,0,2) 
  
##look at % germination
germination<- cbind(sum.moms.plots[,1:2],
                    (sum.moms.plots$seedling.cnt.11.19+sum.moms.plots$num.died)/sum.moms.plots$seeds.sown)
colnames(germination)[3]="prop.germ"
head(germination)
boxplot(prop.germ~moms,germination)
ggplot(germination,aes(x=moms,y=prop.germ,fill=pop))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))
germination$pop =  substr(germination$moms,0,3)
boxplot(prop.germ~pop,germination[!germination$plot%in%c("MH1","MH2","MH3","MH4","MH5","MH6"),])
aggregate(prop.germ~moms,data=germination,mean)
boxplot(prop.germ~plot,germination)

##checking for rows where sowing may have been skipped
test.data=germ.data[,c(6,7,8,9,seq(11,29,2))]
test.melt=melt(test.data, id.vars = c("moms","uid"))
test.agg=aggregate(value~moms+uid,data=test.melt,function(x)sum(x))
test.agg[test.agg$value==0,]


ggplot(sum.moms.plots,aes(x=moms, y=yr1rec,fill=site))+
  geom_boxplot(position="dodge")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(sum.moms.plots,aes(x=pop, y=yr1rec,fill=site))+
  geom_boxplot(position="dodge")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



##add population variables to data table
pop.vars <- read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/site information/site.vars.csv")
pop.vars$site = dplyr::recode(pop.vars$site, BD="BDT",BH="BHT",FC="FCT",LH="LHT")
pop.vars<- pop.vars[,!colnames(pop.vars)%in%c("X","PlotID")]

#add survival percentage to data table
sum.moms.plots$yr1surv<- sum.moms.plots$seedling.cnt.11.19/(sum.moms.plots$seedling.cnt.11.19+sum.moms.plots$num.died)
#
rec.data = merge(sum.moms.plots,pop.vars,by.x="pop",by.y="site")
##calculated folded aspect, heat load, and distance to study site
rec.data$foldedaspect <- abs(180-abs(rec.data$aspect - 225)) 
L = rec.data$lat*0.0174532925
S = rec.data$slope*0.0174532925
A = rec.data$foldedaspect
rec.data$heatload <- paste(0.339+0.808*(cos(L)*cos(S))-0.196*(sin(L)*sin(S))-0.482*(cos(A)*sin(S)))
rec.data$heatload <- as.numeric(as.character(rec.data$heatload))

coords = as.data.frame(cbind(unique(rec.data$pop),unique(rec.data[,c("long","lat")])))
mh.loc = c(-105.5241, 40.64676) 
coords$long <- as.numeric(coords$long)
coords$lat <- as.numeric(coords$lat)

dist.to.mid =as.data.frame(cbind(unique(rec.data$pop),
                                  distm(coords[,c("long","lat")],mh.loc, fun=distGeo)))
colnames(dist.to.mid)=c("pop","dist_m")
dist.to.mid$pop = as.character(dist.to.mid$pop)
dist.to.mid$dist_m = as.numeric(as.character(dist.to.mid$dist_m))

rec.data <- merge(rec.data,dist.to.mid,by="pop")

##################################
####Visualize data

ggplot(rec.data, aes(x=pop,y=yr1surv,group=site,color=site))+
  stat_summary(fun.data = "mean_se")
ggplot(rec.data, aes(x=pop,y=yr1surv,colour=plot,group=plot))+
  geom_line()

par(mfrow=c(1,1))
plot(yr1rec~dist_m, data=rec.data)

par(mfrow=c(2,2))
plot(yr1rec~cwd, data=rec.data[rec.data$site=="HH",],main="HH")
plot(yr1rec~cwd, data=rec.data[rec.data$site=="MH",],main="MH")
plot(yr1rec~cwd, data=rec.data[rec.data$site=="ML",],main="ML")

ggplot(rec.data, aes(x=plot, y=yr1surv,fill=site))+
  geom_boxplot()

hh.plot<- ggplot(sum.moms.plots[sum.moms.plots$site=="HH",], aes(x=pop,y=yr1surv))+
  geom_boxplot(position="dodge")
mh.plot<- ggplot(sum.moms.plots[sum.moms.plots$site=="MH",], aes(x=pop,y=yr1surv))+
  geom_boxplot(position="dodge")
ml.plot<- ggplot(sum.moms.plots[sum.moms.plots$site=="ML",], aes(x=pop,y=yr1surv))+
  geom_boxplot(position="dodge")

ggarrange(nrow=3,ncol=1,hh.plot, mh.plot, ml.plot)

#make data without ML plots
sum.moms.plots.noml <- sum.moms.plots[!sum.moms.plots$site=="ML",]

ggplot(sum.moms.plots,aes(x=moms,y=yr1surv,fill=site))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=18),
        axis.text.y=element_text(size=18))
ggplot(sum.moms.plots.noml,aes(x=moms,y=yr1surv,fill=pop))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=18),
        axis.text.y=element_text(size=18))
ggplot(sum.moms.plots,aes(x=pop,y=yr1surv,fill=site))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=18),
        axis.text.y=element_text(size=18))

##################################
##compile plot VWC and temperature
vwc<- read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/data/pondo_soil_moisture.csv")
head(vwc)
vwc.agg = matrix(unique(vwc$plot),18,1)
week.list=colnames(vwc[,-c(1,2)])
for(i in 1:length(week.list)){
  w = aggregate(get(week.list[i])~plot,data=vwc,function(x)mean(x))
  vwc.agg=cbind(vwc.agg,w[,2])
}
colnames(vwc.agg)=c("plot",week.list)
vwc.agg<-as.data.frame(vwc.agg)
vwc.agg[week.list] <- sapply(vwc.agg[week.list],as.character)
vwc.agg[week.list] <- sapply(vwc.agg[week.list],as.numeric)
vwc.agg$ave_vwc = apply(vwc.agg[,2:ncol(vwc.agg)], 1, mean)
vwc.agg$ave_vwc = apply(vwc.agg[,2:ncol(vwc.agg)], 1, mean)
vwc.agg$min_vwc = apply(vwc.agg[,2:10], 1, min)
vwc.agg$max_vwc = apply(vwc.agg[,2:10], 1, max)

dev.off()
barplot(max_vwc~plot,data=vwc.agg,col="purple",xlab="plot",ylab="Volumetric Water Content (VWC)")
barplot(ave_vwc~plot,data=vwc.agg,add=T,col="lightblue")
barplot(min_vwc~plot,data=vwc.agg,add=T,col="yellow")
legend("topright",c("max","ave","min"),fill=c("purple","lightblue","yellow"),cex=1.5)

vwc.TS<- vwc
colnames(vwc.TS) <- c("plot","rep","06-02-2020","06-12-2020","06-16-2020",
                      "06-23-2020","06-30-2020","07-07-2020","07-22-2020",
                      "07-29-2020","08-06-2020")
vwc.TS<- melt(vwc.TS,id.vars=c("plot","rep"))
vwc.TS$variable<- mdy(vwc.TS$variable)
vwc.TS.sum<- as.data.frame(cbind(aggregate(value~plot+variable,vwc.TS,mean),
aggregate(value~plot+variable,vwc.TS,function(x)sd(x)/sqrt(3))$value))
colnames(vwc.TS.sum)<- c("plot","date","mean","se")

ggplot(vwc.TS.sum,aes(x=date,y=mean,col=plot))+
  geom_line()+
  geom_errorbar(mapping=aes(x=date,ymin=mean-se,ymax=mean+se),width=0.8)+
###add in lines for when i watered
  geom_vline(xintercept = mdy(c("05-22-2020","05-29-2020","06-04-2020",
                            "06-20-2020","06-25-2020","07-02-2020",
                            "07-08-2020","07-10-2020")),col="red",linetype="dotdash")+
  geom_vline(xintercept = mdy(c("05-28-2020","06-03-2020",
                              "06-17-2020","06-24-2020","07-01-2020",
                              "07-09-2020","07-10-2020")),col="blue",linetype="dotdash")
###extract prism precip estimates for both sites
import <- ogrListLayers("C:/Users/Katie/Google Drive/Ponderosa seed project/gps data/high.kml")
attr(import, "driver")
attr(import, "nlayers")
high.site = readOGR("C:/Users/Katie/Google Drive/Ponderosa seed project/gps data/high.kml", "high.kml") 

import2 <- ogrListLayers("C:/Users/Katie/Google Drive/Ponderosa seed project/gps data/mid.kml")
attr(import2, "driver")
attr(import2, "nlayers")
mid.site = readOGR("C:/Users/Katie/Google Drive/Ponderosa seed project/gps data/mid.kml", "mid.kml") 

day=formatC(seq(01,31,1),width=2,flag=0)

#june
high.precip.jun=matrix(0,length(day),1)
mid.precip.jun=matrix(0,length(day),1)
for(i in 1:length(day)){
  temp.high = raster::extract(raster(paste("C:/Users/Katie/Google Drive/Ponderosa seed project/climate data/PRISM_ppt_stable_4kmD2_20200601_20200630_bil/PRISM_ppt_stable_4kmD2_202006",day[i],"_bil.bil",sep="")),
                      high.site)
  temp.mid = raster::extract(raster(paste("C:/Users/Katie/Google Drive/Ponderosa seed project/climate data/PRISM_ppt_stable_4kmD2_20200601_20200630_bil/PRISM_ppt_stable_4kmD2_202006",day[i],"_bil.bil",sep="")),
                     mid.site)
  high.precip.jun[i,1]=temp.high
  mid.precip.jun[i,1]=temp.mid}
high.precip.jun=as.matrix(high.precip.jun[-31,],30,1)
mid.precip.jun=as.matrix(mid.precip.jun[-31,],30,1)

#july
high.precip.jul=matrix(0,length(day),1)
mid.precip.jul=matrix(0,length(day),1)
for(i in 1:length(day)){
  temp.high = raster::extract(raster(paste("C:/Users/Katie/Google Drive/Ponderosa seed project/climate data/PRISM_ppt_provisional_4kmD2_20200701_20200731_bil/PRISM_ppt_provisional_4kmD2_202007",day[i],"_bil.bil",sep="")),
                              high.site)
  temp.mid = raster::extract(raster(paste("C:/Users/Katie/Google Drive/Ponderosa seed project/climate data/PRISM_ppt_provisional_4kmD2_20200701_20200731_bil/PRISM_ppt_provisional_4kmD2_202007",day[i],"_bil.bil",sep="")),
                             mid.site)
  high.precip.jul[i,1]=temp.high
  mid.precip.jul[i,1]=temp.mid}
#august
high.precip.aug=matrix(0,length(day),1)
mid.precip.aug=matrix(0,length(day),1)
for(i in 1:length(day)){
  temp.high = raster::extract(raster(paste("C:/Users/Katie/Google Drive/Ponderosa seed project/climate data/PRISM_ppt_provisional_4kmD2_20200801_20200831_bil/PRISM_ppt_provisional_4kmD2_202008",day[i],"_bil.bil",sep="")),
                              high.site)
  temp.mid = raster::extract(raster(paste("C:/Users/Katie/Google Drive/Ponderosa seed project/climate data/PRISM_ppt_provisional_4kmD2_20200801_20200831_bil/PRISM_ppt_provisional_4kmD2_202008",day[i],"_bil.bil",sep="")),
                             mid.site)
  high.precip.aug[i,1]=temp.high
  mid.precip.aug[i,1]=temp.mid}
high.precip.aug.edit<-high.precip.aug
high.precip.aug.edit=cbind(high.precip.aug.edit, as.character(seq.Date(as.Date('2020-08-01'),as.Date('2020-08-31'),1)))
high.precip.aug.edit<-as.data.frame(high.precip.aug.edit)
high.precip.aug.edit$V1=as.numeric(as.character(high.precip.aug.edit$V1))
high.precip.aug.edit$V2=as.Date(high.precip.aug.edit$V2)
colnames(high.precip.aug.edit)=c("precip_mm","date")

##combine all months precip data
#high site
jja.precip.high<- rbind(high.precip.jun,high.precip.jul,high.precip.aug)
jja.precip.high=cbind(jja.precip.high, as.character(c(seq.Date(as.Date('2020-06-01'),as.Date('2020-06-30'),1),
                                                      seq.Date(as.Date('2020-07-01'),as.Date('2020-07-31'),1),
                                                      seq.Date(as.Date('2020-08-01'),as.Date('2020-08-31'),1))))
jja.precip.high<-as.data.frame(jja.precip.high)
jja.precip.high$V1=as.numeric(as.character(jja.precip.high$V1))
jja.precip.high$V2=as.Date(jja.precip.high$V2)
colnames(jja.precip.high)=c("precip_mm","date")
#mid site
jja.precip.mid<- rbind(mid.precip.jun,mid.precip.jul,mid.precip.aug)
jja.precip.mid=cbind(jja.precip.mid, as.character(c(seq.Date(as.Date('2020-06-01'),as.Date('2020-06-30'),1),
                                                      seq.Date(as.Date('2020-07-01'),as.Date('2020-07-31'),1),
                                                      seq.Date(as.Date('2020-08-01'),as.Date('2020-08-31'),1))))
jja.precip.mid<-as.data.frame(jja.precip.mid)
jja.precip.mid$V1=as.numeric(as.character(jja.precip.mid$V1))
jja.precip.mid$V2=as.Date(jja.precip.mid$V2)
colnames(jja.precip.mid)=c("precip_mm","date")

####plot soil moisture, precip and watering events
vwc.TS.sum.high = vwc.TS.sum[vwc.TS.sum$plot%in%c("HH1","HH2","HH3","HH4","HH5","HH6"),]
vwc.TS.sum.mid = vwc.TS.sum[!vwc.TS.sum$plot%in%c("HH1","HH2","HH3","HH4","HH5","HH6"),]

#high site
high.precip <- ggplot()+
  ###add in lines for when i watered
  geom_vline(xintercept = mdy(c("05-28-2020","06-03-2020",
                                "06-17-2020","06-24-2020","07-01-2020",
                                "07-09-2020","07-10-2020")),col="blue",alpha=0.5,linetype="dotdash")+
  geom_bar(data=jja.precip.high,aes(x=date,y=precip_mm),stat="identity")+
  geom_line(data=vwc.TS.sum.high,aes(x=date,y=mean,col=plot))+
  geom_errorbar(data=vwc.TS.sum.high,mapping=aes(x=date,ymin=mean-se,ymax=mean+se,col=plot),width=0.8)+
  scale_y_continuous(sec.axis = sec_axis(~./1, name = "Volumetric Water Content (%)"))+
  theme_bw()

#mid site
mid.precip <- ggplot()+
  ###add in lines for when i watered
  geom_vline(xintercept = mdy(c("05-22-2020","05-29-2020","06-04-2020",
                                "06-20-2020","06-25-2020","07-02-2020",
                                "07-08-2020","07-10-2020")),col="blue",alpha=0.5,linetype="dotdash")+
  geom_bar(data=jja.precip.mid,aes(x=date,y=precip_mm),stat="identity")+
  geom_line(data=vwc.TS.sum.mid,aes(x=date,y=mean,col=plot))+
  geom_errorbar(data=vwc.TS.sum.mid,mapping=aes(x=date,ymin=mean-se,ymax=mean+se,col=plot),width=0.8)+
  scale_y_continuous(sec.axis = sec_axis(~./1, name = "Volumetric Water Content (%)"))+
  theme_bw()

ggarrange(high.precip,mid.precip,nrow = 2,ncol = 1)

###are sites/plots different in water content?
vwc.TS.sum$site<- substr(vwc.TS.sum$plot,0,2)
vwc.TS.sum$site<-as.factor(vwc.TS.sum$site)
vwc.mod1<- lm(mean~site,data=vwc.TS.sum)
anova(vwc.mod1)
TukeyHSD(aov(mean~site,data=vwc.TS.sum))
vwc1.glht<- glht(vwc.mod1,linfct=mcp(site="Tukey"))
vwc1.cld<- cld(vwc1.glht)
vwc1.cld ##high site significantly different from mid site

vwc.mod2<- lm(mean~plot,data=vwc.TS.sum[vwc.TS.sum$site=="HH",])
anova(vwc.mod2)
TukeyHSD(aov(mean~plot,data=vwc.TS.sum[vwc.TS.sum$site=="HH",]))
vwc2.glht<- glht(vwc.mod2,linfct=mcp(plot="Tukey"))
vwc2.cld<- cld(vwc2.glht)
vwc2.cld ##high site significantly different from mid site

vwc.mod3<- lm(mean~plot,data=vwc.TS.sum[vwc.TS.sum$site%in%c("MH","ML"),])
anova(vwc.mod3)
TukeyHSD(aov(mean~plot,data=vwc.TS.sum[vwc.TS.sum$site%in%c("MH","ML"),]))
vwc3.glht<- glht(vwc.mod3,linfct=mcp(plot="Tukey"))
vwc3.cld<- cld(vwc3.glht)
vwc3.cld ##high site significantly different from mid site

####
plot_traits = vwc.agg[,c("plot","ave_vwc","min_vwc","max_vwc")]

temp<- read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/data/ibutton_data_2020/ibutton_2020_rdata.csv")
head(temp)

ibutton<- read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/data/ibutton_data_2020/all_ibutton_data.csv")
head(ibutton)

plot.list=c("HH1","HH2","HH3","HH4","HH5","HH6","MH1","MH2","MH3","MH4","MH5","MH6","ML1","ML2","ML3","ML4","ML5","ML6")
date.list=unique(ibutton$Date.Time.HH1)
temp.mat=matrix(date.list,42,1)

for(i in 1:length(plot.list)){
  tmin = aggregate(get(plot.list[i])~get(paste("Date.Time.",plot.list[i],sep="")),data=ibutton,function(x)min(x))
  tmax = aggregate(get(plot.list[i])~get(paste("Date.Time.",plot.list[i],sep="")),data=ibutton,function(x)max(x))
  tmin = tmin[order(as.Date(tmin[,1], format="%m/%d/%Y")),]
  tmax = tmax[order(as.Date(tmax[,1], format="%m/%d/%Y")),]
  temp.mat<- cbind(temp.mat,tmin[,2],tmax[,2])
}
head(temp.mat)
temp.mat<-as.data.frame(temp.mat)
names.list=c()
for(i in plot.list){
  cnames<- c(paste(i,".min",sep=""),paste(i,".max",sep = ""))
  names.list<- c(names.list,cnames)
}
colnames(temp.mat)=c("date",names.list)
temp.mat[names.list] <- sapply(temp.mat[names.list],as.character)
temp.mat[names.list] <- sapply(temp.mat[names.list],as.numeric)
temp.mat["date"] <- sapply(temp.mat["date"],as.character)
str(temp.mat)
head(temp.mat)
###delete first and last days of temp recording as they are not complete days 
temp.mat <- temp.mat[-c(1,nrow(temp.mat)),]
###summarize mean min and mean max for each plot
temp.sum<- apply(temp.mat[,-1],2,mean)
temp.sum<-matrix(temp.sum,nrow=18,ncol=2,by=2)
colnames(temp.sum)=c("mean.min","mean.max")
temp.sum=cbind(plot.list,temp.sum)
temp.sum<- as.data.frame(temp.sum)
temp.sum[c("mean.min","mean.max")] <- sapply(temp.sum[c("mean.min","mean.max")],as.character)
temp.sum[c("mean.min","mean.max")] <- sapply(temp.sum[c("mean.min","mean.max")],as.numeric)

ibutton.edit<- ibutton
colnames(ibutton.edit)=rep(c("Date.Time","temp"),18)
head(ibutton.edit)

ibutton.long<- rbind(ibutton.edit[,1:2],ibutton.edit[,3:4],ibutton.edit[,5:6],ibutton.edit[,7:8],ibutton.edit[,9:10],ibutton.edit[,11:12],
                     ibutton.edit[,13:14],ibutton.edit[,15:16],ibutton.edit[,17:18],ibutton.edit[,19:20],ibutton.edit[,21:22],ibutton.edit[,23:24],
                     ibutton.edit[,25:26],ibutton.edit[,27:28],ibutton.edit[,29:30],ibutton.edit[,31:32],ibutton.edit[,33:34],ibutton.edit[,35:36])
head(ibutton.long)
plot.list.long=vector()
for(i in 1:length(plot.list)){
  p <- rep(plot.list[i],nrow(ibutton))
  plot.list.long = c(plot.list.long,p)
}
ibutton.long$site=plot.list.long
##delete 5/28 and 7/8
ibutton.long = ibutton.long[! ibutton.long$Date.Time %in% c("5/28/2020","7/8/2020"),]
ibutton.mean = aggregate(temp~site, data=ibutton.long, mean)
ibutton.sd = aggregate(temp~site, data=ibutton.long, sd)
ibutton.mean$se = ibutton.sd[,2]/sqrt(nrow(ibutton.long))
ibutton.mean$variable = rep("mean",nrow(ibutton.mean))
ibutton.mean=ibutton.mean[,c("site","variable","temp","se")]
colnames(ibutton.mean)=c("plot.list","variable","value","se")

temp.sd<- apply(temp.mat[,-1],2,sd)
temp.se<- temp.sd/sqrt(nrow(temp.mat))
temp.se<- matrix(temp.se,nrow=18,ncol=2,by=2)

temp.sum.melt<- melt(temp.sum)
temp.edit<- temp[,1:2]
colnames(temp.edit)=c("plot","value")
temp.edit$variable=rep("mean",nrow(temp.edit))

temp.sum.melt$se<- c(temp.se[,1],temp.se[,2])
temp.sum.melt = rbind(temp.sum.melt,ibutton.mean)

ggplot(temp.sum.melt,aes(x=plot.list,y=value,col=variable))+
  geom_point()+
  scale_color_manual(values=c("blue","red","black"))+
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2)+
  ylab("Temperature (C)")+
  xlab("Plot")+
  theme(axis.text = element_text(color="black",size=20),
        text = element_text(size=20),legend.text = element_text(size=20))

#convert dates to date format with "lubridate" package
ibutton.long$Date.Time<- mdy(ibutton.long$Date.Time)
str(ibutton.long)
ibutton.long.sum<- aggregate(temp~Date.Time+site,ibutton.long, mean)
ibutton.long.sum.min<- aggregate(temp~Date.Time+site,ibutton.long, min)
ibutton.long.sum.max<- aggregate(temp~Date.Time+site,ibutton.long, max)
ibutton.long.sum$site.gen<- substr(ibutton.long.sum$site,0,2)
ibutton.long.sum.min$site.gen<- substr(ibutton.long.sum.min$site,0,2)
ibutton.long.sum.max$site.gen<- substr(ibutton.long.sum.max$site,0,2)


ggplot(ibutton.long.sum, aes(x=Date.Time, y=temp, col=site))+
  geom_line()+
  geom_line(data=ibutton.long.sum.min)+
  geom_line(data=ibutton.long.sum.max)+
  scale_color_manual(values=c(rep("blue",6),rep("red",6),rep("orange",6)))+
ggtitle("max, mean & min daily temperature (C)")+
  xlab("Date")+
  scale_x_date(date_breaks="5 day")+
  theme(axis.text.x=element_text(angle=45, hjust=1,size=16),axis.text = element_text(size=16),
        axis.title = element_text(size=16),legend.text = element_text(size=16)) 

##are temperatures significantly different between sites?
ibutton.long$region<- substr(ibutton.long$site ,0,2)
ibutton.long$region<- as.factor(ibutton.long$region)
ibutton.long$site<- as.factor(ibutton.long$site)
temp.mod<- lm(temp~site,data=ibutton.long)
temp.aov<- aov(temp~site, data=ibutton.long)
summary(temp.aov)
TukeyHSD(temp.aov)
temp.glht<- glht(temp.mod, linfct = mcp(site = "Tukey"))
#confint(temp.glht)
#temp.cld<- cld(temp.glht) #this takes a while 
temp.cld
###look at differences in average temp of 3 "regions"
temp.mod2<- lm(temp~region,data=ibutton.long)
anova(temp.mod2)
TukeyHSD(aov(temp~region,data=ibutton.long))
temp2.glht<- glht(temp.mod2,linfct=mcp(region="Tukey"))
temp2.cld<- cld(temp2.glht)
temp2.cld ##sites are all significantly different from each other in temperature 
par(mfrow=c(2,2))
plot(temp.mod2)
###look at plots within "regions"
#HH
temp.mod3<- lm(temp~site,data=ibutton.long[ibutton.long$region=="HH",])
anova(temp.mod3)
TukeyHSD(aov(temp~site,data=ibutton.long[ibutton.long$region=="HH",]))
temp3.glht<- glht(temp.mod3,linfct=mcp(site="Tukey"))
temp3.cld<- cld(temp3.glht)
temp3.cld
aggregate(temp~site,data=ibutton.long[ibutton.long$region=="HH",],mean)
aggregate(temp~site,data=ibutton.long[ibutton.long$region=="HH",],sd)
#MH
temp.mod4<- lm(temp~site,data=ibutton.long[ibutton.long$region=="MH",])
anova(temp.mod4)
TukeyHSD(aov(temp~site,data=ibutton.long[ibutton.long$region=="MH",]))
temp4.glht<- glht(temp.mod4,linfct=mcp(site="Tukey"))
temp4.cld<- cld(temp4.glht)
temp4.cld
aggregate(temp~site,data=ibutton.long[ibutton.long$region=="MH",],mean)
#ML
temp.mod5<- lm(temp~site,data=ibutton.long[ibutton.long$region=="ML",])
anova(temp.mod5)
TukeyHSD(aov(temp~site,data=ibutton.long[ibutton.long$region=="ML",]))
temp5.glht<- glht(temp.mod5,linfct=mcp(site="Tukey"))
temp5.cld<- cld(temp5.glht)
temp5.cld
aggregate(temp~site,data=ibutton.long[ibutton.long$region=="ML",],mean)

###combine temp and vwc data into plot traits
plot_traits<-merge(plot_traits,temp.sum,by.x="plot",by.y="plot.list")
##add plot traits to compiled data
rec.data = merge(rec.data,plot_traits,by="plot")
##
##########################################
##read in mother tree traits
##missing data for LHT116 because cones opened before # seeds could be counted 
cone_traits <- read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/data/mother_tree_traits.csv")
head(cone_traits)
cone_traits$pop=substr(cone_traits$tree,0,3)
head(cone_traits)
##add cone traits to compiled data
rec.data = merge(rec.data,cone_traits,by.x="moms",by.y="tree",all.y=FALSE)
rec.data<- rec.data[,-ncol(rec.data)]
colnames(rec.data)[3]<-"pop"
##

pairs(cone_traits[,-c(1,6)])
cor.test(cone_traits$mg_per_seed,cone_traits$seed_cnt_per_cone)
head(rec.data)

pop_traits = unique(rec.data[,c("pop","cwd","aet","slope","elev","foldedaspect","heatload")])

##analyze population traits with PCA
pairs(pop_traits[,-1])
trait.cor = as.data.frame(cor(pop_traits[,-1]))
trait.cor
abs(trait.cor)>0.8

pop_traits_uncor<-pop_traits[,!colnames(pop_traits)=="elev"]

poptrait_pca = prcomp(pop_traits_uncor[,-1],scale.=T)
summary(poptrait_pca)
poptrait_pca$rotation #print eigenvectors, or varialble loadings

par(mfrow=c(1,1))
plot(poptrait_pca, type="l") #scree plot
biplot(poptrait_pca,cex=0.5)#biplot
autoplot(poptrait_pca, data=pop_traits_uncor,colour="pop",
         loadings=T,loadings.label=T)

#combine rec data with cone data
seedweights<- read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/data/field_seed_weights.csv")
rec.data = merge(rec.data,seedweights[,-c(1:2,4)],by="moms")
head(rec.data)

ggplot(rec.data, aes(x=mean.max,y=yr1rec))+
  geom_point()+
  geom_smooth(method="lm",alpha=0.3)

ggplot(rec.data, aes(x=ave_vwc,y=yr1rec,col=pop))+
  geom_point()+
  geom_smooth(method="lm",alpha=0.3)

#############################################
###add other population climate data from prism extract
pop_climate <- read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/climate data/pondosites_climate.csv")
pop_climate$site<- dplyr::recode(pop_climate$site,FC="FCT",BH="BHT",LH="LHT",BD="BDT")
pop_tmin<-aggregate(tmin~site, pop_climate,min)
pop_tmax<-aggregate(tmax~site, pop_climate,max)
pop_tmean<-aggregate(tmean~site,pop_climate,mean)
pop_ppt<-aggregate(ppt~site, pop_climate,function(x)sum(x))

rec.data<-merge(rec.data,pop_tmin, by.x="pop",by.y="site")
rec.data<-merge(rec.data,pop_tmax, by.x="pop",by.y="site")
rec.data<-merge(rec.data,pop_tmean, by.x="pop",by.y="site")
rec.data<-merge(rec.data,pop_ppt, by.x="pop",by.y="site")

############################################
##MODELS####################################
############################################

## survival model of just categorical variables
###make new data frame with just MH and HH results as models are hard to converge with ML included
rec.data.noml<-rec.data[rec.data$site%in%c("HH","MH"),] 
rec.data.noml$pop<- factor(rec.data.noml$pop)
rec.data.noml$site<- factor(rec.data.noml$site)

##set up deviation coding (see https://stats.idre.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/#SIMPLE)
contrasts(rec.data.noml$pop)<-contr.sum(8)
contrasts(rec.data.noml$site)<-contr.sum(2)

catmod1 = glmer(cbind(seedling.cnt.11.19,
                 num.died)~pop*site+
                  (1|moms)+(1|plot),data=rec.data.noml,family=binomial)
summary(catmod1)
drop1(catmod1,test="Chisq")
Anova(catmod1)
anova(catmod1)
pairs(emmeans(catmod1, "pop",by="site"))

catmod1.Dmoms<- glmer(cbind(seedling.cnt.11.19,
                            num.died)~pop*site+
                        (1|plot),data=rec.data.noml,family=binomial)
moms.param<- -2*logLik(catmod1.Dmoms)+2*logLik(catmod1)
pchisq(as.numeric(moms.param),df=1,lower.tail = F)
pbnm.moms <- pbnm(catmod1.Dmoms,catmod1,nsim=1000,tasks=10,cores=2,seed=31221743)
summary(pbnmx1)

catmodsimp1<- glm(cbind(seedling.cnt.11.19,
                          num.died)~pop
                      ,data=rec.data.noml,family=binomial)
summary(catmodsimp1)
summary(anova(catmodsimp1))

field.data.long.surv<-field.data.long[field.data.long$germ==1,]
test<- glm(surv~pop,data=field.data.long.surv,family=binomial)
summary(test)
summary(glm(cbind(seedling.cnt.11.19,
                                num.died)~pop
                          ,data=rec.data,family=binomial))

catmodsimp2<- glm(cbind(seedling.cnt.11.19,
                        num.died)~moms
                  ,data=rec.data.noml,family=binomial)
summary(catmodsimp2)

catmodsimp3<- glmer(cbind(seedling.cnt.11.19,
                        num.died)~pop+(1|moms)
                  ,data=rec.data.noml,family=binomial)
summary(catmodsimp3)

PseudoR2(catmodsimp1,"McKelveyZavoina")
PseudoR2(catmodsimp2,"McKelveyZavoina")

plot.data<- aggregate(yr1surv~moms+pop,rec.data,mean)
ggplot(plot.data,aes(x=pop,y=yr1surv))+
  geom_point()

### model diagnostics
plot(catmod1,id=0.05,idLabels=~.obs)
plot(catmod1,ylim=c(-2.5,2.5),type=c("p","smooth"))
#random effects
dotplot(ranef(catmod1,condVar=TRUE,whichel="moms"),colorBy = "p.adjust", font.size=16)
dotplot(ranef(catmod1,condVar=TRUE,whichel="plot"))

randoms<-ranef(catmod1, condVar = TRUE)
qq <- attr(ranef(catmod1, condVar = TRUE)[[1]], "postVar")
rand.interc<-randoms$moms
df<-data.frame(Intercepts=randoms$moms[,1],
               sd.interc=2*sqrt(qq[,,1:length(qq)]),
               lev.names=rownames(rand.interc))
df$lev.names<-factor(df$lev.names,levels=df$lev.names[order(df$Intercepts)])

ggplot(df,aes(Intercepts,lev.names,shape=Intercepts>0))+
  guides(size=FALSE,shape=FALSE)+
  geom_vline(xintercept=0)+
  geom_errorbar(aes(xmin=Intercepts-sd.interc, xmax=Intercepts+sd.interc), width=0,color="black")+
  geom_point(aes(size=2))+
  scale_shape_manual(values=c(1,16))+
  theme_bw()+
  theme(axis.text.x=element_text(size=rel(1.2)),
        axis.title.x=element_text(size=rel(1.3)),
        axis.text.y=element_text(size=rel(1.2)),
        panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank())


qq_plot <- attr(ranef(catmod1, condVar = TRUE)[[2]], "postVar")
rand.interc.plot<-randoms$plot
df.plot<-data.frame(Intercepts=randoms$plot[,1],
               sd.interc=2*sqrt(qq_plot[,,1:length(qq_plot)]),
               lev.names=rownames(rand.interc.plot))
df.plot$lev.names<-factor(df.plot$lev.names,levels=df.plot$lev.names[order(df.plot$Intercepts)])

ggplot(df.plot,aes(Intercepts,lev.names,shape=Intercepts>0))+
  guides(size=FALSE,shape=FALSE)+
  geom_vline(xintercept=0)+
  geom_errorbar(aes(xmin=Intercepts-sd.interc, xmax=Intercepts+sd.interc), width=0,color="black")+
  geom_point(aes(size=2))+
  scale_shape_manual(values=c(1,16))+
  theme_bw()+
  theme(axis.text.x=element_text(size=rel(1.2)),
        axis.title.x=element_text(size=rel(1.3)),
        axis.text.y=element_text(size=rel(1.2)),
        panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank())

#model coefficient confidence intervals 
catmod1.CIs<-as.data.frame(confint(catmod1,method="Wald"))##should probably use another method (e.g. "boot"), but they take a long time (>1hr)
catmod1.CIs$variable<-rownames(catmod1.CIs)
colnames(catmod1.CIs)=c("lower","upper","variable")
catmod1.CIs<-catmod1.CIs[-c(1:2),]
catmod1.CIs$variable.name<-c("intercept",paste(unique(rec.data$pop)[-8]),"site_HH",paste(unique(rec.data$pop)[-1],":site_HH",sep=""))
ggplot(catmod1.CIs,aes(x=variable.name,y=lower))+
  geom_point()+
  geom_point(aes(y=upper))+
  geom_segment(aes(x=variable.name, y=lower, xend=variable.name, yend=upper),data=catmod1.CIs)+
  geom_hline(yintercept = 0,col="green")+
  ylab("Coefficient 95% CI")+
  theme(axis.text.x = element_text(angle=45,hjust=1))

par(mfrow=c(1,2))
hist(residuals(catmod1, "pearson"))
plot(fitted(catmod1),residuals(catmod1))

###just look at families that were not messed up at lower site
rec.data.6pop<- rec.data.noml[! rec.data.noml$pop %in% c("HM1","WT1"), ]
rec.data.6pop$moms<-factor(rec.data.6pop$moms)
contrasts(rec.data.6pop$moms)<- contr.sum(29)
contrasts(rec.data.6pop$site)<-contr.sum(2)

catmod2 = glmer(cbind(seedling.cnt.11.19,
                      num.died)~moms*site+
                  (1|plot),data=rec.data.6pop,family=binomial)
summary(catmod2)
Anova(catmod2)
catmod2.CIs<-as.data.frame(confint(catmod2,method="Wald"))##should probably use another method (e.g. "boot"), but they take a long time
catmod2.CIs$variable<-rownames(catmod2.CIs)
colnames(catmod2.CIs)=c("lower","upper","variable")
catmod2.CIs<-catmod2.CIs[-1,]
catmod2.CIs$variable.name<-c("intercept",paste(unique(rec.data.6pop$moms)[-29]),"site_HH",paste(unique(rec.data.6pop$moms)[-29],":site_HH",sep=""))
ggplot(catmod2.CIs,aes(x=variable.name,y=lower))+
  geom_point()+
  geom_point(aes(y=upper))+
  geom_segment(aes(x=variable.name, y=lower, xend=variable.name, yend=upper),data=catmod2.CIs)+
  geom_hline(yintercept = 0,col="green")+
  ylab("Coefficient 95% CI")+
  theme(axis.text.x = element_text(angle=45,hjust=1,size=14))

#model diagnostics
plot(catmod2,id=0.05,idLabels=~.obs)
plot(catmod2,ylim=c(-2.5,2.5),type=c("p","smooth"))
par(mfrow=c(1,2))
hist(residuals(catmod2, "pearson"))
plot(fitted(catmod2),residuals(catmod2))
##random effects
dotplot(ranef(catmod2,condVar=TRUE,whichel="plot"),colorBy = "p.adjust", font.size=16)

###model survival as a function of plot temp and moisture
plotmod1<- glmer(cbind(seedling.cnt.11.19,
                       num.died)~pop*scale(ave_vwc)+pop*scale(mean.min)+pop*scale(mean.max)+
                   (1|moms)+(1|site),data=rec.data.noml,family=binomial)
summary(plotmod1)$AIC
visreg(plotmod1,"ave_vwc",by="pop",scale="response")

### model survival as a function of seed source climate and site
str(rec.data.noml)
contrasts(rec.data.noml$pop)<-contr.treatment(8)
contrasts(rec.data.noml$site)<-contr.treatment(2)

pop.mod1<-glmer(cbind(seedling.cnt.11.19,num.died)~
                  site+scale(cwd)+scale(aet)+scale(heatload)+scale(dist_m)+(1|moms)+(1|plot),
                data=rec.data.noml,family=binomial)
summary(pop.mod1)

pop.mod2 <- glmer(cbind(seedling.cnt.11.19,num.died)~
                    site*scale(cwd)+site*scale(aet)+site*scale(heatload)+site*scale(dist_m)+(1|moms)+(1|plot),
                  data=rec.data.noml,family=binomial)
summary(pop.mod2)
summary(pop.mod2)$AIC
summary(catmod1)$AIC
o<-visreg(pop.mod2,"aet",scale="response",by="site",gg=T,ylab="Survival")
t<-visreg(pop.mod2,"heatload",scale="response",by="site",gg=T,ylab="Survival")
tr<- visreg(pop.mod2, "dist_m",scale="response",by="site",gg=T,ylab="Survival")
f <- visreg(pop.mod2, "site",scale="response",gg=T,ylab="Survival")
ggarrange(o,t,tr,f)

options(na.action="na.fail")
#popmod2dredge <- dredge(pop.mod2) #this takes a minute or two 
popmod2dredge[popmod2dredge$delta<2,]

pop.mod3 <- glmer(cbind(seedling.cnt.11.19,num.died)~
                    scale(ave_vwc)*scale(cwd)+scale(ave_vwc)*scale(aet)+scale(ave_vwc)*scale(heatload)+scale(ave_vwc)*scale(dist_m)+
                    scale(mean.min)*scale(cwd)+scale(mean.min)*scale(aet)+scale(mean.min)*scale(heatload)+scale(mean.min)*scale(dist_m)+
                    scale(mean.max)*scale(cwd)+scale(mean.max)*scale(aet)+scale(mean.max)*scale(heatload)+scale(mean.max)*scale(dist_m)+
                    (1|moms)+(1|site),
                  data=rec.data.noml,family=binomial)
summary(pop.mod3)
visreg(pop.mod3,"aet",scale="response", by='ave_vwc')
visreg(pop.mod3,"heatload",scale="response", by='ave_vwc')
visreg(pop.mod3,"dist_m",scale="response", by='ave_vwc')
visreg(pop.mod3,"dist_m",scale="response", by='mean.min')

pop.mod4<- glmer(cbind(seedling.cnt.11.19,num.died)~
                   site*scale(aet)+site*scale(heatload)+site*scale(dist_m)+
                   site*scale(tmin)+site*scale(ppt)+(1|moms)+(1|plot),
                 data=rec.data.noml,family=binomial)
summary(pop.mod4)

### model diagnostics
plot(pop.mod2,id=0.05,idLabels=~.obs)
plot(pop.mod2,ylim=c(-2.5,2.5),type=c("p","smooth"))
#random effects
dotplot(ranef(pop.mod2,condVar=TRUE,whichel="moms"))
dotplot(ranef(pop.mod2,condVar=TRUE,whichel="plot"))

confint(pop.mod2,method="Wald")##should probably use another method (e.g. "boot"), but they take a long time

par(mfrow=c(1,2))
hist(residuals(pop.mod2, "pearson"))
plot(fitted(pop.mod2),residuals(pop.mod2))

##mean min and ave vwc are marginally correlated, but not extreme nor significant at p<0.05
cor.test(temp.sum$mean.min, vwc.agg$ave_vwc)
plot(temp.sum$mean.min, vwc.agg$ave_vwc)

##model mom traits 
mom.mod1<- glmer(cbind(seedling.cnt.11.19,num.died)~
                   scale(ave_vwc)*scale(cwd)+scale(ave_vwc)*scale(aet)+scale(ave_vwc)*scale(heatload)+scale(ave_vwc)*scale(dist_m)+
                   scale(mean.min)*scale(cwd)+scale(mean.min)*scale(aet)+scale(mean.min)*scale(heatload)+scale(mean.min)*scale(dist_m)+
                   scale(mean.max)*scale(cwd)+scale(mean.max)*scale(aet)+scale(mean.max)*scale(heatload)+scale(mean.max)*scale(dist_m)+
                   scale(mean.max)*scale(g.seed)+scale(mean.min)*scale(g.seed)+scale(ave_vwc)*scale(g.seed)+
                   (1|site),
                 data=rec.data.noml,family=binomial)
summary(mom.mod1)
summary(mom.mod1)$AIC
aet.int<- visreg(mom.mod1,"aet",scale="response", by='ave_vwc',gg=T,ylab="Survival")
dist.int<- visreg(mom.mod1,"dist_m",scale="response", by='mean.min',gg=T,ylab="Survival")
vwc.site<- visreg(mom.mod1,"ave_vwc",scale="response",by="site",gg=T,ylab="Survival")
aet.site<- visreg(mom.mod1,"aet",scale="response",by="site",gg=T,ylab="Survival")
hl.site<- visreg(mom.mod1,"heatload",scale="response",by="site",gg=T,ylab="Survival")
dist.site<- visreg(mom.mod1,"dist_m",scale="response",by="site",gg=T,ylab="Survival")
g.site<- visreg(mom.mod1,"g.seed",scale="response",by="site",gg=T,ylab="Survival")
dotplot(ranef(mom.mod1,condVar=TRUE))
ggarrange(aet.int,dist.int)
ggarrange(vwc.site,aet.site,hl.site,dist.site,g.site)

###look at variation in seed/cone traits
seedweights$Site = dplyr::recode(seedweights$Site, BD="BDT",BH="BHT",FC="FCT",LH="LHT")
seed_traits<- merge(seedweights, pop_traits,by.x="Site",by.y="pop")

cone_data_reps<-read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/data/cone_data_with_reps_2019.csv")

seedmod1<-lm(g.seed~scale(cwd)+scale(aet)+scale(heatload),data=seed_traits)
summary(seedmod1)#effect of heatload and cwd on seed weight
par(mfrow=c(2,2))
plot(seedmod1)
visreg(seedmod1,"cwd")
visreg(seedmod1,"heatload")
seedmod1.2<-lm(seed_weight_g~site,data=cone_data_reps[!cone_data_reps$moms=="LHT116",])
summary(seedmod1.2)
TukeyHSD(aov(seed_weight_g~site,data=cone_data_reps[!cone_data_reps$moms=="LHT116",]))
visreg(seedmod1.2,"site")
summary(aov(g.seed~Site,data=seed_traits))
TukeyHSD(aov(g.seed~Site,data=seed_traits))

cone_traits<- merge(cone_traits,seed_traits[,c("moms","g.seed")],by.x="tree",by.y="moms")
pairs(cone_traits[,c("cone_height_mm","cone_dry_g","seed_cnt_per_cone","g.seed")])
trait.cor = as.data.frame(cor(cone_traits[,c("cone_height_mm","cone_dry_g","seed_cnt_per_cone","g.seed")]))
seed_traits<-merge(seed_traits,cone_traits[,c("tree","cone_dry_g")],by.x="moms",by.y="tree")
seed_traits<-merge(seed_traits,cone_traits[,c("tree","seed_cnt_per_cone")],by.x="moms",by.y="tree")

seedmod2<-lm(cone_dry_g~scale(cwd)+scale(aet)+scale(heatload),data=seed_traits)
summary(seedmod2) #no effect of pop climate on cone weight 
summary(aov(cone_dry_g~Site,data=seed_traits))
TukeyHSD(aov(cone_dry_g~Site,data=seed_traits))
par(mfrow=c(1,1))
boxplot(cone_dry_g~Site,data=seed_traits)
#BD has lighter cones than the two heaviest pops (HM6 and LH)

seedmod3<- lm(seed_cnt_per_cone~scale(cwd)+scale(aet)+scale(heatload),data=seed_traits[!seed_traits$moms=="LHT116",])
summary(seedmod3) #no effect of pop climate on seed count 
cntmod1<- lm(seed_cnt_per_cone~Site,data=seed_traits)
summary(aov(seed_cnt_per_cone~Site,data=seed_traits))
TukeyHSD(aov(seed_cnt_per_cone~Site,data=seed_traits))
boxplot(seed_cnt_per_cone~Site,data=seed_traits)
#BH has more seeds per cone than most other pops

###############old stuff #####################
##these are moms that had significant interaction with site = MH
subset1<- rec.data[which(rec.data$moms%in%c("LHT123M","LHT188","WT1T9")),]

##plots show that these moms did not have a significant difference in survival between sites HH and MH
ggplot(subset1, aes(x=moms,y=yr1surv,fill=site))+
  geom_boxplot()
ggplot(rec.data, aes(x=moms,y=yr1surv,fill=site))+
  geom_boxplot()

subset.hh = rec.data[which(rec.data$site=="HH"),c("plot","pop","moms","yr1surv")]
subset.hh= subset.hh[
  with(subset.hh, order(pop, moms)),
  ]
subset.mh = rec.data[which(rec.data$site=="MH"),c("plot","pop","moms","yr1surv")]
subset.mh = subset.mh[
  with(subset.mh, order(pop, moms)),
  ]
unique(subset.hh[,2:3] == subset.mh[,2:3])

subset.both<- rbind(aggregate(yr1surv~moms,data=subset.hh,mean),
aggregate(yr1surv~moms,data=subset.mh,mean))
subset.both$site = c(rep(1,39),rep(2,39))
subset.both$color = rep("B",nrow(subset.both))
subset.both[subset.both$moms %in% c("LHT123M","LHT188","WT1T9"),]$color<- "Y"

ggplot(subset.both,aes(x=site, y=yr1surv, col=moms))+
  geom_line()+
  scale_color_manual(values=c(rep("gray",26),"purple","green",rep("gray",6),"blue",rep("gray",4)))

subset.pop<- rbind(aggregate(yr1surv~pop,data=subset.hh,mean),
                    aggregate(yr1surv~pop,data=subset.mh,mean))
subset.pop$site = c(rep(1,8),rep(2,8))
ggplot(subset.pop,aes(x=site, y=yr1surv, col=pop))+
  geom_line()

###################Making plots like Alison's########################
##average soil moisture vs. survival probability trendline data
visreg(ppmodel1,"ave_vwc",scale="response")
ave_vwc_df <- visreg(ppmodel1,"ave_vwc",scale="response", plot = FALSE)
#visreg cannot calculate CIs for mixed models. see - https://rdrr.io/cran/visreg/man/visreg-faq.html
#from a stats perspective it seems like you can predict a CI from a mixed model, it just seems like visreg can't do it for mixed-models
ave_vwc_df$fit$ave_vwc #x values of trend line
ave_vwc_df$fit$visregFit # y values of trend line
ave_vwc_df$fit$visregLwr
ave_vwc_df$fit$visregUpr
ave_vwc_df <- as.data.frame(cbind(ave_vwc_df$fit$ave_vwc, ave_vwc_df$fit$visregFit, ave_vwc_df$fit$visregLwr, ave_vwc_df$fit$visregUpr))
ave_vwc_df

#creating bins for histograms
#finding breaks along x axis for each bin
vwc.break <- hist(as.numeric(rec.data$ave_vwc), breaks=seq(3,15,by=1), plot = FALSE)$br #remember - need to look at range of data to know what makes sense
vwc.break
#finding mid points of each bin (so that the histogram plots in the middle of the bin)
vwc.x <- hist(as.numeric(rec.data$ave_vwc), breaks=vwc.break, plot = FALSE)$mid
vwc.x
rec.data$tempbins <- cut(as.numeric(rec.data$ave_vwc), breaks = as.numeric(vwc.break), labels = as.numeric(vwc.x)) #this adds a column of moisture bins and assigns each row to a bin
temp_rec <- as.data.table(rec.data)
surv.vwc <- temp_rec[,.(surv = sum(seedling.cnt.11.19)), by=.(tempbins)] #consolidating # surv germs from each quad by bin 
surv.vwc
surv.vwc$surv <-as.numeric(as.character(surv.vwc$surv))#prepping for reordering
surv.vwc[order(surv.vwc$tempbins),] #reordering (temperature bins got out of order in previous step)
died.vwc <- temp_rec[,.(dead = sum(num.died)), by = .(tempbins)] #consolidating dead germs from each quad by bin 
died.vwc
died.vwc$dead <-as.numeric(as.character(died.vwc$dead)) #prepping for reordering
died.vwc[order(died.vwc$tempbins),]#reordering (temperature bins got out of order in previous step)

##scale the y values for the histogram bars

died.vwc$dead <-  died.vwc$dead/(sum(died.vwc$dead,surv.vwc$surv)) #total 29 dead vwc
died.vwc$dead <-as.numeric(as.character(died.vwc$dead)) #prepping for reordering
died.vwc <- died.vwc[order(died.vwc$tempbins),]#reordering (temperature bins got out of order in previous step)
died.vwc$tempbins <-as.numeric(as.character(died.vwc$tempbins))#NEED to do this - otherwise it will not plot on the same x values as the trend line 
died.vwc

surv.vwc$surv <- surv.vwc$surv/(sum(died.vwc$dead,surv.vwc$surv)) #total 140 surviving vwc
surv.vwc$surv <-as.numeric(as.character(surv.vwc$surv))#prepping for reordering
surv.vwc <- surv.vwc[order(surv.vwc$tempbins),] #reordering (temperature bins got out of order in previous step)
surv.vwc$tempbins <-as.numeric(as.character(surv.vwc$tempbins))#NEED to do this - otherwise it will not plot on the same x values as the trend line 
surv.vwc


#Notes for below: #side 1= below; side 2= left; side 3=above; side 4= right
#las -- labels are parallel ( =0) or perpendiduclar (=2) to axis 
#mgp -- don't mess with this - axis label locations relative to the edge of the innner plot window. (location of label, tick mark labels, tick mark defaults)

###########REDMOND ET AL. 2015 GLOBAL CHANGE BIO FIG 7 VERSION 
#drawing figure boundaries and drawing model line with CI

###PLOTTING CHART AREA AND MODEL TREND LINE
par(oma = c(2, 2, 2,2) + 0.1, mar= c(3,2,2,1)+ 0.1, mgp=c(3,1,0)) #mar = margins c(bottom, left, top,right); oma = outer margin area

plot(ave_vwc_df$V1, ave_vwc_df$V2, lwd =2, xlab = expression(paste("Average Soil Moisture (VWC)")), 
     type = "l", cex=0.9, cex.lab = 0.9, tck = -0.03, yaxt = "n", ylim=c(-0.3, 1.3), 
     xaxt = "n", xlim=c(3,15), col = "blue") #model trend line
box(lwd= 2) #for entire border  
polygon(c(ave_vwc_df$V1, rev(ave_vwc_df$V1)), c(ave_vwc_df$V3, rev(ave_vwc_df$V4)), col = "grey83", border = FALSE) #CI shaded instead of lines
#ISSUES WITH CI? see notes beginning on line 75 above
lines(ave_vwc_df$V1, ave_vwc_df$V2, lwd = 3, col = "black") #CI covers line plotted above 
#lines(avg_max_psme_df$V1, avg_max_psme_df$V3, lty =2, lwd =2) #lower CI
#lines(avg_max_psme_df$V1, avg_max_psme_df$V4, lty =2, lwd =2) #upper CI
axis(side = 2, at=c(0,0.2, 0.4, 0.6, 0.8, 1), tck =-0.03, labels =TRUE, cex.axis=1.3, lwd = 2, las=2) # y axis labels and tick marks; cex.axis specifies size of tick label; 
axis(side = 1, tck = -0.03, labels = TRUE, cex.axis=1.3, lwd = 2) #x axis labels and tick marks -- if you don't want numbers/ labels, make labels = FALSE
mtext(text= "Probability of germinant survival", side =2, line = 3, cex = 1.3, font =2)
mtext(text = expression(paste("Average Soil Moisture (VWC)")), font =2, side = 1.3, line =3, cex = 1.3)
#mtext(text = "Frequency", side =4, cex = 1.3, line = 2, font =2) #not using since currently pasting in label anyway
#mtext(text="(c)", side=3, font =2)
##ADDING SECOND Y AXIS FOR HISTOGRAMS
axis(side =4, at = 1.3, labels = "0", las =2, cex.axis = 1.3, cex = 1.6)
axis(side =4, at = -0.3, labels = "0", las =2, cex.axis = 1.3, cex = 1.4)
axis(side =4, at = -0.15, labels = "25", las =2, cex.axis = 1.3, cex = 1.5)
axis(side =4, at = 1.15, labels = "25", las =2, cex.axis = 1.3, cex = 1.3)
axis(side =4, at = 0, labels = "50", las =2, cex.axis = 1.3, cex = 1.3)
axis(side =4, at = 1, labels = "50", las =2, cex.axis = 1.3, cex = 1.3)
###ADDING HISTOGRAMS
## histograms start at 1.4 and go down or -0.6 and go up (adjusting by 0.4)
#top histogram -- SURVIVED
for (i in 1:length(surv.vwc$surv)){
  if (surv.vwc$surv[i] > 0) polygon(c(rep(surv.vwc$tempbins[i]-.5, 2), rep(surv.vwc$tempbins[i]+.5, 2)), c((1-surv.vwc$surv[i])+0.3,1.3,1.3, (1-surv.vwc$surv[i])+0.3), col = "white", lwd = 2)
} ### need to subtract from one AND add the amount above one that the points start from 
#bottom histogram -- DIED
for (i in 1:length(died.vwc$dead)){
  if (died.vwc$dead[i] > 0) polygon(c(rep(died.vwc$tempbins[i]-.5, 2), rep(died.vwc$tempbins[i]+.5, 2)), c(died.vwc$dead[i]-0.3, -0.3,-0.3, died.vwc$dead[i]-0.3), col = "grey25", lwd= 2)
}



simp.mod<- glm(cbind(seedling.cnt.11.19,
                     num.died)~mean.min,data=rec.data,family="binomial")
summary(simp.mod)
range(rec.data$mean.min)
xmin=seq(4,9,0.01)
ymin = predict(simp.mod,list(mean.min=xmin),type="response")
par(mfrow=c(1,1))
plot(rec.data$mean.min,rec.data$yr1surv)
lines(xmin,ymin)

simp.mod2<- glm(cbind(seedling.cnt.11.19,
                     num.died)~ave_vwc,data=rec.data,family="binomial")
summary(simp.mod2)
range(rec.data$ave_vwc)
xvwc=seq(0,15,0.01)
yvwc = predict(simp.mod2,list(ave_vwc=xvwc),type="response")
plot(rec.data$ave_vwc,rec.data$yr1surv)
lines(xvwc,yvwc)

###expand data into 0's and 1's (attempt #2)
rec.data$plot<-as.character(rec.data$plot)
rec.data$moms<-as.character(rec.data$moms)
str(rec.data)

survivors = matrix(0,0,3)
for(i in 1:nrow(rec.data)){
r = do.call("rbind", replicate(rec.data[i,4], rec.data[i,1:3], simplify = FALSE))
survivors = rbind(survivors,r)
}
sum(rec.data[,4]) == nrow(survivors)#check to make sure there is correct number of rows                        
head(survivors)
survivors$surv = rep(1,nrow(survivors))

dead = matrix(0,0,3)
for(i in 1:nrow(rec.data)){
  d = do.call("rbind", replicate(rec.data[i,5], rec.data[i,1:3], simplify = FALSE))
  dead = rbind(dead,d)
}
sum(rec.data[,5]) == nrow(dead)#check to make sure there is correct number of rows                        
head(dead)
dead$surv = rep(0,nrow(dead))

survival.data<- rbind(survivors, dead)

survival.data<- merge(survival.data,unique(rec.data[,c("pop","cwd","aet","lat","long","slope","aspect","elev","foldedaspect","heatload","dist_m")]),by="pop")
survival.data<- merge(survival.data,unique(rec.data[,c("plot","ave_vwc","min_vwc","max_vwc","mean.min","mean.max")]),by="plot")
head(survival.data)

###now plot results of model with frequency of survivng and dead seedlings (Alison-type plot)
plot(survival.data$ave_vwc,survival.data$surv)
lines(xvwc,yvwc)
line.vec<- as.data.frame(cbind(xvwc,yvwc))

ggplot(survival.data,aes(x=ave_vwc,y=surv))+
  xlim(3,15)+
  geom_jitter(width=.5,height=.05,col="cyan")+
  geom_line(data=line.vec,aes(x=xvwc,y=yvwc))

##lollipop plots
surv.vwc<- aggregate(surv~ave_vwc,data=survival.data, function(x) sum(x))
ggplot(surv.vwc,aes(x=ave_vwc,y=surv))+
  geom_point()+
  geom_segment(aes(x=ave_vwc,xend=ave_vwc,y=0,yend=surv))

died.vwc<- aggregate(surv~ave_vwc,data=survival.data, function(x){length(which(x==0))})
ggplot(died.vwc,aes(x=ave_vwc,y=surv))+
  geom_point()+
  geom_segment(aes(x=ave_vwc,xend=ave_vwc,y=0,yend=surv))


################################################################
##calculating some numbers for McIntire Stennis report
nrow(field.data.long)
View(field.data.long %>% 
  group_by(mom, plot) %>% 
  summarise(n = n(), germ = sum(germ), surv = sum(surv)))

sum(field.data.long$surv)

field.data.long %>% 
  filter(! plot = "ML") %>% 
  summarise(surv = sum(surv))

