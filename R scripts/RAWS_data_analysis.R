#### Climate data from RAWS near High Park Fire #####
library(ggplot2)
library(stringr)

redstone <- read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/climate data/redstone.RAWS.csv",
                     stringsAsFactors = FALSE, header = T)

plot(redstone$total.precip.mm)
redstone$month <- str_sub(redstone$date,0,nchar(redstone$date)-7)
redstone$year <- str_sub(redstone$date,-4,-1)

head(redstone)

plot(redstone[redstone$month %in% c(3,4,5),]$total.precip.mm)

years = seq(2003,2020,1)
redstone.spr.precip = matrix(0,length(years),1)

for(i in 1:length(years)){
  sum = sum(redstone[redstone$month %in% c(3,4,5) & redstone$year == years[i],]$total.precip.mm)
redstone.spr.precip[i,]= sum
}

redstone.spr.precip = cbind(years,redstone.spr.precip)
redstone.spr.precip = as.data.frame(redstone.spr.precip)
redstone.mean.precip = mean(redstone.spr.precip[-10,]$V2)

ggplot(redstone.spr.precip,aes(x=years,y=V2))+
  geom_point()+
  geom_hline(yintercept = redstone.mean.precip)

redstone.MA.precip = matrix(0,length(years),1)

for(i in 1:length(years)){
  sum = sum(redstone[redstone$month %in% c(3,4) & redstone$year == years[i],]$total.precip.mm)
  redstone.MA.precip[i,]= sum
}
redstone.MA.precip = cbind(years,redstone.MA.precip)
redstone.MA.precip = as.data.frame(redstone.MA.precip)
redstone.MA.mean.precip = mean(redstone.MA.precip[-10,]$V2)


ggplot(redstone.MA.precip,aes(x=years,y=V2))+
  geom_point()+
  geom_hline(yintercept = redstone.MA.mean.precip)

may.precip.redstone = redstone[redstone$month==5,]
quantile(may.precip.redstone$total.precip.mm,0.9)

red.UQ = median(may.precip.redstone[may.precip.redstone$total.precip.mm>median(may.precip.redstone$total.precip.mm),]$total.precip.mm)
red.UQ/4*1000*1000/1e+6 #number of liters needed per quad per week 
upperquart.may/4/3.785 #number of gallons needed per quad per week (UQ)

### REDFEATHER ####

redfeather <- read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/climate data/redfeather_RAWS.csv",
                     stringsAsFactors = FALSE, header = T)
colnames(redfeather)[1]="date"

plot(redfeather$total.precip.mm)
redfeather$month <- str_sub(redfeather$date,0,nchar(redfeather$date)-7)
redfeather$year <- str_sub(redfeather$date,-4,-1)

head(redfeather)

plot(redfeather[redfeather$month %in% c(3,4,5),]$total.precip.mm)

years.redfeather = seq(1985,2020,1)
redfeather.spr.precip = matrix(0,length(years.redfeather),1)

for(i in 1:length(years.redfeather)){
  sum = sum(redfeather[redfeather$month %in% c(3,4,5) & redfeather$year == years.redfeather[i],]$total.precip.mm)
  redfeather.spr.precip[i,]= sum
}

redfeather.spr.precip = cbind(years.redfeather,redfeather.spr.precip)
redfeather.spr.precip = as.data.frame(redfeather.spr.precip)
redfeather.mean.precip = mean(redfeather.spr.precip[-27,]$V2)

ggplot(redfeather.spr.precip,aes(x=years.redfeather,y=V2))+
  geom_point()+
  geom_hline(yintercept = redfeather.mean.precip)

redfeather.MA.precip = matrix(0,length(years.redfeather),1)

for(i in 1:length(years.redfeather)){
  sum = sum(redfeather[redfeather$month %in% c(3,4) & redfeather$year == years.redfeather[i],]$total.precip.mm)
  redfeather.MA.precip[i,]= sum
}
redfeather.MA.precip = cbind(years.redfeather,redfeather.MA.precip)
redfeather.MA.precip = as.data.frame(redfeather.MA.precip)
redfeather.MA.mean.precip = mean(redfeather.MA.precip[-27,]$V2)

ggplot(redfeather.MA.precip,aes(x=years.redfeather,y=V2))+
  geom_point()+
  geom_hline(yintercept = redfeather.MA.mean.precip)

apr.precip = redfeather[redfeather$month==4,]
may.precip = redfeather[redfeather$month==5,]
june.precip = redfeather[redfeather$month==6, ]

ggplot(may.precip,aes(y=total.precip.mm))+
  geom_boxplot()

redfeather$month=as.numeric(redfeather$month)
redfeather$month=as.factor(redfeather$month)

ggplot(redfeather[which(!(redfeather$month==4&redfeather$year==2011)),], aes(x=month,y=total.precip.mm))+
  geom_boxplot(fill="lightblue")+
  geom_boxplot(aes(x=month,y=ave.temp.C),fill="pink")

redstone$month=as.factor(as.numeric(redstone$month))
ggplot(redstone, aes(x=month,y=total.precip.mm))+
  geom_boxplot(fill="lightblue")+
  geom_boxplot(aes(x=month,y=ave.temp.C),color="red",fill="pink")

summary(may.precip[-1,]$total.precip.mm)
quant90 = quantile(may.precip[-1,]$total.precip.mm,0.9)
quant90/4
##number of gallons per plot needed to reach average weekly precip for 90th percentile may precip
water.mm3 = quant90/4*1000*1000 #number of mm^3 of water per quad per week 
water.mm3/1e+6 # number of liters needed per quad per week 
water.mm3/1e+6/3.785 #number of gal needed per quad per week 

week1.mm = 9.906
week1.gal = week1.mm/3.785
water.mm3/1e+6/3.785 - week1.gal

may.precip.adj = may.precip[-1,]
upperquart.may = median(may.precip.adj[may.precip.adj$total.precip.mm>median(may.precip.adj$total.precip.mm),]$total.precip.mm)
upperquart.may/4*1000*1000/1e+6 #number of liters needed per quad per week 
upperquart.may/4/3.785 #number of gallons needed per quad per week (UQ)
upperquart.may/4/3.785 - week1.gal

###week of 5/21 - 5/28
week2.mm.mid = 0.004 + 8.102
week2.gal.mid = week2.mm.mid/3.785
upperquart.may/4/3.785 - week2.gal.mid
week2.mm.high = 0.183 + 8.599
week2.gal.high = week2.mm.high/3.785
upperquart.may/4/3.785 - week2.gal.high
  
7.6 * 250 * 250 / 1e+6 #liters for a .25 x .25 m square @ 2 gallons per quad

###week of 5/28 - 6/4
week3.mm.mid = 0.835+0.515+0.056+2.157+1.362+.393
week3.gal.mid = week3.mm.mid/3.785
upperquart.may/4/3.785 - week3.gal.mid
week3.mm.high = 1.094+0.676+0.146+1.752+3.882+.479
week3.gal.high = week3.mm.high/3.785
upperquart.may/4/3.785 - week3.gal.high

##june data
upperquart.june = median(june.precip[june.precip$total.precip.mm>median(june.precip$total.precip.mm),]$total.precip.mm)
upperquart.june/4/3.785 - week3.gal.mid
upperquart.june/4/3.785 - week3.gal.high

###week of 6/5 - 6/12
week4.mm.mid =1.509
week4.gal.mid = week4.mm.mid/3.785
upperquart.june/4/3.785 - week4.gal.mid
week4.mm.high = 1.224
week4.gal.high = week4.mm.high/3.785
upperquart.june/4/3.785 - week4.gal.high

week4.mm.rf = 1.27+3.56+10.41
upperquart.june/4/3.785-week4.mm.rf/3.785

quantile(june.precip$total.precip.mm,.9)/4/3.785-week4.mm.rf/3.785

###week of 6/13 - 6/20
week5.mm.mid =.819 + 3.023 + 2.018
week5.gal.mid = week5.mm.mid/3.785
upperquart.june/4/3.785 - week5.gal.mid
week5.mm.high = .883 + 3.797 + 1.905
week5.gal.high = week5.mm.high/3.785
upperquart.june/4/3.785 - week5.gal.high

summary(june.precip$total.precip.mm)/4
median(june.precip$total.precip.mm)/4/3.785

###week of 6/21 - 6/27
week6.mm.mid = 1.922+.282 + 2.688 + 2.452+.332
week6.gal.mid = week6.mm.mid/3.785
upperquart.june/4/3.785 - week6.gal.mid
week6.mm.high = 2.17 +.381 + 2.821 + 2.281+.889
week6.gal.high = week6.mm.high/3.785
upperquart.june/4/3.785 - week6.gal.high

##look at july precip history
july.precip = redfeather[redfeather$month==7, ]
summary(july.precip$total.precip.mm)
upperquart.july = median(july.precip[july.precip$total.precip.mm>median(july.precip$total.precip.mm),]$total.precip.mm)
upperquart.july/4/3.785
mean.july= mean(july.precip$total.precip.mm)

##week of 6/28 - 7/4
week7.mm.mid = 0.314 + 0.224 +0.152
week7.gal.mid = week7.mm.mid/3.785
upperquart.june/4/3.785 - week7.gal.mid
week7.mm.high = 0.103 + 0.014 + 0.368
week7.gal.high = week7.mm.high/3.785
upperquart.june/4/3.785 - week7.gal.high

##week of 7/5 - 7/11
week7.mm.mid = 3.676 +1.188
week7.gal.mid = week7.mm.mid/3.785
upperquart.july/4/3.785 - week7.gal.mid
week7.mm.high = 2.935 + 1.748
week7.gal.high = week7.mm.high/3.785
upperquart.july/4/3.785 - week7.gal.high

##week of 7/12 - 7/18
week8.mm.mid = 0.638 + 1.266+0.273
week8.gal.mid = week8.mm.mid/3.785
upperquart.july/4/3.785 - week8.gal.mid
week8.mm.high = 0.635 + 1.663+0.458
week8.gal.high = week8.mm.high/3.785
upperquart.july/4/3.785 - week8.gal.high

##week of 7/19 - 7/25
week9.mm.mid = 0.269 + 0.59 + 4.358+0.874+0.273
week9.gal.mid = week9.mm.mid/3.785
upperquart.july/4/3.785 - week9.gal.mid
week9.mm.high = 0.334 + 0.826 + 4.688 +1.535+0.288
week9.gal.high = week9.mm.high/3.785
upperquart.july/4/3.785 - week9.gal.high
mean.july/4/3.785

##look at august precip history
aug.precip = redfeather[redfeather$month==8, ]
summary(aug.precip$total.precip.mm)/4/3.785
upperquart.aug = median(aug.precip[aug.precip$total.precip.mm>median(aug.precip$total.precip.mm),]$total.precip.mm)
upperquart.aug/4/3.785
mean.aug= mean(aug.precip$total.precip.mm)

##week of 7/26 - 8/1
week10.mm.mid = 0.474+2.883+0.013+2.883+3.743+3.007
week10.gal.mid = week10.mm.mid/3.785
upperquart.july/4/3.785 - week10.gal.mid
upperquart.aug/4/3.785 - week10.gal.mid
week10.mm.high = 1.055+0.006+0.12+4.911+7.167+4.953
week10.gal.high = week10.mm.high/3.785
upperquart.july/4/3.785 - week10.gal.high
upperquart.aug/4/3.785 - week10.gal.high

##week of 8/2 - 8/8
week10.mm.mid = 2.631+0.5+0.68+2.323+0.625+2.612
week10.gal.mid = week10.mm.mid/3.785
upperquart.aug/4/3.785 - week10.gal.mid
week10.mm.high = 2.577+0.584+0.723+2.062+0.696+4.09
week10.gal.high = week10.mm.high/3.785
upperquart.aug/4/3.785 - week10.gal.high

##week of 8/9 - 8/15
week11.mm.mid=0
week11.mm.high=0

##week of 8/16 - 8/22
week12.mm.mid = 1.765 + 1.127
week12.gal.mid = week12.mm.mid/3.785
upperquart.aug/4/3.785 - week12.gal.mid
week12.mm.high = 1.781 + 1.265
week12.gal.high = week12.mm.high/3.785
upperquart.aug/4/3.785 - week12.gal.high

##week of 8/23 - 8/29
week13.mm.mid = 1.578 + 1.289 + 1.674 + 3.367
week13.gal.mid = week13.mm.mid/3.785
upperquart.aug/4/3.785 - week13.gal.mid
week13.mm.high = 1.286 + 1.891 +2.193 + 3.378
week13.gal.high = week13.mm.high/3.785
upperquart.aug/4/3.785 - week13.gal.high

##week of 8/30 - 9/5
week14.mm.mid = 3.209 +0.02 + 0.947
week14.gal.mid = week14.mm.mid/3.785
upperquart.aug/4/3.785 - week14.gal.mid
week14.mm.high = 2.932 + 1.261
week14.gal.high = week14.mm.high/3.785
upperquart.aug/4/3.785 - week14.gal.high

#### Look at daily precip trends in May ####
library(chron)

may.data <- read.table("C:/Users/Katie/Google Drive/Ponderosa seed project/climate data/redfeather_may_precip.txt")

colnames(may.data)=c("date","year","doy","run","precip.mm")
may.data$day = substr(may.data$date,0,5)
may.data = may.data[-281,]

##first week of May
may.data.1 = may.data[may.data$day %in% str_sub(seq.dates("05/01/20","05/07/20",by="days"),0,5),]
may.data.1.sum = aggregate(precip.mm~year,data=may.data.1,FUN=function(x) sum(x))
summary(may.data.1.sum$precip.mm)
boxplot(may.data.1.sum$precip.mm)
#upper quartile
median(may.data.1.sum[may.data.1.sum$precip.mm > median(may.data.1.sum$precip.mm),]$precip.mm)


##second week of May
may.data.2 = may.data[may.data$day %in% str_sub(seq.dates("05/08/20","05/14/20",by="days"),0,5),]
may.data.2.sum = aggregate(precip.mm~year,data=may.data.2,FUN=function(x) sum(x))
summary(may.data.2.sum$precip.mm)
boxplot(may.data.2.sum$precip.mm)
#upper quartile
median(may.data.2.sum[may.data.2.sum$precip.mm > median(may.data.2.sum$precip.mm),]$precip.mm)


##third week of May
may.data.3 = may.data[may.data$day %in% str_sub(seq.dates("05/15/20","05/21/20",by="days"),0,5),]
may.data.3.sum = aggregate(precip.mm~year,data=may.data.3,FUN=function(x) sum(x))
summary(may.data.3.sum$precip.mm)
boxplot(may.data.3.sum$precip.mm)
#upper quartile
median(may.data.3.sum[may.data.3.sum$precip.mm > median(may.data.3.sum$precip.mm),]$precip.mm)


##fourth week of May
may.data.4 = may.data[may.data$day %in% str_sub(seq.dates("05/22/20","05/31/20",by="days"),0,5),]
may.data.4.sum = aggregate(precip.mm~year,data=may.data.4,FUN=function(x) sum(x))
summary(may.data.4.sum$precip.mm)
boxplot(may.data.4.sum$precip.mm)
#upper quartile
median(may.data.4.sum[may.data.4.sum$precip.mm > median(may.data.4.sum$precip.mm),]$precip.mm)
