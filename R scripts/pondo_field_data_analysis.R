### Ponderosa field experiment data analysis ###
## last edit: 6/26/20 ##
library(ggplot2)
library(ggpubr)
library(maditr)

germ.data <- read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/data/pondo_field_germ_data_entered.csv")
colnames(germ.data)

max(germ.data$seedling.cnt.5.27,na.rm=T)
max(germ.data$seedling.cnt.6.10.12,na.rm = T)
max(germ.data$seedling.cnt.6.16.18,na.rm = T)
max(germ.data$seedling.cnt.6.23.25,na.rm = T)
max(germ.data$seedling.cnt.6.3.4,na.rm = T)
max(germ.data$seedling.cnt.6.30.7.1,na.rm = T)
max(germ.data$seedling.cnt.7.15,na.rm = T)
max(germ.data$seedling.cnt.7.22.23,na.rm = T)
max(germ.data$seedling.cnt.7.7.8,na.rm = T)
max(germ.data$seedling.cnt.7.29,na.rm = T)
max(germ.data$seedling.cnt.8.6,na.rm = T)
max(germ.data$seedling.cnt.11.19,na.rm = T)

totals = c(sum(germ.data$seedling.cnt.6.3.4,na.rm=T),
sum(germ.data$seedling.cnt.6.10.12,na.rm=T),
sum(germ.data$seedling.cnt.6.16.18,na.rm=T),
sum(germ.data$seedling.cnt.6.23.25,na.rm=T),
sum(germ.data$seedling.cnt.6.30.7.1,na.rm=T),
sum(germ.data$seedling.cnt.7.7.8,na.rm=T),
sum(germ.data$seedling.cnt.7.15,na.rm=T),
sum(germ.data$seedling.cnt.7.22.23,na.rm=T),
sum(germ.data$seedling.cnt.7.29,na.rm=T),
sum(germ.data$seedling.cnt.8.6,na.rm=T),
sum(germ.data$seedling.cnt.11.19,na.rm=T)
)

weekly.total = as.data.frame(cbind(seq(1,11,1),totals))
colnames(weekly.total)=c("week","count")
ggplot(weekly.total,aes(x=week,y=count))+
  scale_x_continuous(breaks=seq(1,11,1))+
  geom_line()

sum.moms = aggregate(seedling.cnt.11.19~moms,data=germ.data[!is.na(germ.data$seedling.cnt.11.19),],function(x) sum(x))
plot(sum.moms)
sum.plots = aggregate(seedling.cnt.11.19~plot,data=germ.data[!is.na(germ.data$seedling.cnt.11.19),],function(x) sum(x))
plot(sum.plots)

germ.data$diff = germ.data$seedling.cnt.11.19 - germ.data$seedling.cnt.8.6
unique(germ.data[germ.data$diff>0, ]$uid)
sum(germ.data[germ.data$diff>0, ]$diff,na.rm=T)
sum(germ.data[germ.data$diff<0, ]$diff,na.rm=T)
aggregate(diff~uid+moms, data=germ.data[germ.data$diff >0, ], function(x)sum(x))
aggregate(diff~plot, data=germ.data[germ.data$diff >0, ], function(x)sum(x))
aggregate(diff~plot, data=germ.data[germ.data$diff <0, ], function(x)sum(x))
aggregate(diff~plot, data=germ.data, function(x) sum(x))

germ.data$max = apply(germ.data[,c(8,seq(9,29,2))], 1, max, na.rm=T)
germ.data$max.diff = germ.data$seedling.cnt.11.19 -  germ.data$max
sum(germ.data[germ.data$max.diff>0, ]$max.diff,na.rm=T)
sum(germ.data[germ.data$max.diff<0, ]$max.diff,na.rm=T)


sum.moms.plots = as.data.frame(aggregate(seedling.cnt.11.19~moms+plot,data=germ.data,function(x) sum(x)))
sum.moms.plots$moms.site = substr(sum.moms.plots$moms,0,3)
sum.moms.plots$plot.type = substr(sum.moms.plots$plot,0,2)

sample.size.by.mom = aggregate(seedling.cnt.11.19~moms+plot.type, data=sum.moms.plots, function(x) sum(x))
head(sample.size.by.mom)

ggplot(sample.size.by.mom, aes(x=moms,y=seedling.cnt.11.19,fill=plot.type))+
  geom_bar(stat="identity",position=position_dodge())+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

moms.summary = dcast(data=sum.moms.plots[,1:3], formula = moms ~plot, value.var="seedling.cnt.11.19")
moms.summary$total = apply(moms.summary[,2:19], 1, sum)
moms.summary$missing = 30*18 - moms.summary$total
View(moms.summary)

ggplot(sum.moms.plots, aes(x=plot, y=seedling.cnt.11.19, fill=moms.site))+
  geom_bar(stat="identity",position="dodge")

##create table for the number of empty cells for each mom/plot
empty.cells = germ.data[which(germ.data$seedling.cnt.11.19==0),]
head(empty.cells)
empty.cell.sum = as.data.frame(aggregate(seedling.cnt.11.19~moms+plot,data=empty.cells,function(x) length(x)))
empty.cell.summary = dcast(data=empty.cell.sum, formula=moms~plot, value.var="seedling.cnt.11.19")
empty.cell.summary$total = apply(empty.cell.summary[,2:19], 1, sum,na.rm=T)

##create table for number of cells with 3 or 4 seedlings for each mom/plot
full.cells = germ.data[which(germ.data$seedling.cnt.11.19==3 | germ.data$seedling.cnt.11.19==4),]
head(full.cells)
full.cell.sum = as.data.frame(aggregate(seedling.cnt.11.19~moms+plot,data=full.cells,function(x) length(x)))
full.cell.summary = dcast(data=full.cell.sum, formula=moms~plot, value.var="seedling.cnt.11.19")
full.cell.summary$total = apply(full.cell.summary[,2:ncol(full.cell.summary)], 1, sum,na.rm=T)
max(full.cell.summary$total,na.rm=T)

### totals by site ###
totals.high = c(sum(germ.data[grep("HH", germ.data$plot), ]$seedling.cnt.6.3.4,na.rm=T),
           sum(germ.data[grep("HH", germ.data$plot), ]$seedling.cnt.6.10.12,na.rm=T),
           sum(germ.data[grep("HH", germ.data$plot), ]$seedling.cnt.6.16.18,na.rm=T),
           sum(germ.data[grep("HH", germ.data$plot), ]$seedling.cnt.6.23.25,na.rm=T),
           sum(germ.data[grep("HH", germ.data$plot), ]$seedling.cnt.6.30.7.1,na.rm=T),
           sum(germ.data[grep("HH", germ.data$plot), ]$seedling.cnt.7.7.8,na.rm=T),
           sum(germ.data[grep("HH", germ.data$plot), ]$seedling.cnt.7.22.23,na.rm=T),
           sum(germ.data[grep("HH", germ.data$plot), ]$seedling.cnt.7.29,na.rm=T),
           sum(germ.data[grep("HH", germ.data$plot), ]$seedling.cnt.8.6,na.rm=T),
           sum(germ.data[grep("HH", germ.data$plot), ]$seedling.cnt.11.19,na.rm=T))
weekly.total.high = as.data.frame(cbind(seq(1,10,1),totals.high))
colnames(weekly.total.high)=c("week","count")
high.plot = ggplot(weekly.total.high,aes(x=week,y=count))+
  geom_line()+
  ggtitle("high")+
  ylim(0,4000)


totals.mid = c(sum(germ.data[grep("M", germ.data$plot), ]$seedling.cnt.6.3.4,na.rm=T),
                sum(germ.data[grep("M", germ.data$plot), ]$seedling.cnt.6.10.12,na.rm=T),
                sum(germ.data[grep("M", germ.data$plot), ]$seedling.cnt.6.16.18,na.rm=T),
                sum(germ.data[grep("M", germ.data$plot), ]$seedling.cnt.6.23.25,na.rm=T),
                sum(germ.data[grep("M", germ.data$plot), ]$seedling.cnt.6.30.7.1,na.rm=T),
                sum(germ.data[grep("M", germ.data$plot), ]$seedling.cnt.7.7.8,na.rm=T),
                sum(germ.data[grep("M", germ.data$plot), ]$seedling.cnt.7.22.23,na.rm=T),
               sum(germ.data[grep("M", germ.data$plot), ]$seedling.cnt.7.29,na.rm=T),
               sum(germ.data[grep("M", germ.data$plot), ]$seedling.cnt.8.6,na.rm=T),
               sum(germ.data[grep("M", germ.data$plot), ]$seedling.cnt.11.19,na.rm=T))
weekly.total.mid = as.data.frame(cbind(seq(1,10,1),totals.mid))
colnames(weekly.total.mid)=c("week","count")
mid.plot = ggplot(weekly.total.mid,aes(x=week,y=count))+
  geom_line()+
  ggtitle("mid")+
  ylim(0,4000)

ggarrange(nrow=2,ncol=1,mid.plot,high.plot)
