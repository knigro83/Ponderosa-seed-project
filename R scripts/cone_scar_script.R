### Ponderosa Project -- Cone Scars ###

scar.data <- read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/data/cone_scars_2019_r.csv")
head(scar.data)
mean.cones = aggregate(cones~tree+year,data=scar.data,mean)
mean.cones$uid = paste(mean.cones$tree,mean.cones$year,sep="")
count.cones = aggregate(cones~tree+year,data=scar.data,function(x) length(x))
count.cones$uid = paste(count.cones$tree,count.cones$year,sep="")
cone.data = merge(mean.cones,count.cones[,c("cones","uid")],by="uid")
colnames(cone.data)[c(4,5)]=c("mean.cones","branch.count")

##only use years that had at least 4 branches
cone.data.reduced = cone.data[cone.data$branch.count >3, ]

nrow(mean.cones)
nrow(count.cones)
nrow(cone.data)
nrow(cone.data.reduced)

head(cone.data.reduced)

##add leadershoot data to cones
leadershoots <- read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/data/leadershoots_2019.csv")
head(leadershoots)

all.data <- merge(cone.data.reduced,leadershoots[,-1],by="tree",all.x=TRUE)
all.data$total.cones = all.data$mean.cones*all.data$LS.count
head(all.data)
nrow(all.data)

##write data
write.csv(all.data, "ponderosa_cone_data_2019.csv")


### Linhart data ###
linhart <- read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/data/linhart_conescar.csv",check.names = FALSE)
head(linhart)
colnames(linhart)[1]="tree.number"

tree.number = unique(linhart$tree.number)
summary=matrix(0,ncol(linhart)-1,length(tree.number))

for(t in 1:length(tree.number)){
  for(i in 2:ncol(linhart)){
  m = mean(linhart[linhart$tree.number == tree.number[t], i],na.rm=T)
  summary[i-1,t]=m
  }}

colnames(summary)=tree.number
linhart.cones = cbind(seq(2018,2000,-1),summary)
colnames(linhart.cones)[1]="year"
library(reshape2)

linhart.cones.melt = melt(as.data.frame(linhart.cones), id.vars=c("year"))
colnames(linhart.cones.melt)=c("year","tree","mean.cones")
linhart.cones.melt$site=rep("LH",nrow(linhart.cones.melt))
linhart.cones.melt$tree2 = paste(linhart.cones.melt$site,"T",linhart.cones.melt$tree,sep="")

##add leadershoots
lin.ls = read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/data/linhart_tree_validation.csv")
lin.ls

linhart.cones.final = merge(linhart.cones.melt, lin.ls, by.x="tree",by.y="Tree.Nuber",all.y=FALSE)
linhart.cones.final$total.cones = linhart.cones.final$mean.cones*linhart.cones.final$Leadershoot.Estimate

##write out data
write.csv(linhart.cones.final, "linhart_cone_summary.csv")
