library(reshape2)
library(ggplot2)
library(dplyr)

seeds <- as.data.frame(read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/data/seed_data.csv"))

seed_data_melt= melt(seeds,id.vars=c("site","tree","tag","coneID"))

sites=unique(seeds$site)
for(i in 1:length(sites)){
plot <- ggplot(data=seed_data_melt[seed_data_melt$site==sites[i]& !seed_data_melt$variable=="seed_wt",],aes(x=tree, y=value,fill=variable))+
  geom_boxplot()+
  labs(title=sites[i])+
  geom_hline(yintercept=mean(seeds[seeds$site==sites[i],]$height),color="red")+
  geom_hline(yintercept=mean(seeds[seeds$site==sites[i],]$cone_wt),color="green3")+
  geom_hline(yintercept=mean(seeds[seeds$site==sites[i],]$seed_cnt),color="cyan")+
  geom_hline(yintercept=mean(seeds[seeds$site==sites[i],]$seeds_per_g),color="magenta")
print(plot)
}

ggplot(data=seed_data_melt[seed_data_melt$site=="LH"& !seed_data_melt$variable=="seed_wt",],aes(x=tree, y=value,fill=variable))+
  geom_boxplot()+
  labs(title="LH")+
  geom_hline(yintercept=mean(seeds[seeds$site=="LH",]$height),color="red")+
  geom_hline(yintercept=mean(seeds[seeds$site=="LH",]$cone_wt),color="green3")+
  geom_hline(yintercept=mean(seeds[seeds$site=="LH",]$seed_cnt, na.rm=TRUE),color="cyan")+
  geom_hline(yintercept=mean(seeds[seeds$site=="LH",]$seeds_per_g, na.rm=TRUE),color="magenta")


options(na.action(na.exclude))
seed.agg <- as.data.frame(aggregate(.~site+tree,data=seeds[!seeds$seeds_per_g %in% NA,c("site","tree","height","cone_wt","seed_cnt",
                                           "seeds_per_g")],mean))

##Borden  
sum.BD = summary(seed.agg[seed.agg$site=="BD",c("height","cone_wt","seed_cnt",
                                           "seeds_per_g")],c(0.25,0.5,0.75))
height_low_BD = seed.agg[seed.agg$site=="BD" & seed.agg$height < quantile(seed.agg[seed.agg$site=="BD","height"],c(0.25)),]
height_mid_BD = seed.agg[seed.agg$site=="BD" & seed.agg$height > quantile(seed.agg[seed.agg$site=="BD","height"],c(0.75)),]
height_hi_BD = seed.agg[seed.agg$site=="BD" & seed.agg$height <= quantile(seed.agg[seed.agg$site=="BD","height"],c(0.75))
         & seed.agg$height >= quantile(seed.agg[seed.agg$site=="BD","height"],c(0.25)),]

sites=unique(seeds$site)
vars = c("height","cone_wt","seed_cnt","seeds_per_g")
for(i in 1:length(sites)){
  for(j in 1:length(vars)){
height_low_paste(i) = seed.agg[seed.agg$site==sites[i] & seed.agg[,vars[j]] < quantile(seed.agg[seed.agg$site==sites[i],vars[j]],c(0.25)),]$tree
height_mid_paste(i) = seed.agg[seed.agg$site==sites[i] & seed.agg[,vars[j]] > quantile(seed.agg[seed.agg$site==sites[i],vars[j]],c(0.75)),]$tree
height_hi_paste(i) = seed.agg[seed.agg$site==sites[i] & seed.agg[,vars[j]] <= quantile(seed.agg[seed.agg$site==sites[i],vars[j]],c(0.75))
                        & seed.agg[,vars[j]] >= quantile(seed.agg[seed.agg$site==sites[i],vars[j]],c(0.25)),]$tree
  }}

height_low_1

summary(seed.agg)
seed.agg[seed.agg$site == "BD",]
