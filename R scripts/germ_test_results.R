#### Ponderosa germination test results ####
library(reshape2)
library(ggplot2)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

germ_data <- read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/data/germ_data_R.csv")

germ_data <- melt(germ_data, id.vars=c("site","tree","uid","label","treatment","bag.color","num.seeds"))
ggplot(germ_data, aes(x=variable, y=value/num.seeds, group=uid, col=treatment))+
  geom_line()+
  theme(axis.text.x = element_text(angle=45))

germ_data_subset <- germ_data[which(germ_data$uid %in% c("BH5","BH6","BH7","BH8","HM14","HM16","HM17","WT18","WT115","LHM45","BH5-CS","BH6-CS","BH7-CS","BH8-CS","HM14-CS","HM16-CS","HM17-CS","WT18-CS","WT115-CS","LHM45-CS"
)),]
germ_data_subset$treeID <- paste(germ_data_subset$site,germ_data_subset$tree)
germ_data_subset$prop <- germ_data_subset$value/germ_data_subset$num.seeds
  
ggplot(germ_data_subset, aes(x=variable, y=prop, group=uid, col=site, lty=treatment))+
  scale_colour_manual(values=cbbPalette)+
  geom_line()

germ_data_end <- germ_data_subset[which(germ_data_subset$variable=="X4.27.2020"),]
germ_data_end

ggplot(germ_data_end, aes(x=treeID, y=prop, fill=treatment))+
  geom_bar(position="dodge",stat="identity")+
  scale_fill_manual(values=c("lightblue","tan"))

##overall percent germinated (62% (cold) vs 54% (none))
prop.cold = sum(germ_data_end[germ_data_end$treatment=="cold",]$value)/sum(germ_data_end[germ_data_end$treatment=="cold",]$num.seeds)
prop.none = sum(germ_data_end[germ_data_end$treatment=="none",]$value)/sum(germ_data_end[germ_data_end$treatment=="none",]$num.seeds)
prop.cold 
prop.none

ggplot(germ_data_end, aes(x=treatment, y=prop, fill=treatment))+
  geom_boxplot()+
  geom_point(data = data.frame(x = factor(c("cold","none")), y = c(prop.cold,prop.none)),
             aes(x=x, y=y), fill = 'gold',shape=23,size=6)+
  geom_dotplot(binaxis='y', stackdir='center',
                              position=position_dodge(1),fill="black",binwidth =0.02)+
  scale_fill_manual(values=c("lightblue","tan"))

t.test(germ_data_end[germ_data_end$treatment=="cold",]$prop,germ_data_end[germ_data_end$treatment=="none",]$prop)

