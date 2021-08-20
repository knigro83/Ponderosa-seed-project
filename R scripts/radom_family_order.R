## randomizing pondo exp

ids<- read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/data/field.tree.ids.csv",header = F,
               stringsAsFactors = F)
ids
str(ids)
sample(ids,1)
ids[sample(1:length(ids),1),]

avector <- as.vector(ids['a2'])
class(avector) 

avector <- aframe[['a2']]
class(avector)

avector <- ids[,1]
class(avector)
length(ids[,1])

#for sowing
order=matrix(data=NA,nrow=39,ncol=24)

for(i in 1:24){
samp <- sample(ids[,1],39)
order[,i]=samp}
order

write.csv(order, "family.order.csv")

#for planting
order2=list()

for(i in 1:18){
  samp <- c(sample(ids[,1],39),NA)
  samp.mat <- matrix(samp,nrow=8,ncol=5)
  order2[[i]] <- samp.mat
  }
order2
lapply(order2, function(x) write.table(data.frame(x), 'C:/Users/Katie/Google Drive/Ponderosa seed project/planting.matrices.csv', append=T, sep=','))


#ambient plot number
set.seed(83)
plot.order= matrix(data=NA, nrow=24,ncol=4)
colnames(plot.order)=c("amb","plant","seed","seed")
for(i in 1:24){
samp2 <- sample(c(1,2,3,4),4,replace=F)
plot.order[i,]=samp2
  }
plot.order
write.csv(plot.order, "plot.order.csv")
