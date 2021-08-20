
a = vector()

for(i in seq(1,10,1)){
r= rep(i,10)
a=c(a,r)
}
a
quad = cbind(rep(a,2),c(rep(1:10,10),rep(11:20,10)))

M = do.call(rbind, replicate(36, quad, simplify=FALSE))
colnames(M)=c("row","column")

plot.layout = read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/data/plot_layout_codes.csv")
plot.quad = unique(plot.layout[,c("plot","quad")])

p=matrix(,,2)
colnames(p)=c("plot","quad")
for(i in 1:nrow(plot.quad)){
  r= do.call(rbind, replicate(200, plot.quad[i,], simplify=FALSE))
  p=rbind(p,r)
}
p = p[-1,]
nrow(p)

M = cbind(p,M)
head(M)

num = aggregate(row~plot+quad, data=plot.layout, function(x) sum(x))

less.quads = num[num$row==100,]
less.quads$uid = paste(less.quads$plot,less.quads$quad,sep=".")

M$uid = paste(M$plot,M$quad,sep=".")

M = M[-which(M$uid %in% less.quads$uid & M$row==10 & M$column %in% c(11:20)),]
nrow(M)

moms = as.character(plot.layout$mom)
n=vector()
for(i in 1:length(moms)){
  r= rep(moms[i],10)
  n=c(n,r)
}
n
length(n)

M$moms = n

View(M)

M[which(M$uid == "HH5.1" & M$row==3),]

write.csv(M, "pondo_field_germ_data_setup.csv")
