###creating order for ponderosa greenhouse experiment

fams <- read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/data/greenhouse exp/greenhouse_families.csv",header = FALSE)

random.samp<- sample(fams[,1],nrow(fams))

write.csv(random.samp,"C:/Users/Katie/Google Drive/Ponderosa seed project/data/greenhouse exp/random_samp.csv")

set.seed(83)
fams2 <- read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/data/greenhouse exp/families_w2.csv",header = FALSE)

random.samp2<- sample(fams2[,1],nrow(fams2))

write.csv(random.samp2,"C:/Users/Katie/Google Drive/Ponderosa seed project/data/greenhouse exp/random_samp2.csv")

fams3 <- read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/data/greenhouse exp/more_fams.csv",header = FALSE)
random.samp3<- sample(fams2[,1],nrow(fams3))
write.csv(random.samp3,"C:/Users/Katie/Google Drive/Ponderosa seed project/data/greenhouse exp/random_samp3.csv")

dr <- read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/data/greenhouse exp/DROUGHT_RACKS.csv",header = FALSE)

c(unlist(dr))
