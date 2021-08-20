germ.data <- read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/data/pondo_field_germ_data_entered.csv")

germ.data2 = germ.data[,-seq(10,28,2)]
colnames(germ.data2)

check.data = cbind(germ.data2[,1:7],
                   germ.data2$seedling.cnt.6.10.12-germ.data2$seedling.cnt.6.3.4,
                   germ.data2$seedling.cnt.6.16.18-germ.data2$seedling.cnt.6.10.12,
                   germ.data2$seedling.cnt.6.23.25-germ.data2$seedling.cnt.6.16.18,
                   germ.data2$seedling.cnt.6.30.7.1-germ.data2$seedling.cnt.6.23.25,
                   germ.data2$seedling.cnt.7.7.8-germ.data2$seedling.cnt.6.30.7.1,
                   germ.data2$seedling.cnt.7.15-germ.data2$seedling.cnt.7.7.8,
                   germ.data2$seedling.cnt.7.22.23-germ.data2$seedling.cnt.7.15,
                   germ.data2$seedling.cnt.7.29-germ.data2$seedling.cnt.7.22.23,
                   germ.data2$seedling.cnt.8.6-germ.data2$seedling.cnt.7.29)

colnames(check.data)[8:16]=c("check1","check2","check3","check4",
                             "check5","check6","check7","check8","check9")
View(check.data)
