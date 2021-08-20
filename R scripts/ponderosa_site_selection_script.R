sites<-read.csv("C:/Users/Katie/Google Drive/ponderosa_sites_R.csv")
plot(sites$lat, sites$CWD_1981_2)
plot(sites$lat, sites$PRISM_tmea)
plot(sites$lat, sites$PRISM_tmea)
plot(sites$lat, sites$PRISM_ppt_)
plot(sites$elevation, sites$PRISM_tmea)
plot(sites$elevation, sites$PRISM_ppt_)
plot(sites$elevation, sites$CWD_1981_2)
plot(sites$lat, sites$CWD_1981_2)

##find current ponderosa range in high park fire 
library(adehabitatHR)
library(raster)
library(foreign)

pond.elev = raster("C:/Users/Katie/Google Drive/ArcGIS/US140_EVT/pond_elev")

hist(pond.elev,breaks=50, main="Elevation of ponderosa pine in High Park Fire",
     xlab="Elevation (m)", xlim=c(1550,3000))
abline(v=c(2323,2895), col=c("deeppink4","dodgerblue"), lwd=2)

quantile(pond.elev, c(0.025,0.5,0.975))
mean(pond.elev)
max(pond.elev)

