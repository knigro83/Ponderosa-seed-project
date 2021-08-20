#### MAKE MAP OF PONDEROSA SEED SOURCE SITES WITH ELEVATION AND CWD ####
# used tutorial at this website: https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html


#read in packages

library("rnaturalearth")
library("rnaturalearthdata")
library(rgeos)
library(ggplot2)
library(ggspatial)
library(ggrepel)
library(cowplot)
library(sf)
library(maps)
library(raster)

###read in data

site.data <- read.csv("C:/Users/Katie/Google Drive/Ponderosa seed project/site information/site.vars.csv")
elevation <- raster("C:/Users/Katie/Google Drive/ArcGIS/front range topo/dem_frontrange.tif")
ponderosa <- raster("C:/Users/Katie/Google Drive/ArcGIS/US140_EVT/US_140EVT_20180618/Grid/us_140evt")

plot(ponderosa)
str(ponderosa)
extent(ponderosa)#need to change crs
crop(ponderosa, extent())

ponderosa_df<- as.data.frame(ponderosa, xy=TRUE)

ggplot()+
  geom_raster(data=ponderosa, aes(x=, y=lat, group = ))



#checking out the elevation data 
summary(elevation)
elevation.df <- as.data.frame(elevation, xy=TRUE)
str(elevation.df)

elevation_lowres <- raster(nrow = round(nrow(elevation)/10), ncol = round(ncol(elevation)/10)) 
elevation_lowres <- resample(elevation, elevation_lowres, method = 'bilinear')
plot(elevation_lowres)
summary(elevation_lowres)
ncol(elevation_lowres)


elevation_agg <- aggregate(elevation, 10, fun=mean)
plot(elevation_agg)
elevation_agg_df<- as.data.frame(elevation_agg, xy=TRUE)

ggplot()+
  geom_raster(data=elevation_agg_df, aes(x=x,y=y, fill = dem_frontrange))+
  scale_fill_viridis_c()+
  coord_quickmap()


world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
colorado <- states[states$ID=="colorado",]
map_data("state")
cocities <- data.frame(state = rep("colorado", 4), 
                       city = c("Denver", "Fort Collins", "Pueblo","Colorado Springs"), 
                       lat = c(39.7392,40.5853,38.2544,38.8339), 
                       lng = c(-104.9903,-105.0844,-104.6091,-104.8214))
cocities <- st_as_sf(cocities, coords = c("lng", "lat"), remove = FALSE, 
                     crs = 4326, agr = "constant")

#Katie's sites
ggplot(data = world) +
  #geom_sf(data=states, fill=NA)+
  #geom_sf(data=colorado, fill="lightgray")+
  geom_raster(data=elevation_agg_df, aes(x=x,y=y, fill = dem_frontrange))+
  geom_sf(data=cocities)+
  geom_text(data = cocities, aes(x = lng+c(0.4,0.6,0.4,0.8), y = lat, label = city), 
            size = 4, col = "black", fontface = "bold")+
  coord_sf(xlim = c(-106,-104.5), ylim = c(38, 41), expand = FALSE)+
  geom_point(data=site.data, aes(x=long,y=lat,color=cwd),shape=17,size=3)+
  scale_color_gradient(low="blue", high="red")+
  theme_bw()+
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  xlab("Longitude") + ylab("Latitude")+
  labs(color="CWD")+
  theme(panel.grid.major = element_line(color=NA),axis.text=element_text(size=12),
                          axis.title=element_text(size=14))

#for Ivy

#just grab Boulder and Fort Collins
nococities <- data.frame(state = rep("colorado", 2), 
                       city = c("Fort Collins", "Boulder"), 
                       lat = c(40.5853,40.0150), 
                       lng = c(-105.0844,-105.2705))
nococities <- st_as_sf(nococities, coords = c("lng", "lat"), remove = FALSE, 
                     crs = 4326, agr = "constant")

#clip DEM to just noco 
noco_ex<- extent(-106,-105,39.8,40.8)
elevation_noco <- crop(elevation,noco_ex)
#make it lower resolution with aggregate()
elevation_noco_agg <- aggregate(elevation_noco, 10, fun=mean)
elevation_noco_agg_df <- as.data.frame(elevation_noco_agg, xy=TRUE)

#clip site data to just the Boulder and Borden sites
site.data.ivy <- site.data[site.data$site %in% c("BD","LH"),]

ggplot(data = world) +
  #geom_sf(data=states, fill=NA)+
  #geom_sf(data=colorado, fill="lightgray")+
  geom_raster(data=elevation_noco_agg_df, aes(x=x,y=y, fill = dem_frontrange))+
  scale_fill_gradient(low="black",high="white")+
  geom_sf(data=nococities, col="white")+
  geom_text(data = nococities, aes(x = lng + c(0,0.05), y = lat + c(0.015,0), label = city), 
            size = 5, col = "white", fontface = "bold")+
  geom_point(aes(y = 40.571633, x = -105.081249),shape=14, col = "green", size = 5)+
  coord_sf(xlim = c(-106,-105), ylim = c(39.8, 40.8), expand = FALSE)+
  geom_point(data=site.data.ivy, aes(x=long,y=lat,color=cwd),shape=17,size=5)+
  scale_color_gradient(low="blue", high="red")+
  theme_bw()+
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  xlab("Longitude") + ylab("Latitude")+
  labs(color="CWD")+
  theme(panel.grid.major = element_line(color=NA),axis.text=element_text(size=12),
        axis.title=element_text(size=14))
##need to add ponderosa range
