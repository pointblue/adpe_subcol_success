library(sp)
library(ggplot2)
require(dplyr)
require(gridExtra)
library(raster)
library(viridis)
library(rgdal)
library(ggsn)
library(imager)
library(rasterVis)
library(RStoolbox)
# require(ggExtra)

#### Start Here ##############################################################

# Make figure for Crozier ####
# Read in and format Crozier data ####
# crozier boundary
croz_bound <- readOGR("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/croz/layers/croz_outer_bound.shp")

proj <-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
croz_bound_t <- spTransform(croz_bound,proj)
ext <- extent(croz_bound_t)+0.0002
# aspect
c_aspect <- raster("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/croz/layers/croz_aspect_corrected.tif")
c_aspect_t <- crop(projectRaster(c_aspect, crs=proj),ext)
c_aspect_spdf <- as(c_aspect_t, "SpatialPixelsDataFrame")
c_aspect_df <- as.data.frame(c_aspect_spdf)%>%
  rename(value=croz_aspect_corrected)%>%
  mutate(value=ifelse(value<0,NA,value))

# slope
c_slope <- raster("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/croz/layers/croz_slope.tif")
c_slope_t <- crop(projectRaster(c_slope, crs=proj),ext)
c_slope_df <- as.data.frame(as(c_slope_t, "SpatialPixelsDataFrame"))%>%
  rename(value=croz_slope)%>%
  mutate(value=ifelse(value<0,NA,value))

# elevation
elev <- raster("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/croz/layers/croz_dem_clip_windshelter_grid.asc")+47
elev_t <- crop(projectRaster(elev, crs=proj),ext)
elev_df <- as.data.frame(as(elev_t, "SpatialPixelsDataFrame"))%>%
  rename(value=croz_dem_clip_windshelter_grid)%>%
  mutate(value=ifelse(value<0,NA,value))

# flow accumulation
flow <- raster("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/croz/layers/croz_flowacc_snow_v2.tif")
flow_t <- crop(projectRaster(flow, crs=proj),ext)
flow_df <- as.data.frame(as(flow_t, "SpatialPixelsDataFrame"))%>%
  rename(value=croz_flowacc_snow_v2)%>%
  mutate(value=ifelse(value<0,NA,value),value=log1p(value))

# Windshelter
wind <- raster("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/croz/layers/croz_windshelter_300m_2pi_v2.asc")
wind_t <- crop(projectRaster(wind, crs=proj),ext)
wind_df <- as.data.frame(as(wind_t, "SpatialPixelsDataFrame"))%>%
  rename(value=croz_windshelter_300m_2pi_v2)%>%
  mutate(value=ifelse(value<0,NA,value))

# Skua
skua <-raster("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/croz/layers/skua50m_dist_rast_bin.tif")
skua_ext <- ext
skua_t <- crop(projectRaster(skua, crs=proj),skua_ext)
skua_df <- as.data.frame(as(skua_t, "SpatialPixelsDataFrame"))%>%
  rename(value= skua50m_dist_rast_bin)%>%
  mutate(value=ifelse(is.na(value),0,value))



# make Crozier plots ####
## colors 
# col.p <- c("#035aa1","#72ABD0","white","#FDBF71","#d40404") # 
col.p <- c("#006C84","#5EA8A7","#B2DBD5","white")

# col.p <- c("#205682","#72ABD0","white","#FDBF71","#F94943")

p.c_aspect<- ggplot() +  
  geom_raster(data=c_aspect_df, aes(x=x, y=y, fill=value), alpha=0.8)+
  geom_polygon(data=croz_bound_t, aes(x=long, y=lat, group=group),
  fill=NA, col="white",size=1) +
  scale_fill_gradientn(colors=col.p,limits =c(0,360), "Aspect (°)") +
  theme(legend.position=c("right")) +
  # theme(legend.key.width=unit(0.5, "cm"), legend.key.height = unit(2,"cm"))+
  theme(legend.key.width = unit(0.2, "cm"), legend.key.height = unit(0.35, "cm"))+
  labs(x="",y="")+
  theme(legend.position = c(0.93, 0.91),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = "grey50"),
        legend.text = element_text(colour="white"),
        legend.title = element_text(colour="white"),
        axis.text=element_blank(),
        axis.ticks = element_blank())
plot(p.c_aspect)


p.c_slope<- ggplot() +  
  geom_raster(data=c_slope_df, aes(x=x, y=y, fill=value), alpha=0.8)+
  geom_polygon(data=croz_bound_t, aes(x=long, y=lat, group=group),
               fill=NA, col="white",size=1) +
  scale_fill_gradientn(colors=col.p,limits =c(0,30), "Slope (°)") +
  theme(legend.position="right") +
  # theme(legend.key.width=unit(1, "cm"))+
  theme(legend.key.width = unit(0.2, "cm"), legend.key.height = unit(0.35, "cm"))+
  labs(x="",y="")+
  theme(legend.position = c(0.93, 0.91),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = "grey50"),
        legend.text = element_text(colour="white"),
        legend.title = element_text(colour="white"),
        axis.text=element_blank(),
        axis.ticks = element_blank())
# plot(p.c_slope)


p.elev<- ggplot() +  
  geom_raster(data=elev_df, aes(x=x, y=y, fill=value))+
  geom_polygon(data=croz_bound_t, aes(x=long, y=lat, group=group),
               fill=NA, col="white",size=1) +
  scale_fill_gradientn(colors=col.p,limits =c(0,225), "Elevation (m)") +
  theme(legend.position="right") +
  # theme(legend.key.width=unit(1, "cm"))+
  theme(legend.key.width = unit(0.2, "cm"), legend.key.height = unit(0.35, "cm"))+
  labs(x="",y="")+
  theme(legend.position = c(0.93, 0.91),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = "grey50"),
        legend.text = element_text(colour="white"),
        legend.title = element_text(colour="white"), axis.text=element_blank(),
        axis.ticks = element_blank())
# plot(p.elev)


p.flow<- ggplot() +  
  geom_raster(data=flow_df, aes(x=x, y=y, fill=value))+
  geom_polygon(data=croz_bound_t, aes(x=long, y=lat, group=group),
               fill=NA, col="white",size=1) +
  scale_fill_gradientn(colors=col.p,limits =c(0,12), "Flow (log)") +
  theme(legend.position="right") +
  # theme(legend.key.width=unit(1, "cm"))+
  theme(legend.key.width = unit(0.2, "cm"), legend.key.height = unit(0.35, "cm"))+
  labs(x="",y="")+
  theme(legend.position = c(0.93, 0.91),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = "grey50"),
        legend.text = element_text(colour="white"),
        legend.title = element_text(colour="white"),
        axis.text=element_blank(),
        axis.ticks = element_blank())
# plot(p.flow)

p.wind<- ggplot() +  
  geom_raster(data=wind_df, aes(x=x, y=y, fill=value))+
  geom_polygon(data=croz_bound_t, aes(x=long, y=lat, group=group),
               fill=NA, col="white",size=1) +
  scale_fill_gradientn(colors=col.p,limits =c(0,0.75),"Shelt. Index") +
  theme(legend.position="right") +
  # theme(legend.key.width=unit(1, "cm"))+
  theme(legend.key.width = unit(0.2, "cm"), legend.key.height = unit(0.35, "cm"))+
  labs(x="",y="")+
  theme(legend.position = c(0.93, 0.91),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = "grey50"),
        legend.text = element_text(colour="white"),
        legend.title = element_text(colour="white"),
        axis.text=element_blank(),
        axis.ticks = element_blank())
# plot(p.wind)


p.skua<- ggplot() +  
  geom_polygon(data=croz_bound_t, aes(x=long, y=lat, group=group),
               fill=NA, col="white",size=1)+
  geom_tile(data=skua_df, aes(x=x, y=y, fill=value))+
  # scale_fill_manual(col.p[1]) +
  theme(legend.position="right") +
  # theme(legend.key.width=unit(1, "cm"))
  theme(legend.key.width = unit(0.2, "cm"), legend.key.height = unit(0.35, "cm"))+
  labs(x="",y="", fill="Skua nests")+
  theme(legend.position = c(0.93, 0.91),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = "grey50"),
        legend.text = element_text(colour="white"),
        legend.title = element_text(colour="white"),
        axis.text=element_blank(),
        axis.ticks = element_blank())
plot(p.skua)

# Predicted subcol quality
# satellite image
croz_WV3 <- raster("V:/Project/Terrestrial/adpe/sat_images/2016jul21_croz_2014_20m_pansharpened/WV03_2014.tif")
croz_WV3 <- stack("V:/Project/Terrestrial/adpe/sat_images/2016jul21_croz_2014_20m_pansharpened/WV03_2014.tif")
wv_ext <- extent(elev)
croz_WV3_c <- crop(croz_WV3,crs=proj,wv_ext)
croz_WV3_t <- crop(projectRaster(croz_WV3_c,crs=proj),ext)
# croz_WV3_df <- as.data.frame(as(croz_WV3_t, "SpatialPixelsDataFrame"))

# Predicted subcol quality raster
subcol_qual <- raster("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/croz/layers/croz_predict_subcol.tif")
subcol_t <- crop(projectRaster(subcol_qual, crs=proj),ext)
subcol_df <- as.data.frame(as(subcol_t, "SpatialPixelsDataFrame"))%>%
  rename(value= croz_predict_subcol)

# Skua nests
skua_nest<- read.csv("Z:/Informatics/S031/analyses/aschmidt/subcol_var/data/croz_skua_nests_1617.txt")%>%
  filter(lat<(-77.448))
# snow fields
snow <-  readOGR("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/croz/layers/croz_2014_snowfields.shp")
snow_t <- crop(spTransform(snow, proj),ext)

# Turqoise to orange
col.subcol <- c("#006C84","#B2DBD5","#e6ceb5","#ff8324","#f73c2f")

p.subcol<-  ggplot() +
  ggRGB(croz_WV3_t,r=1,g=2,b=3,ggLayer=TRUE, maxpixels = 5e6)+
  geom_tile(data=skua_df, aes(x=x, y=y, fill=value), fill="white",alpha=0.3)+
  geom_tile(data=subcol_df, aes(x=x, y=y, fill=value))+
  # geom_polygon(data=snow_t,aes(x=long, y=lat, group=group, fill="Snow"), colour  = 'black',fill=NA,size=0.75)+
  geom_polygon(data=croz_bound_t, aes(x=long, y=lat, group=group),
               fill=NA, col="white",size=1)+
  scale_fill_gradientn(colors=col.subcol,limits =c(-0.12,0.12),"Subcolony/n quality")+
  geom_point(data=skua_nest,aes(x=lon,y=lat, color="#ffdd00"), size=3)+
  scale_color_identity(name = "Skua nests",labels="",guide="legend")+
  labs(x="Longitude",y="Latitude")+
  theme(legend.key.size = unit(1, "cm"),
    legend.position = c(0.93, 0.97),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "grey50"),
    legend.text = element_text(colour="white"),
    legend.title = element_text(colour="white"),
    # legend.box.background = element_rect(fill="grey50"),
    legend.key=element_rect(fill="grey"),
    axis.title=element_text(size=14),
    axis.text=element_text(size=12))+
    scale_x_continuous(expand=c(0.0003,0.0005))+
    scale_y_continuous(expand=c(0.0001,0.0001))


plot(p.subcol)


### put everything together
cairo_pdf(file=paste("croz_covariate_map_fig", format(Sys.time(), "%Y-%m-%d"), "pdf", sep = "."),width=12,height=12)

grid.arrange(p.c_aspect,p.c_slope,p.elev,p.flow,p.wind,p.subcol,
             ncol = 3, nrow = 4, 
             layout_matrix = rbind(c(1,2,3), c(4,6,6),c(5,6,6)))
dev.off()


jpeg(file=paste("croz_covariates", "jpeg", sep = "."),units="in",width=12,height=14, res=500)
grid.arrange(p.c_aspect,p.c_slope,p.elev,p.flow,p.wind,p.subcol,
             ncol = 3, nrow = 4, 
             layout_matrix = rbind(c(1,2,3), c(4,6,6),c(5,6,6)))
dev.off()


# Royds Figure ####
# Read in and format Royds data ####
# Royds boundary
royds_bound <- readOGR("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/royds/layers/royds_outer_bound_rev2.shp")
# set projection
proj <-"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
royds_bound_t <- spTransform(royds_bound,proj)
# save extent to use for other layers
r_ext <- extent(royds_bound_t)


# aspect
r_aspect <- raster("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/royds/rev2/royds_aspect_corrected.tif")
r_aspect_t <- crop(projectRaster(aspect, crs=proj),r_ext)
r_aspect_spdf <- as(r_aspect_t, "SpatialPixelsDataFrame")
r_aspect_df <- as.data.frame(r_aspect_spdf)%>%
  rename(value=royds_aspect_corrected)%>%
  mutate(value=ifelse(value<0,NA,value))

# slope
r_slope <- raster("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/royds/rev2/royds_slope.tif")
r_slope_t <- crop(projectRaster(r_slope, crs=proj),r_ext)
r_slope_df <- as.data.frame(as(r_slope_t, "SpatialPixelsDataFrame"))%>%
  rename(value=royds_slope)%>%
  mutate(value=ifelse(value<0,NA,value))

# elevation
r_elev <- raster("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/royds/layers/royds_mosaic_dem-tile-0_clip.tif")+46
r_elev_t <- crop(projectRaster(r_elev, crs=proj),r_ext)
r_elev_df <- as.data.frame(as(r_elev_t, "SpatialPixelsDataFrame"))%>%
  rename(value=royds_mosaic_dem.tile.0_clip)%>%
  mutate(value=ifelse(value<0,0,value))

# flow accumulation
r_flow <- raster("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/royds/rev2/royds_flow_acc_snow_rev2.tif")
r_flow_t <- crop(projectRaster(r_flow, crs=proj),r_ext)
r_flow_df <- as.data.frame(as(r_flow_t, "SpatialPixelsDataFrame"))%>%
  rename(value=royds_flow_acc_snow_rev2)%>%
  mutate(value=ifelse(value<0,NA,value),value=log1p(value))

# Windshelter
wind <- raster("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/croz/layers/croz_windshelter_300m_2pi_v2.asc")
wind_t <- crop(projectRaster(wind, crs=proj),ext)
wind_df <- as.data.frame(as(wind_t, "SpatialPixelsDataFrame"))%>%
  rename(value=croz_windshelter_300m_2pi_v2)%>%
  mutate(value=ifelse(value<0,NA,value))

# Skua
r_skua <-raster("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/royds/rev2/royds_skua_50m_rast.tif")
r_skua_ext <- extent(r_skua)
r_skua_t <- crop(projectRaster(r_skua, crs=proj),r_skua_ext)
r_skua_df <- as.data.frame(as(r_skua_t, "SpatialPixelsDataFrame"))%>%
  rename(value= royds_skua_50m_rast)%>%
  mutate(value=ifelse(value>50,NA,1))



# Make Royds plots ####
## colors 
# col.p <- c("#035aa1","#72ABD0","white","#FDBF71","#d40404") # 
col.p <- c("#006C84","#5EA8A7","#B2DBD5","white")

# col.p <- c("#205682","#72ABD0","white","#FDBF71","#F94943")

p.r_aspect<- ggplot() +  
  geom_raster(data=r_aspect_df, aes(x=x, y=y, fill=value), alpha=0.8)+
  geom_polygon(data=royds_bound_t, aes(x=long, y=lat, group=group),
               fill=NA, col="white",size=1) +
  scale_fill_gradientn(colors=col.p,limits =c(0,360), "Aspect (°)") +
  theme(legend.position=c("right")) +
  # theme(legend.key.width=unit(0.5, "cm"), legend.key.height = unit(2,"cm"))+
  theme(legend.key.width = unit(0.2, "cm"), legend.key.height = unit(0.35, "cm"))+
  labs(x="",y="")+
  theme(legend.position = c(0.93, 0.91),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = "grey50"),
        legend.text = element_text(colour="white"),
        legend.title = element_text(colour="white"),
        axis.text=element_blank(),
        axis.ticks = element_blank())
plot(p.r_aspect)


p.r_slope<- ggplot() +  
  geom_raster(data=r_slope_df, aes(x=x, y=y, fill=value), alpha=0.8)+
  geom_polygon(data=royds_bound_t, aes(x=long, y=lat, group=group),
               fill=NA, col="white",size=1) +
  scale_fill_gradientn(colors=col.p,limits =c(0,45), "Slope (°)") +
  theme(legend.position="right") +
  # theme(legend.key.width=unit(1, "cm"))+
  theme(legend.key.width = unit(0.2, "cm"), legend.key.height = unit(0.35, "cm"))+
  labs(x="",y="")+
  theme(legend.position = c(0.93, 0.91),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = "grey50"),
        legend.text = element_text(colour="white"),
        legend.title = element_text(colour="white"),
        axis.text=element_blank(),
        axis.ticks = element_blank())
plot(p.r_slope)


p.r_elev<- ggplot() +  
  geom_raster(data=r_elev_df, aes(x=x, y=y, fill=value))+
  geom_polygon(data=royds_bound_t, aes(x=long, y=lat, group=group),
               fill=NA, col="white",size=1) +
  scale_fill_gradientn(colors=col.p,limits =c(0,25), "Elevation (m)") +
  theme(legend.position="right") +
  # theme(legend.key.width=unit(1, "cm"))+
  theme(legend.key.width = unit(0.2, "cm"), legend.key.height = unit(0.35, "cm"))+
  labs(x="",y="")+
  theme(legend.position = c(0.93, 0.91),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = "grey50"),
        legend.text = element_text(colour="white"),
        legend.title = element_text(colour="white"), axis.text=element_blank(),
        axis.ticks = element_blank())
plot(p.r_elev)


p.r_flow<- ggplot() +  
  geom_raster(data=r_flow_df, aes(x=x, y=y, fill=value))+
  geom_polygon(data=royds_bound_t, aes(x=long, y=lat, group=group),
               fill=NA, col="white",size=1) +
  scale_fill_gradientn(colors=col.p,limits =c(0,7), "Flow (log)") +
  theme(legend.position="right") +
  # theme(legend.key.width=unit(1, "cm"))+
  theme(legend.key.width = unit(0.2, "cm"), legend.key.height = unit(0.35, "cm"))+
  labs(x="",y="")+
  theme(legend.position = c(0.93, 0.91),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = "grey50"),
        legend.text = element_text(colour="white"),
        legend.title = element_text(colour="white"),
        axis.text=element_blank(),
        axis.ticks = element_blank())
plot(p.r_flow)

p.r_wind<- ggplot() +  
  geom_raster(data=wind_df, aes(x=x, y=y, fill=value))+
  geom_polygon(data=croz_bound_t, aes(x=long, y=lat, group=group),
               fill=NA, col="white",size=1) +
  scale_fill_gradientn(colors=col.p,limits =c(0,0.75),"Shelt. Index") +
  theme(legend.position="right") +
  # theme(legend.key.width=unit(1, "cm"))+
  theme(legend.key.width = unit(0.2, "cm"), legend.key.height = unit(0.35, "cm"))+
  labs(x="",y="")+
  theme(legend.position = c(0.93, 0.91),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = "grey50"),
        legend.text = element_text(colour="white"),
        legend.title = element_text(colour="white"),
        axis.text=element_blank(),
        axis.ticks = element_blank())
# plot(p.wind)


p.r_skua<- ggplot() +  
  geom_polygon(data=royds_bound_t, aes(x=long, y=lat, group=group),
               fill=NA, col="white",size=1)+
  geom_tile(data=r_skua_df, aes(x=x, y=y, fill=value))+
  # scale_fill_manual(col.p[1]) +
  theme(legend.position="right") +
  # theme(legend.key.width=unit(1, "cm"))
  theme(legend.key.width = unit(0.2, "cm"), legend.key.height = unit(0.35, "cm"))+
  labs(x="",y="", fill="Skua nests")+
  theme(legend.position = c(0.93, 0.91),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = "grey50"),
        legend.text = element_text(colour="white"),
        legend.title = element_text(colour="white"),
        axis.text=element_blank(),
        axis.ticks = element_blank())
plot(p.r_skua)

# Predicted subcol quality
# satellite image
croz_WV3 <- raster("V:/Project/Terrestrial/adpe/sat_images/2016jul21_croz_2014_20m_pansharpened/WV03_2014.tif")
croz_WV3 <- stack("V:/Project/Terrestrial/adpe/sat_images/2016jul21_croz_2014_20m_pansharpened/WV03_2014.tif")
wv_ext <- extent(elev)
croz_WV3_c <- crop(croz_WV3,crs=proj,wv_ext)
croz_WV3_t <- crop(projectRaster(croz_WV3_c,crs=proj),ext)
# croz_WV3_df <- as.data.frame(as(croz_WV3_t, "SpatialPixelsDataFrame"))

# Predicted subcol quality raster
subcol_qual <- raster("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/croz/layers/croz_predict_subcol.tif")
subcol_t <- crop(projectRaster(subcol_qual, crs=proj),ext)
subcol_df <- as.data.frame(as(subcol_t, "SpatialPixelsDataFrame"))%>%
  rename(value= croz_predict_subcol)

# Skua nests
skua_nest<- read.csv("Z:/Informatics/S031/analyses/aschmidt/subcol_var/data/croz_skua_nests_1617.txt")%>%
  filter(lat<(-77.448))
# snow fields
snow <-  readOGR("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/croz/layers/croz_2014_snowfields.shp")
snow_t <- crop(spTransform(snow, proj),ext)

# Turqoise to orange
col.subcol <- c("#006C84","#B2DBD5","#e6ceb5","#ff8324","#f73c2f")

p.subcol<-  ggplot() +
  ggRGB(croz_WV3_t,r=1,g=2,b=3,ggLayer=TRUE, maxpixels = 5e6)+
  geom_tile(data=skua_df, aes(x=x, y=y, fill=value), fill="white",alpha=0.3)+
  geom_tile(data=subcol_df, aes(x=x, y=y, fill=value))+
  # geom_polygon(data=snow_t,aes(x=long, y=lat, group=group, fill="Snow"), colour  = 'black',fill=NA,size=0.75)+
  geom_polygon(data=croz_bound_t, aes(x=long, y=lat, group=group),
               fill=NA, col="white",size=1)+
  scale_fill_gradientn(colors=col.subcol,limits =c(-0.12,0.12),"Subcolony/n quality")+
  geom_point(data=skua_nest,aes(x=lon,y=lat, color="#ffdd00"), size=3)+
  scale_color_identity(name = "Skua nests",labels="",guide="legend")+
  labs(x="Longitude",y="Latitude")+
  theme(legend.key.size = unit(1, "cm"),
        legend.position = c(0.93, 0.97),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = "grey50"),
        legend.text = element_text(colour="white"),
        legend.title = element_text(colour="white"),
        # legend.box.background = element_rect(fill="grey50"),
        legend.key=element_rect(fill="grey"),
        axis.title=element_text(size=14),
        axis.text=element_text(size=12))+
  scale_x_continuous(expand=c(0.0003,0.0005))+
  scale_y_continuous(expand=c(0.0001,0.0001))


plot(p.subcol)