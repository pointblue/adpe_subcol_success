library(RSAGA)
library(raster)

# code that was run on aws server 5/28/20

setwd("~/GIS/royds")

# first read in dem and adjust
dem <- raster("royds_windshelter_reproject_dem_lcc169.asc")+54

summary(dem)
writeRaster(dem,"royds_windshelter_reproject_dem_lcc169_adjust.asc",format="ascii", overwrite=TRUE)

# re calculating windshelter from South but using adjusted dem
ctrl <- wind.shelter.prep(radius=300,direction=pi,tolerance=pi/8,cellsize=2)
wind_shelt <- focal.function("royds_windshelter_reproject_dem_lcc169_adjust.asc",fun=wind.shelter,control=ctrl,
                             radius=300,search.mode="circle")
