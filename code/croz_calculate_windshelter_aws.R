library(RSAGA)
library(raster)

# code that was run on AWS (should be final version that was used in ms)

setwd("~/GIS/crozier")

# first read in dem and adjust
dem <- raster("croz_windshelter_dem_clip_grid_lcc169.asc")+47

summary(dem)
writeRaster(dem,"croz_windshelter_dem_clip_grid_lcc169_adjust.asc",format="ascii", overwrite=TRUE)

ctrl <- wind.shelter.prep(radius=300,direction=pi,tolerance=pi/8,cellsize=2)
wind_shelt <- focal.function("croz_windshelter_dem_clip_grid_lcc169_adjust.asc",fun=wind.shelter,control=ctrl,
                             radius=300,search.mode="circle")
