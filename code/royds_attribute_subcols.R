# extract spatial covariates for subcolonies ####

# libraries
library(raster)
library(sp)
library(rgdal)
library(dplyr)

# read in raster layers and extract average values within subcolony boundaries
# read in subcol polygon layer
r_subcol <-readOGR("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/royds/rev2/royds_subcol_2014.shp")

# elevation
r_elev <-raster("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/royds/layers/royds_mosaic_dem-tile-0.tif")
# slope
# aspect
# flow
# windshelter
r_wind <- raster("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/royds/rev2/royds_windshelter300m_pi_pi8_lcc169_clip.tif")
# skua dist

# extract values
r_subcol_attr <- extract(r_wind,r_subcol,fun=mean,sp=TRUE)

names(r_subcol_attr@data)

r_subcol_attr<- extract(r_elevation,)
