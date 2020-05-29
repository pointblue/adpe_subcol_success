library(RSAGA)
library(raster)


# Initially set direction to SSW with tolerance from S to SW
# Redoing this after David pointed out wind direction at Royds is more Southeast
# Setting direction to SSE with tolerance from S to SE
# also using citations Monaghan et al. 2005 and Seefeldt et al. 2003 to support this
# previously had used same at Croz (direction pi/8)
# first try with 337.5 didn't look right. Trying again with 15pi/8
# ctrl <- wind.shelter.prep(radius=300,direction=14*pi/8,tolerance=pi/8,cellsize=2) # v2
# # ctrl <- wind.shelter.prep(radius=300,direction=2*pi,tolerance=pi/8,cellsize=2)
# wind_shelt <- focal.function("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/royds/layers/royds_dem_windshelter_grid3.asc",fun=wind.shelter,control=ctrl,
#                              radius=300,search.mode="circle")


# Note: Direction is set to 2pi which should be north, but because grid is in polar stereo, South is up so 2pi=South
# royds_windshelt_300m_2pi_pi4
# Reprojected dem to lanber 169 so direction=0 should be north now??
# ctrl <- wind.shelter.prep(radius=300,direction=2*pi,tolerance=pi/4,cellsize=2)
# wind_shelt <- focal.function("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/royds/layers/royds_dem_windshelter_grid3.asc",fun=wind.shelter,control=ctrl,
#                              radius=300,search.mode="circle")
setwd("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/royds/rev2")
ctrl <- wind.shelter.prep(radius=300,direction=pi,tolerance=pi/8,cellsize=2)
wind_shelt <- focal.function("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/royds/rev2/royds_windshelter_reproject_dem_lcc169.asc",fun=wind.shelter,control=ctrl,
                             radius=300,search.mode="circle")


# calculating windshelter with wind from SE
ctrl <- wind.shelter.prep(radius=300,direction=3*pi/4,tolerance=pi/8,cellsize=2)
wind_shelt <- focal.function("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/royds/rev2/royds_windshelter_reproject_dem_lcc169.asc",fun=wind.shelter,control=ctrl,
                             radius=300,search.mode="circle")
