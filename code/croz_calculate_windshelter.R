
library(RSAGA)
library(raster)

# Note: Direction is set to 2pi which should be north, but because grid is in polar stereo, South is up so 2pi=South
# croz_windshelt_300m_2pi but with p/4 tolerance (45 degrees) so wind SE to SW
setwd("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/croz/rev2/")
ctrl <- wind.shelter.prep(radius=300,direction=pi,tolerance=pi/8,cellsize=2)
wind_shelt <- focal.function("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/croz/rev2/croz_windshelter_dem_grid_lcc169.asc",fun=wind.shelter,control=ctrl,
                             radius=300,search.mode="circle")

