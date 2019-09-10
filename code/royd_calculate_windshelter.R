library(RSAGA)
library(raster)

setwd("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/royds/layers")
# Initially set direction to SSW with tolerance from S to SW
# Redoing this after David pointed out wind direction at Royds is more Southeast
# Setting direction to SSE with tolerance from S to SE
# also using citations Monaghan et al. 2005 and Seefeldt et al. 2003 to support this
# previously had used same at Croz (direction pi/8)
# first try with 337.5 didn't look right. Trying again with 15pi/8
# ctrl <- wind.shelter.prep(radius=300,direction=14*pi/8,tolerance=pi/8,cellsize=2) # v2
ctrl <- wind.shelter.prep(radius=300,direction=2*pi,tolerance=pi/8,cellsize=2)
wind_shelt <- focal.function("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/royds/layers/royds_dem_windshelter_grid3.asc",fun=wind.shelter,control=ctrl,
                             radius=300,search.mode="circle")

# royds_windshelt_300m_2pi_pi4_v1
ctrl <- wind.shelter.prep(radius=300,direction=2*pi,tolerance=pi/4,cellsize=2)
wind_shelt <- focal.function("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/royds/layers/royds_dem_windshelter_grid3.asc",fun=wind.shelter,control=ctrl,
                             radius=300,search.mode="circle")

!all(is.na(x))
(x - ctr)/control$dist
ctr=centervalue(x)


wind.shelter2 <- function (x, prob = NULL, control){}
if (missing(x)) 
  return("windshelter")
if (missing(control)) 
  stop("need 'control' argument - call 'wind.shelter.prep' first")
ctr = centervalue(x)
x[control$mask] = NA
res = NA
res = atan(max((x - ctr)/control$dist, na.rm=TRUE))
# if (is.null(prob)) {
#   res = max(x, na.rm = TRUE)
# }
# else res = stats::quantile(x, probs = prob, na.rm = TRUE)
return(res)
}

