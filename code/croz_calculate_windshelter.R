library(RSAGA)
library(raster)
# setting direction to SSW with tolerance from S to SW (pi/8 for both)
# re-try with direction = S (2*pi) and tolerance pi/8
setwd("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/croz/layers")
ctrl <- wind.shelter.prep(radius=300,direction=2*pi,tolerance=pi/8,cellsize=2)
wind_shelt <- focal.function("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/croz/layers/croz_dem_clip_windshelter_grid.asc",fun=wind.shelter,control=ctrl,
               radius=300,search.mode="circle")


# trying again with pi/4 instead (so wind centered on SW) and pi/12 tolerance
setwd("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/croz/layers/")
ctrl <- wind.shelter.prep(radius=100,direction=pi/4,tolerance=pi/12,cellsize=2)
wind_shelt <- focal.function("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/croz/layers/croz_dem_clip_windshelter_grid.asc",fun=wind.shelter,control=ctrl,
                             radius=100,search.mode="circle")

# croz_windshelt_300m_2pi but with p/4 tolerance (45 degrees) so wind SE to SW
setwd("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/croz/layers/")
ctrl <- wind.shelter.prep(radius=100,direction=2*pi,tolerance=pi/4,cellsize=2)
wind_shelt <- focal.function("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/croz/layers/croz_dem_clip_windshelter_grid.asc",fun=wind.shelter,control=ctrl,
                             radius=100,search.mode="circle")


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

