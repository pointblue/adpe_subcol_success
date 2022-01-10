
library(dplyr)
library(circular)
library(raster)
library(maptools)

# aspect correction function modified from matlab function by 
  # Antarctic Science 19 (1), 129-130 (2007) © Antarctic Science Ltd Printed in the UK DOI: 10.1017/S0954102007000181
  # 129
  # Short Note
  # Correcting GIS-based slope aspect calculations for the Polar Regions
  # GEOFF J.M. MORET and AUDREY D. HUERTA
trueaspect= function(data,dx,xll,yll){
  #TRUEASPECT corrects slope aspect data so that angles are relative to
  #geographic north instead of grid north. 
  # The inputs are: 
  # data: a matrix of slope aspect data, with NoData cells set to -9999 and cells without an aspect set to -1 
  # dx: the cell size 
  # xll: The x-coordinate of the lower left hand grid cell. This should be in polar stereographic projection, with false easting removed.
  # yll: The y-coordinate of the lower left hand grid cell. This should be in a polar stereographic projection, with false northing removed.
  # Original function included a variable hemi (Use hemi = 1 for the south pole and hemi = 2 for the north pole.) which I removed 
  # Also removed range (If range = 1, the output values will be between 0 and 360 degrees) because only want in 0-360
  # If range has any other value, they will be between -180 and +180 degrees.
  nrows=nrow(data)
  ncols=ncol(data)
  for(m in 1:nrows){
    for(n in 1:ncols){
      ifelse(data[m,n]==-1,9999,data[m,n])
    }
  }
  x=xll+c(0:(ncols-1))*dx
  y=yll+c(0:(nrows-1))*dx
  newdata=matrix(0,nrows,ncols)
  for (i in 1:nrows){
    for(j in 1:ncols){
        lon=atan2(x[j],y[i])*180/pi
        newdata[i,j]=data[i,j]-lon
    }
  }

  newdata[newdata< -1000]<- -9999
  newdata[newdata<0&newdata> -9999]<- newdata[newdata<0&newdata> -9999]+360
  newdata[newdata>1000] <- -1
  newdata[newdata>360] <- newdata[newdata>360]-360
  newdata
}


# code below edited for Sci Rep revision 2:

# read in 
royds_aspect_raw <-raster("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/royds/rev2/royds_aspect.tif")
# get lower left coordinates from raster
r_xll <- royds_aspect_raw@extent@xmin
r_yll <- royds_aspect_raw@extent@ymin
# Convert to matrix
r_data <- raster::as.matrix(royds_aspect_raw, mode="numeric")
# replace NA with -9999
r_data[is.na(r_data)]<- -9999

# calculate corrected aspect
r_aspect_correct <- trueaspect(r_data,dx=2,xll=r_xll,yll=r_yll)
# check that distribution looks right
hist(r_aspect_correct[!r_aspect_correct==-9999])
# replace -9999 with NA (not sure this step is meaningful)
r_aspect_correct[r_aspect_correct==-9999]<- NA

# convert to raster
r_aspect_corr_rast <- raster(r_aspect_correct,xmn=r_xll,
                             xmx=royds_aspect_raw@extent@xmax,
                             ymn=r_yll,
                             ymx=royds_aspect_raw@extent@ymax, crs=royds_aspect_raw@crs)
r_aspect_corr_rast@file@nodatavalue<- -9999
r_aspect_corr_rast@data@min<- 0

# save raster
writeRaster(r_aspect_corr_rast,"Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/royds/rev2/royds_aspect_corrected.tif", overwrite=TRUE)
