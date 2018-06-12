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



# correct crozier aspect
# read in aspect raster
library(raster)
library(maptools)
croz_aspect_raw <-raster("V:/Project/Terrestrial/adpe/aschmidt/crozier_GIS_layers/croz_aspect.tif")
# get lower left coordinates from raster
c_xll <- croz_aspect_raw@extent@xmin
c_yll <- croz_aspect_raw@extent@ymin
# Convert to matrix
c_data <- raster::as.matrix(croz_aspect_raw, mode="numeric")
# replace NA with -9999
c_data[is.na(c_data)]<- -9999
range(c_data[100,])

c_aspect_correct <- trueaspect(c_data,dx=2,xll=c_xll,yll=c_yll)
# check that distribution looks right
hist(c_aspect_correct[!c_aspect_correct==-9999])
# replace -9999 with NA
c_aspect_correct[c_aspect_correct==-9999]<- NA


# #read-in the subcolony raster (zones)c
# c_subcol<- raster("V:/Project/Terrestrial/adpe/aschmidt/crozier_GIS_layers/croz_selected_subcol_2014_2.2rast.tif")
# 
# #create a raster stack
# s <- stack(c_subcol, croz_aspect_raw)
# 
# #extract raster cell count (sum) within each polygon area (poly)
# for (i in 1:length(grids)){
#   ex <- extract(s, poly, fun=sum, na.rm=TRUE, df=TRUE)
# }
# 
# #write to a data frame
# df <- data.frame(ex)

# #write to a CSV file
# write.csv(df, file = "./path/to/data/CSV.csv")


hist(c_aspect_test)
range(c_aspect_test, na.rm=TRUE)
# save as raster
c_aspect_corr_rast <- raster(c_aspect_correct,xmn=croz_aspect_raw@extent@xmin,
                             xmx=croz_aspect_raw@extent@xmax,
                             ymn=croz_aspect_raw@extent@ymin,
                             ymx=croz_aspect_raw@extent@ymax, crs=croz_aspect_raw@crs)
c_aspect_corr_rast@file@nodatavalue<- -9999
c_aspect_corr_rast@data@min<- 0

writeRaster(c_aspect_corr_rast,"V:/Project/Terrestrial/adpe/aschmidt/crozier_GIS_layers/croz_aspect_corrected.tif", overwrite=TRUE)


# trying different approach
# use R to calculate aspect
# read in dem
c_dem <- raster("V:/Project/Terrestrial/adpe/aschmidt/crozier_GIS_layers/crozier_mosaic_dem-tile-0_clip.tif")
c_aspect<- terrain(c_dem,opt='aspect',unit="degrees",neighbors = 8)
subcol_aspect_extract <-read.csv("V:/Project/Terrestrial/adpe/aschmidt/crozier_GIS_layers/croz_selected_aspect_corrected_extract.txt", header=TRUE)

library(dplyr)
library(circular)

subcol_aspect_extract$cos <- cos(subcol_aspect_extract$VALUE*pi/180)
subcol_aspect_extract$sin <- sin(subcol_aspect_extract$VALUE*pi/180)
subcol_aspect_extract$cos <- cos(subcol_aspect_extract$VALUE*pi/180)
circular(angles, type="angles", units="degrees",modulo="2pi", template='geographics')

subcol_aspect_summ <- subcol_aspect_extract%>%
  mutate(circ=circular(VALUE, type="angles", units="degrees",modulo="2pi", template='geographics'))%>%
  group_by(SRCID_FEAT)%>%
  summarize(mean_aspect=mean(circ))


hist(as.numeric(subcol_aspect_summ$mean_aspect[subcol_aspect_summ$mean_aspect>0]))
  summarize(mean_cos=mean(cos), mean_sin=mean(sin))%>%
  
  
  mutate(mean_aspect=(360+atan2(mean_sin,mean_cos))*(180/pi))
