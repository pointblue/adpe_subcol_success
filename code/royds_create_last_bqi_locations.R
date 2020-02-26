# need to create a table with last BQI value for each bandnumb
# library(foreign) # for reading and writing .dbf files
library(dplyr)
library(readr)
library(ggplot2)
library(raster)
library(sf)
library(nngeo)

# bqi <- read.dbf("Z:/Informatics/S031/analyses/aschmidt/subcol_var/data/bqi_081516.dbf")
r_bqi<- read.csv("Z:/Informatics/S031/analyses/BQI/Royds_BQI_inclcurseas_1994to2017_forAnnie.csv",header=TRUE)

# plot bqi scores by breeding exp
ggplot(r_bqi,aes(Br_exp,BQI_A,group=Bandnumb))+
  geom_line(aes(colour=Bandnumb))

# check out distribution of BQI scores
r_bqi%>%
  group_by(Bandnumb)%>%
  arrange(Season)%>%
  slice(n())%>%
  ggplot(aes(BQI_A))+
  geom_histogram()
  


# Read in resigt data to get coordinates where bred
allresight_raw <- read.csv("Z:/Informatics/S031/S0311920/croz1920/bandsearch/allresight_1819.csv", as.is=TRUE)%>%
  mutate(bandnumb=as.numeric(bandnumb),lat=as.numeric(lat))


# Filter to lines that have gps location
allrs_latlon <- 
  allresight_raw%>%
  mutate(lat=as.numeric(lat), Bandnumb=as.numeric(bandnumb))%>%
  mutate(Season=as.numeric(plyr::mapvalues(season,from=c("9596","9899","9900","0001",
                                              "0102","0203","0304","0405","0506","0607","0708",
                                              "0809","0910","1011","1112","1213","1314","1415",
                                              "1516","1617","1718"),
                                to = c(1995,1998:2017))))%>%
  dplyr::select(Bandnumb,Season,colony, orig_col,lat, lon)%>%
  filter(!lat==0, !lon==0, colony%in%c("ROY","ROYD","ROYDS"),lat<-77.54)%>%
  group_by(Bandnumb,Season)%>%
  dplyr::select(colony,Bandnumb,Season,lat,lon)%>%
  # keep only one record per band per season
  dplyr::slice(n())

# replace positive lat with negative
allrs_latlon$lat[allrs_latlon$lat>0]=-allrs_latlon$lat[allrs_latlon$lat>0]
# replace lat>-70 with NA
allrs_latlon$lat[allrs_latlon$lat>-77.4]=NA
allrs_latlon$lat[allrs_latlon$lat<=(-600)]=NA
# allrs_latlon$lat[allrs_latlon$lat>-70]=NA
plot(allrs_latlon$lat)

# join lat lon to bqi table
bqi_loc <- 
  inner_join(r_bqi,allrs_latlon, by=c("Bandnumb","Season"))

# select individuals that meet criteria for study
# filter to birds that bred at least once in last 4 years and bred at least 3 times overall and 
# not in GDR deploy list
gdr_bands <- read.csv("Z:/Informatics/S031/analyses/GDR/data/cr_gdr_bands.csv")

bqi_crit <- bqi_loc%>%
  filter(Season%in%c(2014:2017),Br_exp>2,!Result=="NB",!Bandnumb%in%gdr_bands$band)%>%
  group_by(Bandnumb)%>%
  arrange(Season)%>%
  slice(n())

# read in predicted productivity raster
pred_prod<-raster("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/royds/layers/royds_predict_prod_base_anom_v2.tif")
# convert raster to points
pred_pt <- st_as_sf(rasterToPoints(pred_prod, spatial=TRUE))%>%
  rename(pred_prod=royds_predict_prod_base_anom_v2)
plot(pred_pt)
# reproject to same a bqi locations
pred_pt <- st_transform(pred_pt,crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# convert bqi coords to spatial points data frame
bqi_sp <- bqi_crit%>%
  ungroup()%>%
  dplyr::select(Bandnumb,lat,lon,BQI_A)%>%
  st_as_sf(coords=c("lon","lat"), remove=FALSE)

st_crs(bqi_sp)<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"


# use spatial join to join predicted productivity to bqi
bqi_prod <- st_join(bqi_sp,pred_pt["pred_prod"], 
                    join=st_nn, maxdist=5)%>%
  as.data.frame(.)%>%
  dplyr::select(-geometry)

ggplot(bqi_prod,aes(pred_prod,BQI_A))+
  geom_point()+
  xlab("Predicted habitat quality")+
  ylab("BQI")+
  geom_smooth(method = "lm")

write.csv(bqi_prod, "Z:/Informatics/S031/analyses/aschmidt/subcol_var/data/royds_bqi_pred_prod_loc.csv", row.names = FALSE)


