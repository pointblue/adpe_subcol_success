#### chunk of code extracted from subcol paper for future referecne
#### incomplete, will not run on its own
#### see subcol_var_analysis.rmd for rest of relevant code


# BQI correlation
# Correlate predicted prod/habitat quality with individual quality ####
# need to create a table with last BQI value for each bandnumb
c_bqi<- read.csv("Z:/Informatics/S031/analyses/BQI/BQI_inclcurseas_1998to2017_forAnnie.csv", header=TRUE)%>%
  mutate(season_yr=Season,season = plyr::mapvalues(Season, 
                                                   from = factor(c(1998:2017)),
                                                   to = c("9899","9900","0001","0102","0203","0304","0405",
                                                          "0506","0607","0708","0809","0910","1011","1112",
                                                          "1213","1314","1415","1516","1617","1718")))

ggplot(c_bqi,aes(Br_exp,BQI_A,group=Bandnumb))+
  geom_line(aes(colour=Bandnumb))


# Read in resight data to get coordinates where bred
allresight_raw <- read.csv("Z:/Informatics/S031/S0311920/croz1920/bandsearch/allresight_1819.csv", as.is=TRUE)%>%
  mutate(bandnumb=as.numeric(bandnumb),lat=as.numeric(lat))

# Filter to lines that have gps location
allrs_latlon <- 
  allresight_raw%>%
  # full_join(rs17)%>%
  mutate(lat=as.numeric(lat), Bandnumb=as.numeric(bandnumb))%>%
  dplyr::select(Bandnumb,season,colony, orig_col,lat, lon)%>%
  filter(!lat==0, !lon==0)%>%
  group_by(Bandnumb,season)%>%
  dplyr::select(Bandnumb,season,lat,lon)%>%
  # keep only one record per band per season
  dplyr::slice(1)

# replace positive lat with negative
allrs_latlon$lat[allrs_latlon$lat>0]=-allrs_latlon$lat[allrs_latlon$lat>0]
# replace lat>-70 with NA
allrs_latlon$lat[allrs_latlon$lat>-77.3|allrs_latlon$lat<=(-600)]=NA
allrs_latlon$lat[allrs_latlon$lat<=(-600)]=NA
# allrs_latlon$lat[allrs_latlon$lat>-70]=NA
allrs_latlon$lon[allrs_latlon$lon>300]=NA
plot(allrs_latlon$lat)

c_bqi_loc <- 
  inner_join(c_bqi,allrs_latlon, by=c("Bandnumb","season"))%>%
  #   arrange(Bandnumb))
  # group_by(Bandnumb)%>%
  # dplyr::slice(n())%>%
  # ungroup%>%
  filter(!lat<(-78.0),!lat==1,!lat>-77.3,lon>168.0,lon<169.9)

plot(c_bqi_loc$lon,c_bqi_loc$lat)


# select individuals that meet criteria for study
# filter to birds that bred at least once in last 4 years and bred at least 3 times overall and 
# not in GDR deploy list
gdr_bands <- read.csv("Z:/Informatics/S031/analyses/GDR/data/cr_gdr_bands.csv")

c_bqi_crit <- c_bqi_loc%>%
  filter(Season%in%c(2014:2017),Br_exp>2,!Result=="NB",!Bandnumb%in%gdr_bands$band)%>%
  group_by(Bandnumb)%>%
  arrange(Season)%>%
  slice(n())

# read in predicted productivity raster
c_pred_prod<-raster("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/croz/layers/croz_predict_subcol.tif")

# convert raster to points
c_pred_pt <- st_as_sf(rasterToPoints(pred_prod, spatial=TRUE))%>%
  rename(pred_prod=croz_predict_subcol)
plot(pred_pt, cex=.2)
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
  xlab("Predicted subcolony quality")+
  ylab("BQI")+
  geom_smooth(method = "lm")

Hmisc::rcorr(bqi_prod$BQI_A,bqi_prod$pred_prod)


write.csv(bqi_prod, "Z:/Informatics/S031/analyses/aschmidt/subcol_var/data/croz_bqi_pred_subcol_loc.csv", row.names = FALSE)



# read in table with Crozier  bqi and nearest predicted prod
c_bqi_pred_prod <- read.csv("Z:/Informatics/S031/analyses/aschmidt/subcol_var/data/croz_bqi_all_pred_prod_v5.csv")%>%
  filter(!GRID_CODE==0,Br_exp>2,Season%in%c(2014,2015,2016,2017),Orig_col=="CROZ",!Device==1,!Device=="GDR")%>%
  rename(hab_qual=GRID_CODE)%>%
  group_by(Bandnumb)%>%
  arrange(Season)%>%
  dplyr::slice(n())

c_bqi_pred_prod<-read.csv("Z:/Informatics/S031/analyses/aschmidt/subcol_var/data/croz_bqi_pred_subcol_loc.csv")


c_bqi_quant=quantile(c_bqi_pred_prod$BQI_A, probs = seq(0, 1, 0.25),na.rm=TRUE)
# low_break=c_bqi_pred_prod[58,"BQI_A"]
# mid_break=c_bqi_pred_prod[115,"BQI_A"]

c_bqi_pred_prod<- c_bqi_pred_prod%>%
  mutate(bqi_cat=ifelse(BQI_A<c_bqi_quant[2],"l",ifelse(BQI_A<c_bqi_quant[4],"m","h")))


ggplot(c_bqi_pred_prod,aes(BQI_A))+
  geom_histogram(bins=60)

# Overall correlation between BQI and predicted breeding success (habitat quality)
Hmisc::rcorr(c_bqi_pred_prod$BQI_A, c_bqi_pred_prod$pred_prod)


ggplot(c_bqi_pred_prod,aes(hab_qual,BQI_A))+
  geom_point()+geom_smooth(method=lm)+
  xlab("habitat quality")

# Scatter plot colored by bqi category
c_bqi_pred_prod$bqi_cat<- factor(c_bqi_pred_prod$bqi_cat, levels=c("h","m","l"))
c_bqi_pred_prod%>%
  group_by(bqi_cat)%>%
  summarise(mean_hq=mean(hab_qual))


p1_cbqi <-ggplot(c_bqi_pred_prod,aes(pred_prod,BQI_A, color=bqi_cat))+
  geom_point(size=1.8)+
  geom_smooth(method=lm, aes(fill=bqi_cat))+
  xlab("Habitat quality")+
  ylab("Breeding quality index (BQI)")+
  # scale_colour_manual(values = c("Black", "lightgrey", "grey50"))
  scale_fill_brewer(palette="Paired",labels = c("High","Medium","Low"), aesthetics = c("colour", "fill"))+
  # scale_color_brewer(palette="Paired",labels = c("High","Medium","Low"))+
  # theme_bw()+ 
  guides(colour=guide_legend(title="Crozier/nBQI/ncategory"), fill=guide_legend(title="Crozier/nBQI/ncategory"))+
  theme_classic()+
  ggtitle(("A"))+
  theme(plot.margin = unit(c(1, 0, 2, 1),"lines"))
print(p1_cbqi)
