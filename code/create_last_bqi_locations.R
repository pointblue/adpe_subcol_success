# need to create a table with last BQI value for each bandnumb
library(foreign) # for reading and writing .dbf files
library(dplyr)
library(readr)

# bqi <- read.dbf("Z:/Informatics/S031/analyses/aschmidt/subcol_var/data/bqi_081516.dbf")
bqi<- read.csv("Z:/Informatics/S031/analyses/BQI/BQI_inclcurseas_1998to2017_forAnnie.csv", header=TRUE)%>%
  mutate(season_yr=Season,season = plyr::mapvalues(Season, 
                         from = factor(c(1998:2017)),
                         to = c("9899","9900","0001","0102","0203","0304","0405",
                                "0506","0607","0708","0809","0910","1011","1112",
                                "1213","1314","1415","1516","1617","1718")))

ggplot(bqi,aes(Br_exp,BQI_A,group=Bandnumb))+
  geom_line(aes(colour=Bandnumb))
  

# # filter to birds that have at least 3 seasons of BQI scores
# bqi_3 <- bqi%>%
#   group_by(Bandnumb)%>%
#   tally()%>%
#   filter(n>=3)

# filter to birds that bread in last 4 years and bred at least 3 times
# filter to birds that bread in last 4 years and seen at least 3 times in last 4 years

# count number of times each bird seen
# bqi_seen3 <- bqi%>%
#   filter(season%in%c("1415","1516","1617", "1718"))%>%
#   group_by(Bandnumb)%>%
#   summarise(n_seen=n())%>%
#   filter(n_seen>2)

# bqi_last4seas <- bqi%>%
#   mutate(season_yr=Season)%>%
#   filter(Bandnumb%in%bqi_seen3$Bandnumb, season%in%c("1415","1516","1617", "1718"),!Device=="GDR"&!Device=="1",Orig_col=="CROZ")
#  
#   
# Read in resigt data to get coordinates where bred
allresight_raw <- read.csv("Z:/Informatics/S031/analyses/aschmidt/subcol_var/data/allresight_1718.csv", as.is=TRUE)%>%
  mutate(bandnumb=as.numeric(bandnumb),lat=as.numeric(lat))
         summary(allresight_raw)

#Read in 1718 data ( Note 8/14/18: Hadn't been fully proofed so hasn't been added to all resight yet)
rs17<- read.csv("Z:/Informatics/S031/analyses/aschmidt/subcol_var/data/resight17.csv", as.is=TRUE)%>%
  mutate(bandnumb=as.numeric(bandnumb),lat=as.numeric(lat), season="1718")

# Filter to lines that have gps location
allrs_latlon <- 
  allresight_raw%>%
  full_join(rs17)%>%
  mutate(lat=as.numeric(lat), Bandnumb=as.numeric(bandnumb))%>%
  dplyr::select(Bandnumb,season,colony, orig_col,lat, lon)%>%
  filter(!lat==0, !lon==0, !colony%in%c("ROY","ROYD"))%>%
  group_by(Bandnumb,season)%>%
  dplyr::select(Bandnumb,season,lat,lon)%>%
  # keep only one record per band per season
  dplyr::slice(1)

# replace positive lat with negative
allrs_latlon$lat[allrs_latlon$lat>0]=-allrs_latlon$lat[allrs_latlon$lat>0]
# replace lat>-70 with NA
allrs_latlon$lat[allrs_latlon$lat>-77.3]=NA
allrs_latlon$lat[allrs_latlon$lat<=(-600)]=NA
allrs_latlon$lat[allrs_latlon$lat>-70]=NA
plot(allrs_latlon$lat)

bqi_loc <- 
  inner_join(bqi,allrs_latlon, by=c("Bandnumb","season"))%>%
 #   arrange(Bandnumb))
  # group_by(Bandnumb)%>%
  # dplyr::slice(n())%>%
  # ungroup%>%
  filter(!lat<(-78.0),!lat==1,!lat>-77.3,lon>168.0,lon<169.9)

plot(bqi_loc$lon,bqi_loc$lat)

write.csv(bqi_loc, "Z:/Informatics/S031/analyses/aschmidt/subcol_var/data/croz_all_bqi_loc.csv")
#write.dbf(as.data.frame(last_bqi_loc), "Z:/Informatics/S031/analyses/aschmidt/subcol_var/data/croz_3seas_last_bqi_loc.dbf")



# # Try filtering differently
# bqi_3seas_6y <- bqi%>%
#   mutate(season_yr=Season, season = plyr::mapvalues(Season, 
#                                                     from = factor(c(1998:2017)),
#                                                     to = c("9899","9900","0001","0102","0203","0304","0405",
#                                                            "0506","0607","0708","0809","0910","1011","1112",
#                                                            "1213","1314","1415","1516","1617","1718")))%>%
#   filter(Br_exp>2,season%in%c("1213","1314","1415","1516","1617", "1718"), Breeder==1, !Device=="GDR"&!Device==
#            "1")%>%
#   group_by(Bandnumb)%>%
#   arrange(Bandnumb,season)%>%
#   # use only last bqi score
#   dplyr::slice(n())%>%
#   dplyr::select(Bandnumb,BQI_A,season)
# 
# allrs_latlon_6y <- 
#   allresight_raw%>%
#   full_join(rs17)%>%
#   mutate(lat=as.numeric(lat), Bandnumb=as.numeric(bandnumb))%>%
#   dplyr::select(Bandnumb,season,colony, orig_col,lat, lon)%>%
#   filter(!lat==0, !lon==0,season%in%c("1213","1314","1415","1516","1617", "1718"), !colony%in%c("ROY","ROYD"))%>%
#   group_by(Bandnumb,season)%>%
#   dplyr::select(Bandnumb,season,lat,lon)%>%
#   # keep only one record per band per season
#   dplyr::slice(1)
# 
# last_bqi_loc_6y <- 
#   inner_join(bqi_3seas_6y,allrs_latlon_6y, by=c("Bandnumb","season"))%>%
#   #   arrange(Bandnumb))
#   group_by(Bandnumb)%>%
#   dplyr::slice(n())%>%
#   ungroup%>%
#   filter(!lat<(-78.0),!lat==1,!lat>-77.3,lon>168.0,lon<169.9)
# 
# plot(last_bqi_loc_6y$lon,last_bqi_loc_6y$lat)
# 
# write.csv(last_bqi_loc_6y, "Z:/Informatics/S031/analyses/aschmidt/subcol_var/data/croz_3seas_last_bqi_loc_6y.csv")
