# need to create a table with last BQI value for each bandnumb
library(foreign) # for reading and writing .dbf files
library(dplyr)

# bqi <- read.dbf("Z:/Informatics/S031/analyses/aschmidt/subcol_var/data/bqi_081516.dbf")
bqi<- read.csv("Z:/Informatics/S031/analyses/BQI/BQI_incl_curr_seas_98thru13.csv", header=TRUE)

bqi_last <- bqi%>%
  select(Bandnumb,Season, Breeder, BQI_A, diff_success, Orig_col)%>%
  filter(!is.na(BQI_A), Orig_col=="CROZ", Breeder==1)%>%
  mutate(Season_yr=Season, Season = plyr::mapvalues(Season, 
                           from = factor(c(1998:2013)),
                           to = c("9899","9900","0001","0102","0203","0304","0405",
                                  "0506","0607","0708","0809","0910","1011","1112",
                                  "1213","1314")))%>%
  group_by(Bandnumb)%>%
  slice(n())%>%
  ungroup

allresight_raw <- read.dbf("Z:/Informatics/S031/analyses/aschmidt/subcol_var/data/allresight.dbf", as.is=TRUE)


allrs_filt <- allresight_raw%>%
  select(BANDNUMB,SEASON,ORIG_COL,LAT, LON)%>%
  rename(Bandnumb=BANDNUMB, Season=SEASON)%>%
  filter(!LAT==0,ORIG_COL=="CROZ")%>%
  arrange(Bandnumb)

last_bqi_loc <- inner_join(bqi_last,allrs_filt)%>%
  group_by(Bandnumb)%>%
  slice(n())%>%
  ungroup%>%
  filter(!LAT<(-77.5)&!LAT==1&!LAT>-77.3&LON>169.2&LON<169.3)


write.csv(last_bqi_loc, "Z:/Informatics/S031/analyses/aschmidt/subcol_var/data/croz_last_bqi_loc.csv")
write.dbf(as.data.frame(last_bqi_loc), "Z:/Informatics/S031/analyses/aschmidt/subcol_var/data/croz_last_bqi_loc.dbf")

sapply(last_bqi_loc, class)
