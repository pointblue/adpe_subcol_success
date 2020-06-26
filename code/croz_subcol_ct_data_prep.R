#### Description ####--------------------------------------------------------------------------------
# Written by A. Schmidt
# This script combines and formats subcolony count data in prep for analysis
# Uses previously compiled and cleaned files containing adult and chick counts from M and counts from 
# extra subcolonies in 1415-1516
# 1617 and 1718 counts are added and cleaned here then combined with other counts 
# where possible, paths point to raw data files from field
# Trying to set it up to do all the processing needed in the script
# Only thing to prep is to save the raw files as .csv
####-------------------------------------------------------------------------------------------------


# Load libraries ####
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

setwd("Z:/Informatics/S031/analyses/aschmidt/subcol_var")

# read in M count data files ####
adct_m <- read.csv("data/m_ad_ct_all_clean.csv", colClasses = c("character", "character", "POSIXct", "character", "numeric", "numeric","numeric", "character"))
chct_m <- read.csv("data/m_ch_ct_all_clean.csv", colClasses = c("character", "character", "POSIXct", "character", "numeric", "character"))

# combine M adult and chick counts to get productivity estimates
m_all_ct<- adct_m%>%
  dplyr::select(col, season, subcol, active_ct,occ_ct, total_ct)%>%
  inner_join(chct_m, by=c("col","season","subcol"))%>%
  mutate(prod=ch_ct/active_ct,area_name="m", subcol = paste(area_name,subcol, sep = ""))

#write.csv(m_all_ct, "data/m_ct_ad_ch_all_clean.csv", row.names = FALSE)

# Load counts from 1415-1516 from extra subcolonies ####
oth_ct<- read.csv("data/subcol_avg_counts_all.csv", header = TRUE, colClasses = c("character","character","numeric","numeric","numeric","numeric"))
# add columns and rename to match M counts
oth_ct_format <- oth_ct %>%
  rename(subcol=subID,occ_ct = occupied,active_ct = active, ch_ct = chicks)%>%
  mutate(col="croz", subcol=plyr::mapvalues(subcol, 
                                     from = c("b_", "d10", "t14", "g23+"),
                                     to = c("b33", "d9", "qr157","g58")),
         area_name = stringr::str_extract(subcol,"[a-z]+"))%>%
  filter(!subcol=="t14a") # t14a not a real subcol so removing
  
# removing ct from d24 in 1415 because unclear if counted extra lobe for both nests and chicks this year
  # first rename d24 in 1415 so can filter out
oth_ct_format[oth_ct_format$subcol=="d24"&oth_ct_format$season=="1415","subcol"]<- "remove"
oth_ct_format <- filter(oth_ct_format, !subcol=="remove")

# mistake in count for c6 in 1415 (see note in original data file). active_ct should be 147
oth_ct_format[oth_ct_format$subcol=="c6"&oth_ct_format$season=="1415","active_ct"]<- 147
oth_ct_format[oth_ct_format$subcol=="c6"&oth_ct_format$season=="1415","occ_ct"]<- NA
oth_ct_format[oth_ct_format$subcol=="c6"&oth_ct_format$season=="1415","prod"]<- oth_ct_format[oth_ct_format$subcol=="c6"&oth_ct_format$season=="1415","ch_ct"]/oth_ct_format[oth_ct_format$subcol=="c6"&oth_ct_format$season=="1415","active_ct"]


# 1617 ####--------------------------------------------------------------------------------------------------------
# load M adult counts
m_ad_1617_raw <- read.csv("Z:/Informatics/S031/S0311617/croz1617/chick counts/adultcount_1617.csv",header=TRUE)
# calculate average count
m_ad_1617_format<- m_ad_1617_raw%>%
  group_by(subcolony)%>%
  dplyr::summarize(active_ct=mean(activenests,na.rm=TRUE), occ_ct=mean(occupiedterritories,na.rm=TRUE),
            tot_ct=mean(totalindividuals,na.rm=TRUE))%>%
  # change all subcol alphas to lowercase
  mutate(season="1617",col="croz",area_name = "m", subcol=tolower(paste("m",subcolony,sep="")))%>%
  # remove empty row
  filter(!active_ct==(is.na(active_ct)))%>%
  # remove extra subcol column
  dplyr::select(-subcolony)%>%
  as.data.frame()

# m21 and m24 counted together for chick count so adding nest count together
m_ad_1617_format[m_ad_1617_format$subcol=="m21",c("active_ct","occ_ct","tot_ct")] <- colSums(m_ad_1617_format[m_ad_1617_format$subcol%in%c("m21","m24"),c("active_ct","occ_ct","tot_ct")])
# remove row for m24
m_ad_1617_format<-m_ad_1617_format[m_ad_1617_format$subcol!="m24",]
# rename m21, m21-24
m_ad_1617_format[m_ad_1617_format$subcol=="m21","subcol"]<-"m21-24"

# load M chick counts
m_ch_1617_raw <- read.csv("Z:/Informatics/S031/S0311617/croz1617/chick counts/chickcount_1617.csv", header=TRUE)
m_ch_1617_format<- m_ch_1617_raw%>%
  group_by(Subcolony)%>%
  summarise(ch_ct=mean(count,na.rm=TRUE))%>%
  mutate(season="1617",col="croz",area_name = "m", subcol=paste("m",Subcolony,sep=""))%>%
  mutate(subcol=plyr::mapvalues(subcol, 
                                from = c("m24-21"),
                                to = c("m21-24")))%>%
  #Remove empty row
  filter(!ch_ct==(is.na(ch_ct)))%>%
  # Remove extra subcol column
  dplyr::select(-Subcolony)

# join 1617 M adult and chick counts
m_1617_join <- m_ad_1617_format%>%
  full_join(m_ch_1617_format)%>%
  #add column for productivity
  mutate(prod=ch_ct/active_ct)%>%
  # looks like 6 and 6a were merged for chick count only so removing both from this year
  filter(!subcol%in%c("m6", "m6a"))



# load subcol counts from 1617 ####
sc_1617_raw <- read.csv("Z:/Informatics/S031/S0311617/croz1617/Subcolonycounts/croz_extra_subcol_ad_and_chick_counts_1617.csv", header=TRUE)
# format count to match
sc_1617_format <- sc_1617_raw%>%
  group_by(subcol_id)%>%
  summarise(active_ct=mean(active,na.rm=TRUE), occ_ct=mean(occupied,na.rm=TRUE), ch_ct=mean(ch_ct,na.rm=TRUE))%>%
  rename(subcol=subcol_id)%>%
  mutate(season="1617",prod= ch_ct/active_ct,col="croz",area_name = stringr::str_extract(subcol,"[a-z]+"), subcol=as.character(subcol))%>%
  # fix a couple area names and subcol names
  mutate(area_name=plyr::mapvalues(area_name, 
                                   from = c("fbeach", "misl"),
                                   to = c("f","m")), subcol=plyr::mapvalues(subcol, 
                                                                            from =c("g23_1","misl","p24_1"), 
                                                                            to = c("g58","mis","p24")))
 
# mistake in count for d24-2: occ_ct=51.5, active_ct=49.5
sc_1617_format[sc_1617_format$subcol==("d24-2"),"active_ct"]<- 49.5
sc_1617_format[sc_1617_format$subcol==("d24-2"),"occ_ct"]<- 51.5
sc_1617_format[sc_1617_format$subcol==("d24-2"),"prod"]<- sc_1617_format[sc_1617_format$subcol==("d24-2"),"ch_ct"]/sc_1617_format[sc_1617_format$subcol==("d24-2"),"active_ct"]

# # looks like I counted d24-1 and d24-2 together in 1516, unclear for 1415. Removing ct for d24 from 1415 (did this above) and combining counts for 1617
# # add rows for d24-1 and d24-2 together
# sc_1617_format[sc_1617_format$subcol=="d24-1",c("active_ct","occ_ct","ch_ct")] <- colSums(sc_1617_format[sc_1617_format$subcol%in%c("d24-1","d24-2"),c("active_ct","occ_ct","ch_ct")])
# # Merge p44_1 and p44_2 because counted together in previous years
# sc_1617_format[sc_1617_format$subcol=="p44_1",c("active_ct","occ_ct","ch_ct")] <- colSums(sc_1617_format[sc_1617_format$subcol%in%c("p44_1","p44_2"),c("active_ct","occ_ct","ch_ct")])
# # remove extra row for d24-2 and p44-2
# sc_1617_format<- sc_1617_format[!sc_1617_format$subcol%in%c("d24-2","p44_2"),]
# # rename d24-1 to d24 and p44_1 to p44
# sc_1617_format[sc_1617_format$subcol%in%c("d24-1","p44_1"),"subcol"] <-c("d24","p44")
# # recalculate prod for these subcol
# sc_1617_format[sc_1617_format$subcol=="d24","prod"] <- sc_1617_format[sc_1617_format$subcol=="d24","ch_ct"]/sc_1617_format[sc_1617_format$subcol=="d24","active_ct"]
# sc_1617_format[sc_1617_format$subcol=="p44","prod"] <- sc_1617_format[sc_1617_format$subcol=="p44","ch_ct"]/sc_1617_format[sc_1617_format$subcol=="p44","active_ct"]


# join 1617 M and extra subcol counts
all_1617 <- m_1617_join%>%
  full_join(sc_1617_format)



# load 1718 data ####
# load M adult counts
m_ad_1718_raw <- read.csv("Z:/Informatics/S031/S0311718/croz1718/chick counts/adultcount_1718.csv",header=TRUE)
# calculate average count
m_ad_1718_format<- m_ad_1718_raw%>%
  group_by(subcolony)%>%
  summarise(active_ct=mean(activenests,na.rm=TRUE), occ_ct=mean(occupiedterritories,na.rm=TRUE), tot_ct=mean(totalindividuals,na.rm=TRUE))%>%
  mutate(season="1718",col="croz",area_name = "m", subcol=tolower(paste("m",subcolony,sep="")))%>%
  # remove empty row
  filter(!active_ct==(is.na(active_ct)))%>%
  # remove extra subcol column
  dplyr::select(-subcolony)%>%
  as.data.frame()

# m21 and m24 counted together for chick count so adding nest count together
m_ad_1718_format[m_ad_1718_format$subcol=="m21",c("active_ct","occ_ct","tot_ct")] <- colSums(m_ad_1718_format[m_ad_1718_format$subcol%in%c("m21","m24"),c("active_ct","occ_ct","tot_ct")])
# remove row for m24
m_ad_1718_format<-m_ad_1718_format[m_ad_1718_format$subcol!="m24",]
# rename m21, m21-24
m_ad_1718_format[m_ad_1718_format$subcol=="m21","subcol"]<-"m21-24"


# load 1718 M chick counts
m_ch_1718_raw <- read.csv("Z:/Informatics/S031/S0311718/croz1718/chick counts/chickcount_1718.csv", header=TRUE)
m_ch_1718_format<- m_ch_1718_raw%>%
  group_by(Subcolony)%>%
  summarise(ch_ct=mean(count,na.rm=TRUE))%>%
  mutate(season="1718",col="croz",area_name = "m", subcol=tolower(paste("m",Subcolony,sep="")))%>%
  mutate(subcol=plyr::mapvalues(subcol, 
                                from = c("m21/24","m16/22"),
                                to = c("m21-24","m16-22")))%>%
  #Remove empty row
  filter(!ch_ct==(is.na(ch_ct)))%>%
  # Remove extra subcol column
  dplyr::select(-Subcolony)

# join 1718 M adult and chick counts
m_1718_join <- m_ad_1718_format%>%
  full_join(m_ch_1718_format)%>%
  #add column for productivity
  mutate(prod=ch_ct/active_ct)

# load 1718 extra subcol counts
sc_1718_raw <- read_csv("Z:/Informatics/S031/S0311718/croz1718/subcolonycounts/croz_subcol_counts_1718.csv")
# format count to match
sc_1718_format <- sc_1718_raw%>%
  group_by(subcol_id)%>%
  summarise(active_ct=mean(active,na.rm=TRUE), occ_ct=mean(occupied,na.rm=TRUE), ch_ct=mean(ch_ct, na.rm=TRUE))%>%
  rename(subcol=subcol_id)%>%
  mutate(season="1718",prod= ch_ct/active_ct,col="croz",area_name = stringr::str_extract(subcol,"[a-z]+"))%>%
  mutate(subcol=plyr::mapvalues(subcol, 
                                from = c("c19-1","p44-1","p44-2","p40-1","p40-2"),
                                to = c("c19_1","p44_1","p44_2","p40_1","p40_2")),
         area_name=plyr::mapvalues(area_name,from=c("fbeach","mis","wb"),to = c("f","m","b")))%>%
  filter(!subcol=="p40_2") # did not count nests, only chx so causing problems

# # Merge p44-1 and p44-2 because counted together in previous years
# sc_1718_format[sc_1718_format$subcol=="p44-1",c("active_ct","occ_ct","ch_ct")] <- colSums(sc_1718_format[sc_1718_format$subcol%in%c("p44-1","p44-2"),c("active_ct","occ_ct","ch_ct")])
# # remove extra row for p44-2
# sc_1718_format<- sc_1718_format[!sc_1718_format$subcol==c("p44-2"),]
# # rename p44_1 to p44
# sc_1718_format[sc_1718_format$subcol=="p44-1","subcol"] <-"p44"
# # recalculate prod for this subcol
# sc_1718_format[sc_1718_format$subcol=="p44","prod"] <- sc_1718_format[sc_1718_format$subcol=="p44","ch_ct"]/sc_1718_format[sc_1718_format$subcol=="p44","active_ct"]


# join 1718 M and extra subcol counts
all_1718 <- m_1718_join%>%
  full_join(sc_1718_format)%>%
  filter(!subcol=="wb")# wb count included in other file


# # Gather WB data
# # Read in WB count data
# wb_ct_raw <- read_csv("Z:/Informatics/S031/S0311617/croz1617/nestcheck/nestcount_compiled_07-16.csv")[-1,]
# # format data column so can filter
# strptime((wb_ct_raw$DATE), "%m/%d/%Y")
# as.Date(wb_ct_raw$DATE,format="%m/%d/%Y")
# wb_ad_ct_format <- wb_ct_raw%>%
#   select(SEASON, DATE, COLONY,SUBCOL, DATE, ACTIVE)%>%
#   mutate(DATE=as.Date(DATE, format="%m/%d/%Y"))%>%
#   mutate(jday=format(DATE, "%j"), SUBCOL=tolower(SUBCOL))%>%
#   filter(jday>333&jday<340, SUBCOL=="wb")
# 
# summary(wb_ct_format)

# Made file of just the WB counts I need in data folder
wb_ct <- read_csv("data/wb_ad_ch_ct_1415_1718.csv")
 wb_ct_format <- wb_ct%>%
  mutate(SEASON=as.character(SEASON),prod=CHICKS/ACTIVE,COLONY=tolower(COLONY),SUBCOL=tolower(SUBCOL),area_name="b")%>%
  rename(season=SEASON, active_ct=ACTIVE, ch_ct=CHICKS,col=COLONY,subcol=SUBCOL)%>%
  dplyr::select(-X8,-X9,-X10, -X11)%>%
  filter(!is.na(col))


# Join all count files together ####
all_ct <- full_join(m_all_ct,oth_ct_format)%>%
  full_join(all_1617)%>%
  full_join(all_1718)%>%
  full_join(wb_ct_format)%>%
  dplyr::select(col, season, subcol, active_ct, ch_ct,prod, area_name)%>%
  # rename m41 to m6a (I swear I already did this somewhere else!!)
  # rename s2 to s2-4 because counted them together on photo
  # d9-11 counted together in 1617 but labeled as d10-11
  #some confusion about l23, l23+. Renaming l23=l23_1, and l23+=l23_2
  # except in 1617, l23_1 should be l23+. Changing that one first:
  mutate(subcol=plyr::mapvalues(subcol, 
                                  from = c("l23_1"),
                                  to = c("l23+")))%>%
  mutate(subcol=plyr::mapvalues(subcol, 
                         from = c("m41", "d10-11","d24-1","d24-2", "l23","l23+", "s2"),
                         to = c("m6a","d9-11","d24_1","d24_2","l23_1","l23_2", "s2-4")))%>%
  ### Remove m36 and m37 because boundaries have been inconsistent over the years (consider for 6 and 6a as well)
  filter(!subcol%in%c("m36","m37"))%>%
  arrange(subcol,season)

# t <- all_ct%>%
#   filter(area_name=="m")
# write.csv(t, "Z:/Informatics/S031/analyses/aschmidt/subcol_var/data/m_ct_ad_ch_all_clean.csv")
 
 
# ann_mean_prod <- aggregate(prod~season,data=all_ct, FUN=mean)%>%
#   rename(ann_mean_prod=prod)
# mean_prod<-mean(ann_mean_prod$ann_mean_prod)
# 
# all_ct_anom <- all_ct%>%
#   full_join(ann_mean_prod, by="season")%>%
#   mutate(ann_prod_anom=prod-ann_mean_prod, prod_anom=prod-mean_prod)%>%
#   # add col_side factor
#   mutate(col_side=ifelse(area_name%in%c("b","c","n","m","p","l"),"w","e"))%>%
#   arrange(season)


# Format subcol measurements ####-------------------------------------------------------------------------------
#load subcol measures
# this file contains area, perimeter, perimeter area ratio, and flood risk for all subcolonies
 geom_flood_14_raw<- read_csv("data/croz_selected_geom_v3.txt")
 # format
 geom_flood_14_format<- geom_flood_14_raw%>%
   dplyr::select(FID,subcol, area, perim, pa_ratio,flood_risk=SUM_AREA)
 

 # V2 has snow cells weighted by 1 and non-snow cell weighted by 0
# V4 has snow cells weighted by 3 and non-snow cell weighted by 1
flow_acc <- read_csv("data/croz_selected_mean_flow_acc_snow_v4.csv")%>% #v4 included in final v17
   dplyr::select(subcol=SUBCOL, flow_acc=MEAN)%>%
   mutate(flow_acc_log1p=log1p(flow_acc))
 
# load aspect stats
# aspect_14_raw <- read_csv("data/croz_selected_mean_aspect_corr.txt")
# aspect_corrected_adjust had 360 added to any aspect values>280 (there are no aspect values between ~180 and 350) 
# so that could average the aspect values for the subcols that span 0 correctly
# now can subtract 360 from the averages and should be correct
aspect_14_raw <- read_csv("Z:/Informatics/S031/analyses/aschmidt/subcol_var/GIS/croz/rev2/croz_selected_aspect_corrected_adjust.txt")
aspect_14_format <- aspect_14_raw%>%
  dplyr::select(subcol=SUBCOL, mean_aspect=MEAN)%>%
  mutate(mean_aspect=mean_aspect-360)

# load elevation stats
elev_14_raw <- read_csv("data/croz_selected_mean_elev.txt")
# reformat
elev_14_format <- elev_14_raw%>%
  dplyr::select(subcol,mean_elev=MEAN)%>%
  mutate(adjust_mean_elev=mean_elev+47)


# load windshelter
# windshelt_14_format <-read_csv("data/croz_selected_mean_windshelt_300m_2pi_v2.1.txt")%>% included in v16
windshelt_14_format <-read_csv("data/croz_selected_wind_300m_pi_pi8_lcc169.txt")%>% # included in v17
  dplyr::select(subcol=croz_selec,mean_windshelt300m=MEAN)

# windshelt_14_format2 <-read_csv("data/croz_selected_mean_windshelt_100mpi4pi12.txt")%>%
#   dplyr::select(subcol=SUBCOL,mean_windshelt100m=MEAN)
# 
# windshelt_14_format3 <-read_csv("data/croz_selected_mean_windshelt_100mpi8pi8.txt")%>%
#   dplyr::select(subcol=SUBCOL,mean_windshelt100mp8=MEAN)
  
 
# load slope stats
slope_14_raw <- read_csv("data/croz_selected_mean_slope.txt")
slope_14_format <- slope_14_raw%>%
  dplyr::select(subcol,mean_slope=MEAN)


# Load skua data
skua50_raw <- read_csv("data/croz_selected_near_skua50.txt")
# format
skua50_format <- skua50_raw%>%
  dplyr::select(FID=IN_FID, skua50=NEAR_DIST)%>%
  mutate(skua50=1)


# # combine all 2014 measurement data
list_14 <- list(geom_flood_14_format, aspect_14_format,slope_14_format,elev_14_format, windshelt_14_format,skua50_format, flow_acc)
all_meas_14 <- as.data.frame(list_14[1])
for(i in 1:(length(list_14)-1)){
  a = data.frame(list_14[i+1])
  all_meas_14 <- full_join(all_meas_14,a)
}

# Fix a few naming issues
all_meas_14_format<-all_meas_14%>%
  mutate(subcol=plyr::mapvalues(subcol, from=c("c19-1"), to = c("c19_1")))
# 
# # combine count and measurement data ####---------------------------------------------------------------------------
# 

all_meas_ct <- all_ct%>%
    inner_join(all_meas_14_format)
# checking what data getting dropped by this join:
anti_join(all_ct,all_meas_ct)
# looks like losing some data in this join but most don't have complete data from years of interest:
# Subcolonies with full count data (ad and chick counts from at least one year 1415-1718) that appear to be missing measurement data:
# c40 (only counted 1617), fbeach_2 (doesn't appear on aerial photo), g46 (counted all 4 years), l19 (only counted 1718), m19, m20 (lots of msubcol that don't have counts from last 4 years), 
# qr157 (doen't look like same area was counted in 1415 and 1516, no counts from 1617 or 1718)
# The only one that would be nice to include is g46 but not worth redoing all spatial stats for that so leaving out for now (not showing up now (6/17/20))
# 
 

# Missing a lot of raster based attributes for subcol
# also it looks like I lost g58 in the raster conversion. It's a small subcol right next to g23 
# so I'm leaving it out for now
miss <- all_meas_ct%>%
  filter(is.na(mean_slope))%>%
  arrange(subcol)%>%
  distinct(subcol)

# Not sure how to do this other than one at a time
miss[1,] # d11
all_meas_ct[all_meas_ct$subcol=="d11",c("mean_aspect","mean_slope","mean_elev","adjust_mean_elev","mean_windshelt300m","flow_acc", "flow_acc_log1p")]<-
  all_meas_14_format[all_meas_14_format$subcol=="d9-11",c("mean_aspect","mean_slope","mean_elev","adjust_mean_elev","mean_windshelt300m","flow_acc", "flow_acc_log1p")][1,]  
miss[2,] #d24: replace with values from d24_1 (split from d24 but should be the same)
all_meas_ct[all_meas_ct$subcol=="d24",c("mean_aspect","mean_slope","mean_elev","adjust_mean_elev","mean_windshelt300m","flow_acc", "flow_acc_log1p")]<-
  all_meas_14_format[all_meas_14_format$subcol=="d24_1",c("mean_aspect","mean_slope","mean_elev","adjust_mean_elev","mean_windshelt300m","flow_acc", "flow_acc_log1p")][1,]  
miss[3,] #d47-48: replace with values from d47
all_meas_ct[all_meas_ct$subcol=="d47-48",c("mean_aspect","mean_slope","mean_elev","adjust_mean_elev","mean_windshelt300m","flow_acc", "flow_acc_log1p")]<-
  all_meas_14_format[all_meas_14_format$subcol=="d47",c("mean_aspect","mean_slope","mean_elev","adjust_mean_elev","mean_windshelt300m","flow_acc", "flow_acc_log1p")][1,]  
miss[4,] #d9: replace with values from d9-11
all_meas_ct[all_meas_ct$subcol=="d9",c("mean_aspect","mean_slope","mean_elev","adjust_mean_elev","mean_windshelt300m","flow_acc", "flow_acc_log1p")]<-
  all_meas_14_format[all_meas_14_format$subcol=="d9-11",c("mean_aspect","mean_slope","mean_elev","adjust_mean_elev","mean_windshelt300m","flow_acc", "flow_acc_log1p")][1,]  
miss[5,] #m21-24, replace with m21
all_meas_ct[all_meas_ct$subcol=="m21-24",c("mean_aspect","mean_slope","mean_elev","adjust_mean_elev","mean_windshelt300m","flow_acc", "flow_acc_log1p")]<-
  all_meas_14_format[all_meas_14_format$subcol=="m21",c("mean_aspect","mean_slope","mean_elev","adjust_mean_elev","mean_windshelt300m","flow_acc", "flow_acc_log1p")][1,]  
miss[6,] #p22-23, replace with p22
all_meas_ct[all_meas_ct$subcol=="p22-23",c("mean_aspect","mean_slope","mean_elev","adjust_mean_elev","mean_windshelt300m","flow_acc", "flow_acc_log1p")]<-
  all_meas_14_format[all_meas_14_format$subcol=="p22",c("mean_aspect","mean_slope","mean_elev","adjust_mean_elev","mean_windshelt300m","flow_acc", "flow_acc_log1p")][1,]  
miss[7,] #p44_1, replace with p44
all_meas_ct[all_meas_ct$subcol=="p44_1",c("mean_aspect","mean_slope","mean_elev","adjust_mean_elev","mean_windshelt300m","flow_acc", "flow_acc_log1p")]<-
  all_meas_14_format[all_meas_14_format$subcol=="p44",c("mean_aspect","mean_slope","mean_elev","adjust_mean_elev","mean_windshelt300m","flow_acc", "flow_acc_log1p")][1,]  
miss[8,] #p44_2, replace with p44
all_meas_ct[all_meas_ct$subcol=="p44_2",c("mean_aspect","mean_slope","mean_elev","adjust_mean_elev","mean_windshelt300m","flow_acc", "flow_acc_log1p")]<-
  all_meas_14_format[all_meas_14_format$subcol=="p44",c("mean_aspect","mean_slope","mean_elev","adjust_mean_elev","mean_windshelt300m","flow_acc", "flow_acc_log1p")][1,]  


# fill in NA in skua and flood_risk variables with 0
all_meas_ct[is.na(all_meas_ct$skua50), "skua50"]<- 0
all_meas_ct[is.na(all_meas_ct$flood_risk), "flood_risk"]<- 0

# # write data to file
write.csv(all_meas_ct, "data/croz_selected_meas_ct_all_v18.csv", row.names = FALSE)

