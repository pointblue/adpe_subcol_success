# R code for collating subcolony adult and nest counts from Cape Royds
# Version 1 initiated 07/05/16 by A. Schmidt

# Only dealing with 1415-1718 for this analysis. See ~code/old_versions/royd_subcol_count_prep for start of 
# code to format all years of Royds counts

#-----------------------------------------------------------------------------------

# Libraries----
library("XLConnect")
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readr)

# Set working directory
setwd("Z:/Informatics/S031/analyses/aschmidt/subcol_var")
# Start loading in data----

#1415
# Think count for 14a/b is really just count for 14
# Read in data
ct_1415_wb <- loadWorkbook("Z:/Informatics/S031/AllData/chickcount/r/Royds Annual Ground Count 1415.xlsx")
ct_1415 <- readWorksheet(ct_1415_wb, sheet ="12_03_14", startRow = 7, endRow = 30)

ct_1415_format <- ct_1415%>%
  select(subcol=Subcolony, active_ct =Active.Territories.1, ch_ct=chicks)%>%
  mutate(subcol=tolower(subcol),col="royds",prod=ch_ct/active_ct, area_name=NA, season="1415")%>%
  mutate(subcol=plyr::mapvalues(subcol,from=c("14a/b","19/wb1"), 
                  to=c("14a-b","19")))

# separate 1a+b to 1a and 1b
ct_1415_format[ct_1415_format$subcol=="1a+b","active_ct"]<-ct_1415_format[ct_1415_format$subcol=="1a+b","active_ct"]-ct_1415_format[ct_1415_format$subcol=="1b","active_ct"]
ct_1415_format[ct_1415_format$subcol=="1a+b","ch_ct"]<-ct_1415_format[ct_1415_format$subcol=="1a+b","ch_ct"]-ct_1415_format[ct_1415_format$subcol=="1b","ch_ct"]
ct_1415_format[ct_1415_format$subcol=="1a+b","subcol"]<-"1a"
ct_1415_format[ct_1415_format$subcol=="1a","prod"]<- ct_1415_format[ct_1415_format$subcol=="1a","ch_ct"]/ct_1415_format[ct_1415_format$subcol=="1a","active_ct"]
ct_1415_format[ct_1415_format$subcol=="1a",]
# separate 3a+b to 3a and 3b
ct_1415_format[ct_1415_format$subcol=="3a+b","active_ct"]<-ct_1415_format[ct_1415_format$subcol=="3a+b","active_ct"]-ct_1415_format[ct_1415_format$subcol=="3b","active_ct"]
ct_1415_format[ct_1415_format$subcol=="3a+b","ch_ct"]<-ct_1415_format[ct_1415_format$subcol=="3a+b","ch_ct"]-ct_1415_format[ct_1415_format$subcol=="3b","ch_ct"]
ct_1415_format[ct_1415_format$subcol=="3a+b","subcol"]<-"3a"
ct_1415_format[ct_1415_format$subcol=="3a","prod"]<- ct_1415_format[ct_1415_format$subcol=="3a","ch_ct"]/ct_1415_format[ct_1415_format$subcol=="3a","active_ct"]
ct_1415_format[ct_1415_format$subcol=="3a",]



# 1516
ct_1516_wb <- loadWorkbook("Z:/Informatics/S031/S0311516/royds1516/Colony Counts/Royds Annual Ground Count 1516.xlsx")
ct_1516 <- readWorksheet(ct_1516_wb, sheet ="12_01_15", startRow = 5, endRow = 28)

ct_1516_format <- ct_1516%>%
  select(subcol=Subcolony, active_ct =Active.Territories, ch_ct=Chicks)%>%
  mutate(subcol=tolower(subcol),col="royds",prod=ch_ct/active_ct, area_name=NA, season="1516")%>%
  # remove ct from 19, not clear what was counted here
  filter(!subcol==19)%>%
  mutate(subcol=plyr::mapvalues(subcol, from=c("14a/b","wb1"), to=c("14a-b","19")))



# 1617
ct_1617_wb <-loadWorkbook("Z:/Informatics/S031/S0311617/royds1617/ground & chick counts/Royds Annual Ground Count 1617.xlsx")
ct_1617 <- readWorksheet(ct_1617_wb, sheet ="12 01 16", startRow = 8, endRow = 30)

ct_1617_format <- ct_1617%>%
  select(subcol=Subcolony, active_ct =Active.Territories, ch_ct=chicks)%>%
  mutate(subcol=tolower(subcol),col="royds",prod=ch_ct/active_ct, area_name=NA, season="1617")%>%
  mutate(subcol=plyr::mapvalues(subcol, from=c("14a/b"), to=c("14a-b")))

# 1718
ct_1718_wb <-loadWorkbook("Z:/Informatics/S031/S0311718/royds1718/ground & chick counts/Royds Annual Ground Count 1011_1718.xlsx")
ct_1718 <- readWorksheet(ct_1718_wb, sheet ="11_30_17", startRow = 7, endRow = 30)

ct_1718_format <- ct_1718%>%
  select(subcol=Subcolony, active_ct =Active.Territories, ch_ct=chicks)%>%
  mutate(subcol=tolower(subcol),col="royds",prod=ch_ct/active_ct, area_name=NA, season="1718")%>%
  mutate(subcol=plyr::mapvalues(subcol, from=c("14a/b","19 (wb1)","11/12"), to=c("14a-b", "19", "11-12")))


r_ct_all4<- ct_1415_format%>%
  full_join(ct_1516_format)%>%
  full_join(ct_1617_format)%>%
  full_join(ct_1718_format)%>%
  arrange(subcol)

# write.csv(r_ct_all4, "data/royds_ct_clean_1415-1718.csv", row.names = FALSE)


# Format subcol measurements ####-------------------------------------------------------------------------------
# load subcol measures

# this file contains area, perimeter, perimeter area ratio, and flood risk for all subcolonies
r_geom_14_raw<- read_csv("data/royds_selected_geom.txt")
# format
r_geom_14_format<- r_geom_14_raw%>%
  select(FID,subcol, area, perimeter, pa_ratio,flood_risk)%>%
  filter(!is.na(subcol))

# r_flow_acc_14_format <- read.csv("data/royds_mean_flow_acc.csv", header=TRUE)%>%
#   select(subcol=SUBCOL, mean_flow_acc= MEAN)%>%
#   mutate(flow_acc_snow_log1p=log1p(flow_acc_snow))

r_flow_acc_14_snowformat <- read.csv("data/royds_mean_flow_acc_snow_v2.csv", header=TRUE)%>%
  select(subcol=SUBCOL, flow_acc= MEAN)%>%
  mutate(flow_acc_log1p=log1p(flow_acc),flow_acc_log=log(flow_acc))

# load aspect stats
# loading new aspect table with means calculated using trig functions
r_aspect_14_raw <- read.csv("data/royds_subcol_aspect_rev2_090820.csv", header=TRUE)
r_aspect_14_format <- r_aspect_14_raw%>%
  dplyr::select(subcol=SUBCOL,mean_aspect=MEAN_ASPECT)

r_join <- r_geom_14_format%>%
  full_join(r_aspect_14_format)%>%
  filter(!is.na(subcol))

# load elevation stats
r_elev_14_raw <- read.csv("data/royds_mean_elev.csv", header=TRUE)
# reformat
r_elev_14_format <- r_elev_14_raw%>%
  select(subcol=SUBCOL,mean_elev=MEAN)%>%
  mutate(adjust_mean_elev=mean_elev+46) # elevation of DEM is off by about 46m (I asked PGC about this but I forget the reason they gave me)

# load wind/shade stats and format
# r_wind_14_format<- read.csv("data/royds_wind.txt", header=TRUE)%>%
#   select(subcol=royd_subco,mean_wind=MEAN)

# r_windshelt_14_format <- read.csv("data/royds_mean_windshelter100m.csv",header=TRUE)%>%
  # select(subcol=SUBCOL, mean_windshelt100m=MEAN)

# Royds wind from SE
# r_windshelt_14_300mformat <- read.csv("data/royds_mean_windshelt_rev2.txt",header=TRUE)%>%
#   dplyr::select(subcol=SUBCOL, mean_windshelt300m=MEAN)

# ROyds wind from S
r_windshelt_14_300mformat <- read.csv("data/royds_mean_windshelt_rev2.txt",header=TRUE)%>%
  dplyr::select(subcol=SUBCOL, mean_windshelt300m=MEAN)
 
# load slope stats
r_slope_14_format <- read.csv("data/royds_mean_slope.csv", header=TRUE)%>%
  select(subcol=SUBCOL,mean_slope=MEAN)
 
# # load insolation stats
# r_solar_14_format <- read.csv("data/royds_solar.txt")%>%
#   select(subcol=royd_subco, mean_solar=MEAN)

# Load skua data
r_skua50_format <- read.csv("data/royds_skua_50m.csv")%>%
  select(FID=in_fid)%>%
  mutate(skua50=1)
# 
# skua100_raw <- read.csv("data/croz_skua_100m.txt")
# skua100_format <- skua100_raw%>%
#   select(SUBCOLID, NEAR_RANK)%>%
#   rename(FID=SUBCOLID, skua100=NEAR_RANK)
# 
# # combine all 2014 measurement data

r_list_14 <- list(r_geom_14_format,r_flow_acc_14_snowformat,r_aspect_14_format,r_slope_14_format,r_elev_14_format,r_windshelt_14_300mformat, r_skua50_format)
r_all_meas_14 <- as.data.frame(r_list_14[1])
for(i in 1:(length(r_list_14)-1)){
  a = data.frame(r_list_14[i+1])
  r_all_meas_14 <- full_join(r_all_meas_14,a)
}

# remove unnamed subcol
r_all_meas_14<- r_all_meas_14%>%
  filter(!is.na(subcol),!is.na(FID))%>%
  mutate(skua50=replace_na(skua50,0))

# # # combine count and measurement data ####---------------------------------------------------------------------------
# # 
# 
r_all_meas_ct <- r_ct_all4%>%
  inner_join(r_all_meas_14)

# check if lost any data
anti_join(r_ct_all4,r_all_meas_ct)
# looks like only lost count from subcol 8 which is because it didn't show up in the aerial image so there are no attributes for it

# 
# write data to file
write.csv(r_all_meas_ct, "data/royds_selected_meas_ct_all_v12.csv", row.names = FALSE)

