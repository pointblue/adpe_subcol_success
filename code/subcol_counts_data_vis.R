#Set working directory
setwd("Z:/Informatics/S031/analyses/aschmidt/subcol_var")

# Libraries
library(ggplot2)
library(dplyr)
library(readr)
library(mgcv)
library(bbmle)


# read in data files ####
ct_meas <- read_csv("data/croz_selected_meas_ct_all.csv")
spec(ct_meas)
# select only years 1415-1718
ct_meas_sub <- ct_meas%>%
  filter(season%in%c("1415", "1516","1617","1718"))

# Area and perimeter highly correlated
# Also wind and solar
# number of times each subcolony counted
n_subcol_cts <- ct_meas%>%
  group_by(subcol)%>%
  tally()

# calculate number of subcolonies counted each year 
n_subcol_season <- ct_meas%>%
  group_by(season)%>%
  tally()


# plot ave by subcol
ggplot(ct_meas, aes(subcol,ann_prod_anom)) + 
  geom_boxplot() + 
  annotate("text", x = 1:length(n_subcol_cts$subcol),y =1.65, label = n_subcol_cts$n, col="blue") + 
  labs(y = "chicks per active nest")

# plot average by season
ct_meas$Iceberg <- ifelse(ct_meas$season%in%c("0203", "0304","0405","0506"),"Iceberg","No Iceberg")
ggplot(ct_meas, aes(season,prod)) + 
  geom_boxplot() + 
  annotate("text", x = 1:length(n_subcol_season$season),y =2.1, label = n_subcol_season$n, col="blue") + 
  labs(y = "Chicks per active nest", x="Season")

# only use counts from M
# Calculate how many subcol in M counted each yr
n_subcol_m <- ct_meas%>%
  filter(area_name=="m")%>%
  group_by(season)%>%
  tally()

ct_meas%>%
  filter(area_name=="m")%>%
  ggplot(aes(season,prod)) + 
  geom_boxplot(fill="grey90") + 
  annotate("text", x = 1:length(n_subcol_m$season),y =2.1, label = n_subcol_m$n, col="black")+
  labs(y = "Chicks per active nest", x="Season")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))


# plot average area of subcolonies by year
ggplot(ct_meas,aes(season,area))+
  geom_boxplot() + 
  annotate("text", x = 1:length(n_subcol_season$season),y =1.7, label = n_subcol_season$n, col="blue")

ggplot(ct_meas, aes(area))+
  geom_histogram()


# histogram of productivity estimates
# histogram of all data
ggplot(ct_meas, aes(prod))+
  geom_histogram()

ggplot(ct_meas, aes(ann_prod_anom))+
  geom_histogram()

ggplot(ct_meas, aes(prod))+
  geom_histogram(aes(colour = season)) +
  facet_wrap(~season)

# histogram of area by season
ggplot(ct_meas, aes(area))+
  geom_histogram(aes(colour = season)) +
  facet_wrap(~season)

# plot productivity vs area
ggplot(ct_meas, aes(area,prod))+
  geom_point() + 
  facet_wrap(~season)

ggplot(ct_meas, aes(perim, prod)) + 
  geom_point() +
  facet_wrap(~season)
ggplot(ct_meas, aes(perim, prod)) + 
  geom_point()

