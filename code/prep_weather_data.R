# Script to collate and format weather data
# Window of interest between mid-Nov to Jan 1

library(dplyr)
library(readr)
library(tidyr)
library(XLConnect)
library(stringr)
library(gridExtra)
library(ggplot2)

wind_dir <- c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW","NW","NNW")
spd <-30



# Read in weather data from 1415
c_wthr1415_wb <- loadWorkbook("Z:/Informatics/S031/S0311415/croz1415/weather/WTHR1415.xls")
c_wthr1415 <- readWorksheet(wthr1415_wb, sheet ="Weather")
c_wthr1415_summ <- c_wthr1415%>%
  dplyr::select(DATE, WINDMPH, GUSTMPH, DIR, LOWTEMP, NOTES)%>%
  mutate(DIR=factor(DIR,levels=wind_dir), DATE=as.Date(DATE), WINDMPH=as.numeric(WINDMPH), GUSTMPH=as.numeric(GUSTMPH),
         LOWTEMP= as.numeric(LOWTEMP), season="1415")%>%
  filter(DATE>as.Date("2014-11-27")&DATE<as.Date("2015-01-16"))

r_wthr1415_wb <- loadWorkbook("Z:/Informatics/S031/S0311415/royds1415/weather/WTHR1415.xls")
r_wthr1415 <- readWorksheet(wthr1415_wb, sheet ="Weather")
r_wthr1415_summ <- r_wthr1415%>%
  dplyr::select(DATE, WINDMPH, GUSTMPH, DIR, LOWTEMP, NOTES)%>%
  mutate(DIR=factor(DIR,levels=wind_dir), DATE=as.Date(DATE), WINDMPH=as.numeric(WINDMPH), GUSTMPH=as.numeric(GUSTMPH),
         LOWTEMP= as.numeric(LOWTEMP), season="1415")%>%
  filter(DATE>as.Date("2014-11-27")&DATE<as.Date("2015-01-16"))



# Read in weather data from 1516
c_wthr1516_wb <- loadWorkbook("Z:/Informatics/S031/S0311516/croz1516/weather/WHTR1516.xlsx")
c_wthr1516 <- readWorksheet(c_wthr1516_wb, sheet ="Sheet1")
c_wthr1516_summ <- c_wthr1516%>%
  dplyr::select(DATE,WINDMPH, GUSTMPH, DIR, LOWTEMP, NOTES)%>%
  mutate(DIR=factor(DIR,levels=wind_dir), DATE=as.Date(DATE),WINDMPH=as.numeric(WINDMPH),GUSTMPH=as.numeric(GUSTMPH),
         LOWTEMP= as.numeric(LOWTEMP), season="1516")%>%
  # replace 40+ values after lost wind gauge with 41
  mutate(GUSTMPH=ifelse(is.na(GUSTMPH)&is.na(WINDMPH),41,GUSTMPH))%>%
  filter(DATE>as.Date("2015-11-27")&DATE<as.Date("2016-01-16"))

c_wthr1516_summ%>%
  filter(is.na(GUSTMPH)&is.na(WINDMPH))


# Read in weather data from 1617
c_wthr1617_wb <- loadWorkbook("Z:/Informatics/S031/S0311617/croz1617/weather/WHTR1617.xlsx")
c_wthr1617 <- readWorksheet(c_wthr1617_wb, sheet ="Sheet1")
c_wthr1617_summ <- c_wthr1617%>%
  dplyr::select(DATE,WINDMPH, GUSTMPH, DIR, LOWTEMP, NOTES)%>%
  mutate(DIR=factor(DIR,levels=wind_dir), DATE=as.Date(DATE),WINDMPH=as.numeric(WINDMPH),GUSTMPH=as.numeric(GUSTMPH),
         LOWTEMP= as.numeric(LOWTEMP), season="1617")%>%
  filter(DATE>as.Date("2016-11-27")&DATE<as.Date("2017-01-16"))

# Read in weather data from 1718
c_wthr1718_wb <- loadWorkbook("Z:/Informatics/S031/S0311718/croz1718/weather/WHTR1718.xlsx")
c_wthr1718 <- readWorksheet(c_wthr1718_wb, sheet ="Sheet1")
c_wthr1718_summ <- c_wthr1718%>%
  dplyr::select(DATE, WINDMPH, GUSTMPH, DIR, LOWTEMP, NOTES)%>%
  mutate(DIR=factor(DIR,levels=wind_dir), DATE=as.Date(DATE),WINDMPH=as.numeric(WINDMPH),GUSTMPH=as.numeric(GUSTMPH), 
         LOWTEMP= as.numeric(LOWTEMP), season="1718")%>%
  filter(DATE>as.Date("2017-11-27")&DATE<as.Date("2018-01-16"))


# Join all
c_wthr_summ_all <- c_wthr1415_summ%>%
  full_join(c_wthr1516_summ)%>%
  full_join(c_wthr1617_summ)%>%
  full_join(c_wthr1718_summ)

write.csv(c_wthr_summ_all,"data/weather_summary_1415-1718.csv",row.names=FALSE)
  
# Plots of wind speed and direction ####

# Plot number of times each direction recorded
c_wthr_summ_all%>%
  group_by(season, DIR)%>%
  filter(!is.na(GUSTMPH))%>%
  summarise(perc_days=n()/nrow(.))%>%
dplyr::slice(match(wind_dir,DIR))%>%
  ggplot(aes(DIR, perc_days))+
  geom_bar(stat="identity")+
  facet_wrap(~season, nrow=4)

# Plot direction of high wind events
spd <-30
hwe<- c_wthr_summ_all%>%
  filter(GUSTMPH>spd)%>%
  group_by(season)%>%
  summarise(nobs=n())%>%
  ggplot(aes(season,nobs))+
  geom_bar(stat="identity")+
  ylab("High Wind Events \n(# obs of >30mph)")


# Plot of temperature
p_temp<-c_wthr_summ_all%>%
  group_by(DATE)%>%
  arrange(LOWTEMP)%>%
  dplyr::slice(1)%>%
  group_by(season)%>%
  ggplot(aes(season,LOWTEMP))+
           geom_boxplot()

temp <- c_wthr_summ_all%>%
  group_by(DATE)%>%
  arrange(LOWTEMP)%>%
  dplyr::slice(1)%>%
  group_by(season)

t.test(temp$LOWTEMP[temp$season=="1415"],temp$LOWTEMP[temp$season=="1516"]) # significant
t.test(temp$LOWTEMP[temp$season=="1516"],temp$LOWTEMP[temp$season=="1617"])
t.test(temp$LOWTEMP[temp$season=="1617"],temp$LOWTEMP[temp$season=="1718"])
t.test(temp$LOWTEMP[temp$season=="1516"],temp$LOWTEMP[temp$season=="1718"])
t.test(temp$LOWTEMP[temp$season=="1415"],temp$LOWTEMP[temp$season=="1617"])
t.test(temp$LOWTEMP[temp$season=="1415"],temp$LOWTEMP[temp$season=="1718"]) # significant


# Number of observations with snow

snow_days <- c_wthr_summ_all%>%
  filter(str_detect(NOTES, "snow|Snow|SNOW"))%>%
  group_by(season, DATE)%>%
  dplyr::slice(1)%>%
  group_by(season)%>%
  summarise(snow_days=n())%>%
  ggplot(aes(season,snow_days))+
  geom_bar(stat="identity")


grid.arrange(grobs=list(hwe,snow_days, p_temp), nrow=3)

