# Create table with BQI scores up to 2016 (including result of 2015) for GDR effects analysis

# library(gdata)
library(tidyverse)
library(fuzzyjoin)

# Read in BQI data ####
# Croz BQI

c_bqi <- read.csv("Z:/Informatics/S031/analyses/BQI/BQI_inclcurseas_1998to2017_forAnnie.csv")%>%
  rename(bird_id=Bandnumb)%>%
  rename(cohort=Cohort,season=Season)%>%
  select(bird_id,BQI,season,cohort,Orig_col,Br_col)

# Royds BQI
r_bqi<- read.csv("Z:/Informatics/S031/analyses/BQI/Royds_BQI_inclcurseas_1994to2017_forAnnie.csv")%>%
  rename(bird_id=Bandnumb)%>%
  rename(cohort=Cohort,season=Season)%>%
  select(bird_id,BQI,season,cohort,Orig_col,Br_col)
# some birds missing BQI values
# royds 63137 was deployed on the first year it was seen so did not have a bqi score yet
# royds 62820 was deployed on the first year it was seen so did not have bqi score yet
# royds 62848 was deployed on the first year it was seen so did not have bqi score yet

# join BQI tables
BQI_2015 <-c_bqi%>%
  full_join(r_bqi)%>%
  rename(bqi=BQI,orig_col=Orig_col,br_col=Br_col)%>%
  mutate(age=season-cohort)%>%
  group_by(bird_id)%>%
  filter(season>2012&season<2017)

# write BQI
write.csv(BQI_2015, "Z:/Informatics/S031/analyses/GDR/data/croz_royds_bqi_2013_2016.csv")

# add column for GDR birds ??
# read GDR deploy table
gdr_depl <- read.csv("Z:/Informatics/S031/analyses/GDR/data/croz_royds_gdr_depl_all.csv")%>%
  filter(study=="KA")%>%
  group_by(bird_id)%>%
  slice(1)%>%
  select(bird_id,orig_col,br_col)%>%
  mutate(gdr=1)

# join to BQI
bqi_gdr <- BQI_2015%>%
  full_join(gdr_depl)%>%
  mutate(gdr=factor(ifelse(is.na(gdr),0,gdr)))

bqi_gdr%>%group_by(bird_id)

t=filter(bqi_gdr,gdr==1)

hist(t$BQI)
 
t2 = filter(bqi_gdr,is.na(gdr))
hist(t2$BQI)


setdiff(bqi_gdr$bird_id,BQI_2015$bird_id)
bqi_gdr%>%
  group_by(bird_id)%>%
  slice(n())%>%
  filter(season==2016)%>%
  group_by(gdr)%>%
  summarise(mean_age=mean(age), n=n())

bqi_gdr%>%
  group_by(bird_id)%>%
  arrange(season)%>%
  filter(gdr==1,season>2015,is.na(bqi))

%>%
ggplot(aes(age,fill=gdr,alpha=0.5))+
  geom_density()
