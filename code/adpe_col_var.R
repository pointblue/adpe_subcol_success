library(tidyverse)
d <- read.csv("C:/Users/aschmidt/Dropbox/Antarctica/data/Lynch_and_LaRue_adelie_census.csv")

d%>%
  ggplot(aes(Current.abundance))+
  geom_histogram(bins=30)+
  xlim(0,400000)


# number of indiviudals in colonies with <3000
d%>%filter(Current.abundance<3000)%>%
  summarise(n=n(),total=sum(Current.abundance), fraction=n/nrow(d))

d%>%filter(!is.na(Current.abundance))%>%
  summarise(n=n())
# percent of world population in small colonies
92984/sum(d$Current.abundance, na.rm=TRUE) # 0.0265292

# percent of world population that is Crozier
300000/sum(d$Current.abundance, na.rm=TRUE)

300000/3790000
100000/3800000


summarise(d, total=sum(Current.abundance, na.rm=TRUE))

d%>%
  mutate(log_size=log(Current.abundance))%>%
  ggplot(aes(log_size))+
  geom_histogram(bins=10)
