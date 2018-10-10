

bqi_pred_prod <- read.csv("Z:/Informatics/S031/analyses/aschmidt/subcol_var/data/croz_bqi_all_pred_prod_v2.csv")%>%
  arrange(Bandnumb)%>%
  filter(!GRID_CODE==0,Br_exp>2,Season%in%c(2014,2015,2016,2017),Orig_col=="CROZ",!Device==1)%>%
  rename(hab_qual=GRID_CODE)%>%
  group_by(Bandnumb)%>%
  arrange(Season)%>%
  dplyr::slice(n())

bqi_pred_prod%>%
  summarise(n())

# min_bqi=min(bqi_pred_prod$BQI_A)
# range_bqi=max(bqi_pred_prod$BQI_A)-min(bqi_pred_prod$BQI_A)
# bqi_cuts=range_bqi/3


bqi_quant=quantile(bqi_pred_prod$BQI_A, probs = seq(0, 1, 0.25),na.rm=TRUE)
# low_break=bqi_pred_prod[58,"BQI_A"]
# mid_break=bqi_pred_prod[115,"BQI_A"]

bqi_pred_prod<- bqi_pred_prod%>%
  mutate(bqi_cat=ifelse(BQI_A<bqi_quant[2],"l",ifelse(BQI_A<bqi_quant[4],"m","h")))

# mutate(bqi_cat[1:57]="l"))
# # mutate(bqi_cat=ifelse(BQI_A<0,"l","h"))%>%
# mutate(bqi_cat=ifelse(BQI_A>(-0.05381)&BQI_A<( 0.18842),"m",NA))%>%
# mutate(bqi_cat=ifelse(BQI_A<(-0.05381),"l",bqi_cat))%>%
# mutate(bqi_cat=ifelse(BQI_A>( 0.18842),"h",bqi_cat))%>%

# Overall correlation between BQI and predicted breeding success (habitat quality)
rcorr(bqi_pred_prod$BQI_A, bqi_pred_prod$hab_qual)
library(viridis)
ggplot(bqi_pred_prod,aes(hab_qual,BQI_A))+
  geom_point(aes(colour=Br_exp))+geom_smooth(method=lm)+
  xlab("habitat quality")+
  scale_color_viridis() 


ggplot(bqi_pred_prod,aes(hab_qual,BQI_A, color=bqi_cat))+
  geom_point()+geom_smooth(method=lm)+
  xlab("habitat quality")


# low BQI only
low_bqi <- bqi_pred_prod%>%
  filter(bqi_cat=="l")
rcorr(low_bqi$BQI_A, low_bqi$hab_qual)

m_lbqi_hab_qual <- lm(BQI_A~hab_qual*Br_exp,data=low_bqi)
summary(m_lbqi_hab_qual)
m2 <- lm(BQI_A~hab_qual,data=low_bqi)
summary(m2)
library(bbmle)
AICtab(m_lbqi_hab_qual,m2)

# High quality birds only
high_bqi <- bqi_pred_prod%>%
  filter(bqi_cat=="h")
rcorr(high_bqi$BQI_A, high_bqi$hab_qual)


# Plot of BQI by habitat quality by group
bqi_pred_prod%>%
  group_by(bqi_cat)%>%
  summarise(pred_mean=mean(hab_qual),ci=1.96*sd(hab_qual)/sqrt(n()),n=n())%>%
  ggplot(aes(bqi_cat,pred_mean, fill=bqi_cat))+
  geom_bar(stat="identity")+ 
  geom_errorbar(aes(ymin=pred_mean - ci,ymax=pred_mean+ci),width=0.2)+
  xlab("breeding quality")

