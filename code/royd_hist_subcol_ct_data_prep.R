# R code for collating subcolony adult and nest counts from Cape Royds
# Version 1 initiated 07/05/16 by A. Schmidt


#-----------------------------------------------------------------------------------

# Libraries----
library("XLConnect")
library(dplyr)
library(tidyr)

# Start loading in data----
# want to end up with one table for chick counts and one table for adult counts 
# chick count table will have the following columns:
# c("col","season", "date", "subcol","ch_ct", "notes")
# adult count table will have 8 columns:
# c("col","season", "date", "", "subcol" "active_ct", "occ_ct","total_ct", "notes")


# # load excel workbook with data from 9697-0102 multiple sheets
# wb_1 = loadWorkbook("Z:/Informatics/S031/AllData/chickcount/r/Roydschick-num.xls")
# 
# #9601
# # something funky going on with data in this sheet. Leaving it out until can consult with Grant
# ct_9601 <- readWorksheet(wb_1, sheet = "Sheet1", colTypes = c("character", "numeric","numeric","numeric", "numeric","numeric", "numeric", "numeric","numeric"))
# 
# 
# # 0203
# wb_2 <- loadWorkbook("Z:/Informatics/S031/AllData/chickcount/r/Nest&ChickCount0203.xls")
# 
# ct_0203 <- readWorksheet(wb_2, sheet="Sheet1", colTypes = c("character","character", rep("numeric",11)),startRow =2)
# #also some funkiness going on here
# # Not clear whether territories refers to active or occupied (suspect occupied)
# # Leaving this year out for now

# 0304
wb_3 <- loadWorkbook("Z:/Informatics/S031/AllData/chickcount/r/Nest&ChickCount0304.xls")
ct_0304 <- readWorksheet(wb_3, sheet="Sheet1", colTypes = c("character","character", rep("numeric",17)),startRow =2)
names(ct_0304)

# chick count done on 1/22/04
# convert all letters in df to lower case
ct_0304_format <- ct_0304 %>% 
  select(SUBCOL,Active.N,Chicks.3)%>%
  mutate(col="royd", SUBCOL = tolower(SUBCOL), season = "0304", notes = as.character(NA)) %>%
  rename(subcol=SUBCOL, active_ct=Active.N, ch_ct=Chicks.3)%>%
  filter(!is.na(subcol))
names(ct_0304_format)

# # 0405
# wb_4 <- loadWorkbook("Z:/Informatics/S031/AllData/chickcount/r/chick counts_04.xls")
# ct_0405 <- readWorksheet(wb_4, sheet = "chick counts", colTypes = c("POSIXct", "character", "character",rep("numeric",5)))
# names(ct_0405)
# 
# chct_0405 <- ct_0405%>%
#   select(Date,Subcolony,Approx....Chicks)%>%
#   mutate(col="royd",Subcolony = tolower(Subcolony),notes=as.character(NA))%>%
#   rename(subcol=Subcolony,ch_ct = Approx....Chicks)%>%
#   filter(!is.na(subcol))
# # only occupied territories counted on 12/21/04
# adct_0405 <- ct_0405%>%
#   select(Subcolony,Occ.Terr.21.Dec)%>%
#   mutate(col="royd",Subcolony = tolower(Subcolony),date=as.POSIXct("2004-12-21"),notes=as.character(NA))%>%
#   rename(subcol=Subcolony,occ_ct = Occ.Terr.21.Dec)%>%
#   filter(!is.na(subcol))

#0506: Counts approximate this year
wb_5 <- loadWorkbook("Z:/Informatics/S031/AllData/chickcount/r/chick counts_0506.xls")
ct_0506 <- readWorksheet(wb_5, sheet = "chick counts", colTypes = c("POSIXct", "character", "character",rep("numeric",3)))

ct_0506_format <- ct_0506 %>%
  select(Subcolony,Approx....nests, Approx....Chicks)%>%
  mutate(col="royd",season="0506",Subcolony = tolower(Subcolony),notes=as.character(NA))%>%
  rename(subcol=Subcolony,active_ct=Approx....nests,ch_ct = Approx....Chicks)%>%
  filter(!is.na(subcol))


#0607
wb_6 <- loadWorkbook("Z:/Informatics/S031/AllData/chickcount/r/R Adult-chickCounts0607.xls")
ct_0607 <- readWorksheet(wb_6, sheet = "Sheet1", colTypes = c("character",rep("numeric",5)),startRow=3)

ct_0607_format <- ct_0607 %>%
  select(Subcolony,Nest,chicks)%>%
  mutate(col="royd",season="0607",notes=as.character(NA))%>%
  rename(subcol=Subcolony,active_ct=Nest,ch_ct = chicks)%>%
  filter(!is.na(subcol))


#0708: Unclear if Terr refers to occupied or active. Assuming active
wb_7 <- loadWorkbook("Z:/Informatics/S031/AllData/chickcount/r/R Nest-chickCount 0708.xls")
ct_0708 <- readWorksheet(wb_7, sheet = "Sheet1", colTypes = c("character",rep("numeric",5)),startRow=2)

ct_0708_format <- ct_0708 %>%
  select(SUBCOL,Terr,Col4)%>%
  mutate(col="royd",season="0708",notes=as.character(NA))%>%
  rename(subcol=SUBCOL,active_ct=Terr,ch_ct = Col4)%>%
  filter(!is.na(subcol))



#0809: Unclear if Terr refers to occupied or active. Assuming active
wb_8 <- loadWorkbook("Z:/Informatics/S031/AllData/chickcount/r/Nest-chickCount 0809.xls")
ct_0809 <- readWorksheet(wb_8, sheet = "Sheet1", colTypes = c("character",rep("numeric",5)),startRow=3)

ct_0809_format <- ct_0809 %>%
  select(SUBCOL,Terr,Col4)%>%
  mutate(col="royd",season="0809",notes=as.character(NA))%>%
  rename(subcol=SUBCOL,active_ct=Terr,ch_ct = Col4)%>%
  filter(!is.na(subcol))


#0910
wb_9 <- loadWorkbook("Z:/Informatics/S031/AllData/chickcount/r/Nest-chickCount 0910.xls")
ct_0910 <- readWorksheet(wb_9, sheet = "0910", colTypes = c("character",rep("numeric",5)),startRow=3)

ct_0910_format <- ct_0910 %>%
  select(SUBCOL,Active,chicks)%>%
  mutate(col="royd",season="0910",notes=as.character(NA))%>%
  rename(subcol=SUBCOL,active_ct=Active,ch_ct = chicks)%>%
  filter(!is.na(subcol))



#1011
wb_10 <- loadWorkbook("Z:/Informatics/S031/AllData/chickcount/r/Royds Annual Ground Count 1011 1129.xls")
ct_1011 <- readWorksheet(wb_10, sheet = "11_29_10", colTypes = c("character",rep("numeric",6)),startRow=4)

ct_1011_format <- ct_1011 %>%
  select(Subcolony,Active.Territories,Active.Territories.1,Col7)%>%
  mutate(col="royd",season="1011",notes=as.character(NA), active_ct=rowMeans(.[,2:3], na.rm=TRUE))%>%
  rename(subcol=Subcolony,ch_ct = Col7)%>%
  select(-Active.Territories,-Active.Territories.1)%>%
  filter(!is.na(subcol))


#1112
wb_11 <- loadWorkbook("Z:/Informatics/S031/AllData/chickcount/r/Royds Annual Ground Count 1112 12_01_11.xlsx")
ct_1112 <- readWorksheet(wb_11, sheet = "12_01_11", colTypes = c("character",rep("numeric",8)),startRow=5)

ct_1112_format <- ct_1112 %>%
  select(Subcolony,Active.Territories.1,Col7)%>%
  mutate(col="royd",season="1112",notes=as.character(NA))%>%
  rename(subcol=Subcolony,active_ct=Active.Territories.1,ch_ct = Col7)%>%
  filter(!is.na(subcol))


#1213
wb_12 <- loadWorkbook("Z:/Informatics/S031/AllData/chickcount/r/Royds Annual Ground Count 1213.xlsx")
ct_1213 <- readWorksheet(wb_12, sheet = "11_28_12", colTypes = c("character",rep("numeric",9)),startRow=5)

ct_1213_format <- ct_1213 %>%
  select(Subcolony,Active.Territories,Col6)%>%
  mutate(col="royd",season="1213",notes=as.character(NA))%>%
  rename(subcol=Subcolony,active_ct=Active.Territories,ch_ct = Col6)%>%
  filter(!is.na(subcol))

#1314: no chick count this year
wb_13 <- loadWorkbook("Z:/Informatics/S031/AllData/chickcount/r/Royds Annual Ground Count Seas10_11_13_120113 (2).xlsx")
ct_1314 <- readWorksheet(wb_13, sheet = "12_01_13", colTypes = c("character",rep("numeric",7)),startRow=5)

ct_1314_format <- ct_1314 %>%
  select(Subcolony,Active)%>%
  mutate(col="royd",season="1314",notes=as.character(NA))%>%
  rename(subcol=Subcolony,active_ct=Active)%>%
  filter(!is.na(subcol))


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


r_ct_all<- ct_0304_format%>%
  full_join(ct_0506_format)%>%
  full_join(ct_0607_format)%>%
  full_join(ct_0708_format)%>%
  full_join(ct_0809_format)%>%
  full_join(ct_0910_format)%>%
  full_join(ct_1011_format)%>%
  full_join(ct_1112_format)%>%
  full_join(ct_1213_format)%>%
  full_join(ct_1314_format)%>%
  full_join(ct_1415_format)%>%
  full_join(ct_1516_format)%>%
  full_join(ct_1617_format)%>%
  full_join(ct_1718_format)%>%
  arrange(subcol)

# Clean up joined table
r_ct_all_cln <- r_ct_all%>%
  mutate(subcol=tolower(subcol),prod=ch_ct/active_ct,col="royds",
  subcol=plyr::mapvalues(subcol, from=c("11/12","14a/b","19 (wb)","19 (wb1)","19a/b (wb)","1a+b","1ab","2ab","3a+b","3ab","4+17","5+18","6+8a","7+8b","8 (reference)","9+10","9s + 13"),
                          to = c("11-12","14a-b","19","19","19","1a-b","1a-b","2a-b","3a-b","3a-b","4-17","5-18","6-8a","7-8b","8", "9-10","9s-13")))%>%
  filter(!str_length(subcol)>5,!subcol%in%c("mean","total"),!is.na(prod),!prod>2)%>%
  arrange(subcol,season)%>%
  select(-notes,-area_name)



# Write table
write.csv(r_ct_all_cln,"data/royds_all_ct_clean_thru1718.csv", row.names = FALSE)


