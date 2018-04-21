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


# load excel workbook with data from 9697-0102 multiple sheets
wb_1 = loadWorkbook("Z:/Informatics/S031/AllData/chickcount/r/Roydschick-num.xls")

#9601
# something funky going on with data in this sheet. Leaving it out until can consult with Grant
ct_9601 <- readWorksheet(wb_1, sheet = "Sheet1", colTypes = c("character", "numeric","numeric","numeric", "numeric","numeric", "numeric", "numeric","numeric"))


# 0203
wb_2 <- loadWorkbook("Z:/Informatics/S031/AllData/chickcount/r/Nest&ChickCount0203.xls")

ct_0203 <- readWorksheet(wb_2, sheet="Sheet1", colTypes = c("character","character", rep("numeric",11)),startRow =2)
#also some funkiness going on here
# Not clear whether territories refers to active or occupied (suspect occupied)
# Leaving this year out for now

# 0304
wb_3 <- loadWorkbook("Z:/Informatics/S031/AllData/chickcount/r/Nest&ChickCount0304.xls")
ct_0304 <- readWorksheet(wb_3, sheet="Sheet1", colTypes = c("character","character", rep("numeric",17)),startRow =2)
names(ct_0304)

# chick count done on 1/22/04
# convert all letters in df to lower case
chct_0304 <- ct_0304 %>% 
  select(SUBCOL,Chicks.3) %>%
  mutate(col="royd", SUBCOL = tolower(SUBCOL), season = "0203", date = as.POSIXct("2004-01-22"), notes = as.character(NA)) %>%
  rename(subcol=SUBCOL, ch_ct=Chicks.3)%>%
  filter(!is.na(subcol))
names(chct_0304)
# adult count done on 12/05/2003
adct_0304 <- ct_0304%>%
  select(SUBCOL,Active.N, Terr)%>%
  mutate(col="royd", SUBCOL = tolower(SUBCOL), season = "0203", date = as.POSIXct("2003-12-05"), notes = as.character(NA), total_ct = as.character(NA)) %>%
  rename(subcol=SUBCOL, active_ct=Active.N,occ_ct=Terr)%>%
  filter(!is.na(subcol))

# 0405
wb_4 <- loadWorkbook("Z:/Informatics/S031/AllData/chickcount/r/chick counts_04.xls")
ct_0405 <- readWorksheet(wb_4, sheet = "chick counts", colTypes = c("POSIXct", "character", "character",rep("numeric",5)))
names(ct_0405)

chct_0405 <- ct_0405%>%
  select(Date,Subcolony,Approx....Chicks)%>%
  mutate(col="royd",Subcolony = tolower(Subcolony),notes=as.character(NA))%>%
  rename(subcol=Subcolony,ch_ct = Approx....Chicks)%>%
  filter(!is.na(subcol))
# only occupied territories counted on 12/21/04
adct_0405 <- ct_0405%>%
  select(Subcolony,Occ.Terr.21.Dec)%>%
  mutate(col="royd",Subcolony = tolower(Subcolony),date=as.POSIXct("2004-12-21"),notes=as.character(NA))%>%
  rename(subcol=Subcolony,occ_ct = Occ.Terr.21.Dec)%>%
  filter(!is.na(subcol))

#0506
wb_5 <- loadWorkbook("Z:/Informatics/S031/AllData/chickcount/r/chick counts_0506.xls")
ct_0506 <- readWorksheet(wb_5, sheet = "chick counts", colTypes = c("POSIXct", "character", "character",rep("numeric",3)))

chct_0506 <- ct_0506 %>%
  select(Date, Subcolony,Approx....Chicks)%>%
  mutate(col="royd",Subcolony = tolower(Subcolony),notes=as.character(NA))%>%
  rename(subcol=Subcolony,ch_ct = Approx....Chicks)%>%
  filter(!is.na(subcol))

adct_0506 <- ct_0506%>%
  select(Subcolony,Occ.Terr.21.Dec)%>%
  mutate(col="royd",Subcolony = tolower(Subcolony),date=as.POSIXct("2004-12-21"),notes=as.character(NA))%>%
  rename(subcol=Subcolony,occ_ct = Occ.Terr.21.Dec)%>%
  filter(!is.na(subcol))

#1415
# Think count for 14a/b is really just count for 14
