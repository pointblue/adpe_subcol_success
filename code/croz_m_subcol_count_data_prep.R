
# R code for collating subcolony adult and nest counts
# starting off with this script just dealing with Cape Crozier data
# hope to add Cape Royds later
# Version 1 initiated 5/24/16

#--- for 1112 season, started looking in annual folders for raw data because that is when they
# appeared to start entering all ...

#--- see chickcomparison sheet for notes on subcolony numbers. Seems there was some
# discrepancy between counts in 0203 and 0304


#-------------------------------------------------------------------------------
# Libraries
library("XLConnect")
library(dplyr)
library(tidyr)

# load excel workbook with data from 1996-2012 multiple sheets
wb = loadWorkbook("Z:/Informatics/S031/AllData/chickcount/c/chick counts_00_12.xlsx")

# want to end up with one table for chick counts and one table for adult counts 
# chick count table will have the following columns:
# c("col","season", "date", "subcol","ch_ct", "notes")
# adult count table will have:
# c("col","season", "date", "", "subcol" "active_ct", "occ_ct","total_ct", "notes")



#read in sheet with chick count data from 96-00 ####
# in this sheet "Colony" = "subcol"
chct_9600 <- readWorksheet(wb, sheet = "DA summary", colTypes = c("character", "numeric","numeric","numeric", "numeric","numeric"))
# clean up worksheet
chct_9600<-chct_9600 %>% 
  filter(!is.na(Colony))%>%
  select(Colony, Count.96, Count.97, Count.98, Count.99, Count.2000)
# rename columns
colnames(chct_9600) <- c("subcol", "9596", "9697", "9798", "9899", "9900")
# compile columns for diff years into single column
chct_9600_long <- gather(chct_9600,key = season, value = ch_ct, -subcol)
names(chct_9600_long)
# add columns for col, date
chct_9600_format <- chct_9600_long %>%
  mutate(col="croz", date=as.POSIXct(NA), notes = as.character(NA))

#adult count 9899 and 9900
# I think this file has counts from the aerial photos for M subcolonies but would be good to confirm
# not clear if count is active or occupied. Probably occupied but going to used as active since it's the only count I have
# aerial photos were taken on 12/1/98 (according to ""Z:\Informatics\S031\analyses\ColonySize\AerialCounts_110608.xls")
wb_0 <- loadWorkbook("Z:/Informatics/S031/analyses/aerial counts/dga.xls")
adct_9899 <- readWorksheet(wb_0, sheet = 1, colTypes = c("character", "numeric","character","character", "numeric"))
names(adct_9899)
# c("col","season", "date", "subcol" "active_ct", "occ_ct","total_ct", "notes")

adct_9899_format <- adct_9899 %>%
  select(Cape.Crozier, X1998)%>%
  mutate(col = "croz", season = "9899",date = as.POSIXct("1998-12-01"),occ_ct = NA, total_ct  = NA)%>%
  rename(subcol = Cape.Crozier,active_ct = X1998)%>%
  filter(!is.na(subcol), !subcol=="subcolony")


# read in next sheet with data from 0001 ####
chct_0001 <-readWorksheet(wb, sheet = "Croz0001")
# add columns for season, and date, remove column with "code", remove rows with no subcol designation
chct_0001_format <- chct_0001 %>%
  select(Subcol,num,Code) %>%
  mutate(season = "0001", date = as.POSIXct(NA), col = "croz", notes=Code)%>%
  filter(!is.na(Subcol))%>%
  select(-Code) %>%
  rename(subcol = Subcol, ch_ct = num)


#### no data from 0102 ####



# read in sheet from different workbook with data from 0203 ####
# notes from sheet:
# Chicks counted 1/16/2003					Used  blue subcolony numbers off 2001 photo and black numbers off the older overview photo
# Adults counted 12/2/2002					

wb_1 <- loadWorkbook("Z:/Informatics/S031/S0310203/croz0203/chick counts/chick-num.xlsx")
chct_0203 <-readWorksheet(wb_1, sheet = "Croz0203", startRow=5)
# two entries for subcol 30. Other version of this file (-dga), has first one as 38 so changing that here
chct_0203$Subcol[chct_0203$Subcol=="30"& chct_0203$Chick.Count.1.16.2003==57] <- "38"
# total for subcol 17 should be 306 (according to raw data sheet) and it should be renamed 15-17 (they merged)
chct_0203$Chick.Count.1.16.2003[chct_0203$Subcol=="17"] <-306
chct_0203$Subcol[chct_0203$Subcol=="17"] <-"15-17"
# Total for subcol 31 should be 222
chct_0203$Chick.Count.1.16.2003[chct_0203$Subcol==31] <-222
# add columns for season and date of count and remove rows with no subcol designation
# colnames <- c("col","season", "date", "subcol","ch_ct")
chct_0203_format <- chct_0203 %>% 
  select(Subcol,Chick.Count.1.16.2003, Notes) %>%
  mutate(col = "croz",season = "0203", date = as.POSIXct("2003-01-16")) %>%
  filter(!is.na(Subcol))%>%
  rename(subcol=Subcol, ch_ct=Chick.Count.1.16.2003, notes=Notes) 

# a few subcol with adult counts from 0203
adct_0203 <- chct_0203
names(adct_0203)
# c("col","season", "date", "subcol" "active_ct", "occ_ct","total_ct", "notes")
# replace first subcol==30 with subcol==38
adct_0203$Subcol[adct_0203$Subcol==30&adct_0203$Chick.Count.1.16.2003==57] <- 38
adct_0203_format <- adct_0203 %>%
  select(Subcol, Num.Adults.12.2.2002, Num.active.nests.12.2.2002,Notes)%>%
  mutate(col = "croz", season = "0203",date = as.POSIXct("2002-12-02"),occ_ct = NA)%>%
  rename(subcol = Subcol,active_ct = Num.active.nests.12.2.2002, total_ct = Num.Adults.12.2.2002)%>%
  filter(!is.na(subcol))


# read in next sheet with data from 0304 ####
chct_0304 <-readWorksheet(wb, sheet = "Croz0304", startRow=3)
names(chct_0304)

chct_0304_format <- chct_0304 %>%
  select(subcolony, chicks.1.16.2004,notes)%>%
  mutate(col = "croz", season = "0304",date = as.POSIXct("2004-01-16"))%>%
  rename(subcol = subcolony,ch_ct = chicks.1.16.2004)%>%
  filter(!is.na(subcol))
# adult counts contained in same sheet, need to make separate table
adct_0304 <-chct_0304
names(adct_0304)
# c("col","season", "date", "subcol" "active_ct", "occ_ct","total_ct", "notes")
adct_0304_format <- adct_0304 %>%
  select(subcolony, adults.12.4.2003, active.territories.12.4.2003,notes)%>%
  mutate(col = "croz", season = "0304",date = as.POSIXct("2003-12-04"),occ_ct = NA)%>%
  rename(subcol = subcolony,active_ct = active.territories.12.4.2003, total_ct = adults.12.4.2003)%>%
  filter(!is.na(subcol))


# read in data from 0405 (skip sheet 5 which is a comparison sheet) ####
# "Adult and Chick Counts, Crozier 0405 - Dec. 3, 2004, and Jan 12-13, 2005, area M - using black numbers off overview photo
# Match with GIS map of Area M." (copied from sheet)
chct_0405 <-readWorksheet(wb, sheet = "Croz0405", startRow = 5)
names(chct_0405)
chct_0405_format <- chct_0405 %>%
  select(subcolony, chicks.Jan.12.13.2005,notes)%>%
  mutate(col = "croz", season = "0405",date = as.POSIXct("2005-01-12"))%>%
  rename(subcol = subcolony,ch_ct = chicks.Jan.12.13.2005)%>%
  filter(!is.na(subcol))
# adult counts contained in same sheet, need to make separate table
adct_0405 <-chct_0405
names(adct_0405)
# c("col","season", "date", "subcol" "active_ct", "occ_ct","total_ct", "notes")
adct_0405_format <- adct_0405 %>%
  select(subcolony, total.adults12.3.04, active.12.3.04,notes)%>%
  mutate(col = "croz", season = "0405",date = as.POSIXct("2004-12-03"),occ_ct = NA)%>%
  rename(subcol = subcolony,active_ct = active.12.3.04, total_ct = total.adults12.3.04)%>%
  filter(!is.na(subcol))

# data from 0506 ####
# "Adult and Chick Counts, Crozier 0506, using black numbers off overview photo	
# Dates	
# Adults	Nov. 30, 2005
# Chicks	Jan. 09, 2006
# Location	Area M (match with GIS map of area M)" (copied from sheet)

# read in data from 0506
chct_0506 <-readWorksheet(wb, sheet = "Croz0506",startRow = 8,
                          colTypes = c("character", "numeric","numeric","numeric","numeric",
                                       "numeric","numeric","numeric","numeric","numeric",
                                       "character"))
names(chct_0506)
chct_0506_format <- chct_0506 %>%
  select(subcolony, chicks.01.09.2006,notes)%>%
  mutate(col = "croz", season = "0506",date = as.POSIXct("2006-01-09"))%>%
  rename(subcol = subcolony,ch_ct = chicks.01.09.2006)%>%
  filter(!is.na(subcol))
# adult counts contained in same sheet, need to make separate table
adct_0506 <-chct_0506
names(adct_0506)
# c("col","season", "date", "subcol" "active_ct", "occ_ct","total_ct", "notes")
adct_0506_format <- adct_0506 %>%
  select(subcolony, adults.11.30.05, active.11.30.05,notes)%>%
  mutate(col = "croz", season = "0506",date = as.POSIXct("2005-11-30"),occ_ct = NA)%>%
  rename(subcol = subcolony,active_ct = active.11.30.05, total_ct = adults.11.30.05)%>%
  filter(!is.na(subcol))

#0607 ####
# chick couts are average of 2-4 counts
# read in data from 0607
chct_0607 <-readWorksheet(wb, sheet = "Croz0607",startRow = 8,
                          colTypes = c("character", "numeric","numeric","numeric","numeric",
                                        "numeric","numeric","numeric","numeric","numeric",
                                                                  "character"))
names(chct_0607)
chct_0607_format <- chct_0607 %>%
  select(subcolony, chicks.01.08.2007,notes)%>%
  mutate(col = "croz", season = "0607",date = as.POSIXct("2007-01-08"))%>%
  rename(subcol = subcolony,ch_ct = chicks.01.08.2007)%>%
  filter(!is.na(subcol))
# adult counts contained in same sheet, need to make separate table
# adult counts not averaged (single count)
adct_0607 <-chct_0607
names(adct_0607)
# c("col","season", "date", "subcol" "active_ct", "occ_ct","total_ct", "notes")
adct_0607_format <- adct_0607 %>%
  select(subcolony, adults.11.29.06, active.11.29.06,notes)%>%
  mutate(col = "croz", season = "0607",date = as.POSIXct("2006-11-29"),occ_ct = NA)%>%
  rename(subcol = subcolony,active_ct = active.11.29.06, total_ct = adults.11.29.06)%>%
  filter(!is.na(subcol))


#0708 ####
# chick couts are mostly an average of 3 counts
# read in data from 0607
chct_0708 <-readWorksheet(wb, sheet = "Croz0708",startRow = 2)
names(chct_0708)
chct_0708_format <- chct_0708 %>%
  select(subcolony, chicks.01.13.2008,notes)%>%
  mutate(col = "croz", season = "0708",date = as.POSIXct("2008-01-13"))%>%
  rename(subcol = subcolony,ch_ct = chicks.01.13.2008)%>%
  filter(!is.na(subcol))
# adult counts contained in same sheet, need to make separate table
# adult counts average of 2-4 counts
adct_0708 <-chct_0708
names(adct_0708)
# c("col","season", "date", "subcol" "active_ct", "occ_ct","total_ct", "notes")
adct_0708_format <- adct_0708 %>%
  select(subcolony, adults.11.30.07, active.11.30.07,notes)%>%
  mutate(col = "croz", season = "0708",date = as.POSIXct("2007-11-30"),occ_ct = NA)%>%
  rename(subcol = subcolony,active_ct = active.11.30.07, total_ct = adults.11.30.07)%>%
  filter(!is.na(subcol))


#0809 ####
# chick couts are from single count
# read in data from 0809
chct_0809 <-readWorksheet(wb, sheet = "Croz0809",startRow = 2,
                          colTypes = c("character", "numeric","numeric","numeric","numeric",
                                       "numeric","numeric","numeric","numeric","numeric","numeric",
                                       "character"))
names(chct_0809)
chct_0809_format <- chct_0809 %>%
  select(subcolony, chicks.01.12.2009,notes)%>%
  mutate(col = "croz", season = "0809",date = as.POSIXct("2009-01-12"))%>%
  rename(subcol = subcolony,ch_ct = chicks.01.12.2009)%>%
  filter(!is.na(subcol))
# adult counts contained in same sheet, need to make separate table
# adult counts are from single count
adct_0809 <-chct_0809
names(adct_0809)
# c("col","season", "date", "subcol" "active_ct", "occ_ct","total_ct", "notes")
adct_0809_format <- adct_0809 %>%
  select(subcolony, adults.11.27.08, active.11.27.08,notes)%>%
  mutate(col = "croz", season = "0809",date = as.POSIXct("2008-11-27"),occ_ct = NA)%>%
  rename(subcol = subcolony,active_ct = active.11.27.08, total_ct = adults.11.27.08)%>%
  filter(!is.na(subcol))


#0910 ####
# chick couts are from single count
# read in data from 0809
chct_0910 <-readWorksheet(wb, sheet = "Croz0910",startRow = 2,
                          colTypes = c("character", "numeric","numeric","numeric","numeric",
                                       "numeric","numeric","numeric","numeric","numeric","numeric",
                                       "character"))
names(chct_0910)
chct_0910_format <- chct_0910 %>%
  select(subcolony, chicks.1.19.2010,notes)%>%
  mutate(col = "croz", season = "0910",date = as.POSIXct("2010-01-19"))%>%
  rename(subcol = subcolony,ch_ct = chicks.1.19.2010)%>%
  filter(!is.na(subcol))
# adult counts contained in same sheet, need to make separate table
# adult counts are average of 2-4 counts
adct_0910 <-chct_0910
names(adct_0910)
# c("col","season", "date", "subcol" "active_ct", "occ_ct","total_ct", "notes")
adct_0910_format <- adct_0910 %>%
  select(subcolony, adults.12.02.09, active.12.02.09,notes)%>%
  mutate(col = "croz", season = "0910",date = as.POSIXct("2009-12-02"),occ_ct = NA)%>%
  rename(subcol = subcolony,active_ct = active.12.02.09, total_ct = adults.12.02.09)%>%
  filter(!is.na(subcol))


#1011 ####
# read in data from 1011
chct_1011 <-readWorksheet(wb, sheet = "Croz1011",startRow = 2,colTypes = c("character", "numeric","numeric","numeric","numeric",
                                                                   "numeric","numeric","numeric","numeric","numeric","numeric",
                                                                   "character"))
names(chct_1011)
chct_1011_format <- chct_1011 %>%
  select(subcolony, chicks.1.16.2011,notes)%>%
  mutate(col = "croz", season = "1011",date = as.POSIXct("2011-01-16"))%>%
  rename(subcol = subcolony,ch_ct = chicks.1.16.2011)%>%
  filter(!is.na(subcol))
# adult counts contained in same sheet, need to make separate table
# adult counts are a single count
adct_1011 <-chct_1011
names(adct_1011)
# c("col","season", "date", "subcol" "active_ct", "occ_ct","total_ct", "notes")
adct_1011_format <- adct_1011 %>%
  select(subcolony, adults.11.26.10, active.11.26.10,notes)%>%
  mutate(col = "croz", season = "1011",date = as.POSIXct("2010-11-26"),occ_ct = NA)%>%
  rename(subcol = subcolony,active_ct = active.11.26.10, total_ct = adults.11.26.10)%>%
  filter(!is.na(subcol))


#1112 ####
# chick couts are average of an unknown number of counts
# read in data from 1112
# chct_1112 <-readWorksheet(wb, sheet = 13,startRow = 2)
# names(chct_1112)
# chct_1213_format <- chct_1112 %>%
#   select(subcolony, chicks,notes)%>%
#   mutate(col = "croz", season = "1112",date = as.POSIXct("2012-01-15"))%>%
#   rename(subcol = subcolony,ch_ct = chicks)%>%
#   filter(!is.na(subcol))
# adult counts contained in same sheet, need to make separate table
# adult counts are a single count?
# adct_1112 <-chct_1112
# names(adct_1112)
# # c("col","season", "date", "subcol" "active_ct", "occ_ct","total_ct", "notes")
# adct_1112_format <- adct_1112 %>%
#   select(subcolony, adults.12.1.11, active.12.1.11,Occupied.Territories.12.1.11,notes)%>%
#   mutate(col = "croz", season = "1112",date = as.POSIXct("2011-12-01"))%>%
#   rename(subcol = subcolony,active_ct = active.12.1.11,occ_ct= Occupied.Territories.12.1.11,total_ct = adults.12.1.11)%>%
#   filter(!is.na(subcol))

# import adult count from annual folder
# load workbook with 1112-1415 data

wb_2 <- loadWorkbook("Z:/Informatics/S031/S0311415/croz1415/chick counts/AdultAndChickCounts_CROZ_1415.xlsx")

# 1112 ####
adct_1112 <-readWorksheet(wb_2, sheet = "Adults 120111",startRow = 1)
names(adct_1112)
# need to average counts
adct_1112_format <- adct_1112 %>%
  group_by(Subcolony, Date)%>%
  select(Subcolony, Date, TotalIndividuals, OccupiedTerritories, ActiveNests)%>%
  summarise(
    total_ct=mean(TotalIndividuals, na.rm = TRUE),
    active_ct = mean(ActiveNests, na.rm = TRUE),
    occ_ct = mean(OccupiedTerritories, na.rm = TRUE))%>%
  mutate(col = "croz", season = "1112", notes = as.character(NA))%>% 
  rename(subcol = Subcolony, date = Date)%>%
  filter(!is.na(subcol))

# load 1112 chick count
# need to average observers cts
chct_1112 <-readWorksheet(wb_2, sheet = "Chicks 011612",startRow = 1, colTypes = c("POSIXct", "character", "character", "numeric", "numeric"))
names(chct_1112)
chct_1112_format <- chct_1112 %>%
  group_by(Subcolony, Date)%>%
  summarise(ch_ct=mean(Chicks, na.rm = TRUE))%>%
  mutate(col = "croz", season = "1112", notes = as.character(NA))%>% 
  rename(subcol = Subcolony, date = Date)%>%
  filter(!is.na(subcol))



# 1213 ####
# adult count was done over 2 days 11/28/12 and 11/29/12 according to the journal. 
# Need to change date in spreadsheet to match
adct_1213 <-readWorksheet(wb_2, sheet = "Adults 11282012",startRow = 1)
names(adct_1213)
# need to average counts
adct_1213_format <- adct_1213 %>%
  group_by(Subcolony)%>%
  summarise(
    total_ct=mean(TotalIndividuals, na.rm = TRUE),
    active_ct = mean(ActiveNests, na.rm = TRUE),
    occ_ct = mean(OccupiedTerritories, na.rm = TRUE))%>%
  mutate(date = as.POSIXct("2012-11-28"),col = "croz", season = "1213", notes = as.character(NA))%>% 
  rename(subcol = Subcolony)%>%
  filter(!is.na(subcol))
# load chick count from annual folder
# need to average observers cts
chct_1213 <-readWorksheet(wb_2, sheet = "Chicks01112013",startRow = 1, colTypes = c("POSIXct", "character", "character","numeric"))
names(chct_1213)
chct_1213_format <- chct_1213 %>%
  group_by(Subcolony,Date)%>%
  summarise(ch_ct=mean(count, na.rm = TRUE))%>%
  mutate(col = "croz", season = "1213", notes = as.character(NA))%>% 
  rename(subcol = Subcolony, date = Date)%>%
  filter(!is.na(subcol))


# 1314
adct_1314 <-readWorksheet(wb_2, sheet = "Adults 12042013",startRow = 1)
names(adct_1314)
# need to average counts
# some dates formatted incorretly in spreadsheet so need to replace date column
adct_1314_format <- adct_1314 %>%
  group_by(Subcolony)%>%
  summarise(
    total_ct=mean(TotalIndividuals, na.rm = TRUE),
    active_ct = mean(ActiveNests, na.rm = TRUE),
    occ_ct = mean(OccupiedTerritories, na.rm = TRUE))%>%
  mutate(date = as.POSIXct("2013-12-04"),col = "croz", season = "1314", notes = as.character(NA))%>% 
  rename(subcol = Subcolony)%>%
  filter(!is.na(subcol))
# load chick count from annual folder
# need to average observers cts
chct_1314 <-readWorksheet(wb_2, sheet = "Chicks01122014",startRow = 1, colTypes = c("POSIXct", "character", "numeric","numeric","numeric", "numeric", "numeric"))
names(chct_1314)
chct_1314_format <- chct_1314 %>%
  gather(key = initials, value = ch_ct, -c(Subcolony,Date,mean))%>%
  group_by(Subcolony,Date)%>%
  summarise(ch_ct=mean(ch_ct, na.rm = TRUE))%>%
  mutate(col = "croz", season = "1314", notes = as.character(NA))%>% 
  rename(subcol = Subcolony, date = Date)%>%
  filter(!is.na(subcol))


# 1415
adct_1415 <-readWorksheet(wb_2, sheet = "Adults 12032014",startRow = 1)
names(adct_1415)
# need to average counts
# some dates formatted incorretly in spreadsheet so need to replace date column
adct_1415_format <- adct_1415 %>%
  group_by(Subcolony, Date)%>%
  summarise(
    total_ct=mean(TotalIndividuals, na.rm = TRUE),
    active_ct = mean(ActiveNests, na.rm = TRUE),
    occ_ct = mean(OccupiedTerritories, na.rm = TRUE))%>%
  mutate(col = "croz", season = "1415", notes = as.character(NA))%>% 
  rename(subcol = Subcolony, date = Date)%>%
  filter(!is.na(subcol))
# load chick count from annual folder
# need to average observers cts
chct_1415 <-readWorksheet(wb_2, sheet = "Chicks01132015",startRow = 1, colTypes = c("POSIXct", "character", "character","numeric", "numeric"))
names(chct_1415)
chct_1415_format <- chct_1415 %>%
  group_by(Subcolony,Date)%>%
  summarise(ch_ct=mean(count, na.rm = TRUE))%>%
  mutate(col = "croz", season = "1415", notes = as.character(NA))%>% 
  rename(subcol = Subcolony, date = Date)%>%
  filter(!is.na(subcol))


#1516
# adult count and chick count in separate workbooks
wb_3 <- loadWorkbook("Z:/Informatics/S031/S0311516/croz1516/chick counts/adultcount_1516.xlsx")

# adult count
adct_1516 <- readWorksheet(wb_3, sheet = "raw count")
adct_1516_format <- adct_1516 %>%
  group_by(subcolony, date)%>%
  summarise(
    total_ct=mean(totalindividuals, na.rm = TRUE),
    active_ct = mean(activenests, na.rm = TRUE),
    occ_ct = mean(occupiedterritories, na.rm = TRUE))%>%
  mutate(col = "croz", season = "1516", notes = as.character(NA))%>% 
  rename(subcol = subcolony)%>%
  filter(!is.na(subcol))

wb_4 <- loadWorkbook("Z:/Informatics/S031/S0311516/croz1516/chick counts/chickcount_1516.xlsx")
chct_1516 <-readWorksheet(wb_4, sheet = "raw_data",startRow = 1)
names(chct_1516)
chct_1516_format <- chct_1516 %>%
  group_by(Subcolony,Date)%>%
  summarise(ch_ct=mean(count, na.rm = TRUE))%>%
  mutate(col = "croz", season = "1516", notes = as.character(NA))%>% 
  rename(subcol = Subcolony, date = Date)%>%
  filter(!is.na(subcol))



# Combine all formated tables into one each for adult counts and chick counts
adct_list <- list(adct_0203_format,adct_0304_format, adct_0405_format, adct_0506_format,adct_0607_format,adct_0708_format,
                  adct_0809_format,adct_0910_format,adct_1011_format,adct_1112_format,adct_1213_format,
                  adct_1314_format,adct_1415_format,adct_1516_format)
adct_all <- as.data.frame(adct_list[1])

for(i in 2:length(adct_list)){
  a = data.frame(adct_list[i])
  adct_all <- full_join(adct_all,a)
}

adct_all <- arrange(adct_all, subcol)
adct_all_clean <- adct_all %>%
  filter(!subcol %in% c("averages","Averages/totals:", "Total/avg"))%>%
  filter(!subcol=="39+40")%>% # these subcolonies counted separately most years (only loosing one count by removing the combined rows)
  # some confusion apparant over boundaries of 15-17. Removing counts from 0809 from "15 +17 terrace" and "15+17 beach." Not clear what these distinction mean but they are subcounts of 15+17 total
  filter(!subcol%in%c("15 +17 total", "15+17 beach"))%>%
  # 22-16a appears to have been renamed 22a. One season, 33-34-35 was entered only as 35.
  # changing all capital letters to lower case
  mutate(subcol = plyr::mapvalues(subcol, 
                                  from = c("14A","16+22", "22 & 16","22-16","22-16-A", "22-16A","6A","9 & 10", "35", "5A","1A", "15 & 17", "15+17 terrace"),
                                  to = c("14a",rep("16-22",3),"22a","22a","6a","9-10", "33-34-35", "5a", "1a", rep("15-17",2))))%>%
  # add 18 beach to 18 for 0809 season
  mutate(total_ct = replace(total_ct, subcol=="18"&season=="0809",total_ct[subcol=="18"&season=="0809"]+total_ct[subcol=="18 beach"&season=="0809"]))%>%
  mutate(active_ct = replace(active_ct, subcol=="18"&season=="0809",active_ct[subcol=="18"&season=="0809"]+active_ct[subcol=="18 beach"&season=="0809"]))%>%
  # remove rows for 18 beach
  filter(!subcol=="18 beach")%>%
  # remove row that have no totol and no active count
  filter(!(is.na(total_ct)&is.na(active_ct)))%>%
  # #sort by subcol
  arrange(subcol)%>%
  #order columns
  select(col,season, date, subcol, active_ct, occ_ct, total_ct, notes)


# combine tables for chick count
chct_list <- list(chct_9600_format, chct_0001_format,chct_0203_format,chct_0304_format, chct_0405_format, 
                  chct_0506_format,chct_0607_format,chct_0708_format,
                  chct_0809_format,chct_0910_format,chct_1011_format,chct_1112_format,chct_1213_format,
                  chct_1314_format,chct_1415_format,chct_1516_format)
chct_all <- as.data.frame(chct_list[1])

for(i in 1:(length(chct_list)-1)){
  a = data.frame(chct_list[i+1])
  chct_all <- full_join(chct_all,a)
}

chct_all <- arrange(chct_all, subcol)
chct_all_clean <- chct_all %>%
  filter(!subcol %in% c("averages","Averages/totals:", "Total/avg","Average change/96"))%>%
  filter(!subcol=="39+40")%>% # these subcolonies counted separately most years (only loosing one count by removing the combined rows)
  # some confusion apparant over boundaries of 15-17. Removing counts from 0809 from "15 +17 terrace" and "15+17 beach." Not clear what these distinction mean but they are subcounts of 15+17 total
  filter(!subcol%in%c("15+17 terrace", "15+17 beach"))%>%
  # 22-16a appears to have been renamed 22a. One season, 33-34-35 was entered only as 35.
  # changing all capital letters to lower case
  mutate(subcol = plyr::mapvalues(subcol, 
                                  from = c(" 6/7", "14A", "15 & 17","15 +17 total", "15/17","17+15", "16/22", "16+22", "22-16","22 & 16","22+16", "16/22a", "22-16-A", "22A", "1a","1A", "1AA", "1B", "1C", "25/26", "34/35", "35A", "35", "3A", "5A", "6A", "9 & 10", "9+10"),
                                  to = c("6-7","14a", rep("15-17", 4), rep("16-22",5), rep("22a",3), "1a","1a", "1aa", "1b", "1c", "25-26", "34-35", "35a","33-34-35", "3a", "5a", "6a", "9-10", "9-10")))%>%
  # add 18 beach to 18 for 0809 season
  mutate(ch_ct = replace(ch_ct, subcol=="18"&season=="0809",ch_ct[subcol=="18"&season=="0809"]+ch_ct[subcol=="18 beach"&season=="0809"]))%>%
  # remove rows for 18 beach, subcol = 21?
  filter(!subcol%in%c("18 beach","21?"))%>%
  # remove row that have no count
  filter(!is.na(ch_ct))%>%
  # sort by subcol
  arrange(subcol)%>%
  # order columns
  select(col,season, date, subcol, ch_ct, notes)


# write data to file
# write.csv(adct_all_clean, file = "data/m_ad_ct_all_clean.csv", row.names = FALSE)
# write.csv(chct_all_clean, file = "data/m_ch_ct_all_clean.csv", row.names = FALSE)
