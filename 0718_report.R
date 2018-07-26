# set up data
setwd("C:/Users/schu/Desktop/ab_test/datasets")
library(data.table)
library(dplyr)
library(readxl)
library(lubridate)

sfcampaign <- read.csv("SalesforceCampaign.csv", stringsAsFactors = F)
sfcampaign$DateTimeofPresentationScheduled <- as.Date(ymd_hms(sfcampaign$DateTimeofPresentationScheduled))

# campaign level
sfcampaign_pre_dt <- sfcampaign %>%
  dplyr::filter(IsDeleted == "No" & MSA %in% c("Dallas Metro", "Tampa Metro", "Orlando Metro","Atlanta Metro","Charlotte Metro","Seattle Metro","Portland Metro","Denver Metro","Minneapolis-St.Paul Metro") & 
                  RecordType == "Presentation" & Status %in% c("Completed","Presented","Needs Follow-Up") & DateTimeofPresentationScheduled >= as.Date("2016-01-01") & DateTimeofPresentationScheduled <= as.Date("2018-07-17") & OPOfficeId != "") %>%
  dplyr::select(OPOfficeId, AccountName, MSA, OfficeZipCode, DateTimeofPresentationScheduled, Priority)
str(sfcampaign_pre_dt)  # 2027
length(unique(sfcampaign_pre_dt$OPOfficeId)) # 756

# office level
sfcampaign_priority <- sfcampaign_pre_dt %>% 
  dplyr::select("OPOfficeId","AccountName","MSA","OfficeZipCode","Priority") %>% 
  dplyr::distinct()  #768
nrow(sfcampaign_priority) 

## deal with duplicates
# check duplicates
duplicates <- sfcampaign_priority[which(duplicated(sfcampaign_priority$OPOfficeId)),"OPOfficeId"]
duplicated_id <- sfcampaign_priority[sfcampaign_priority$OPOfficeId %in% duplicates,]
duplicated_id %>% dplyr::arrange(OPOfficeId)

# delete duplicates
sfcampaign_priority <- sfcampaign_priority[!(sfcampaign_priority$AccountName %in% c("Lawrie Lawrence Real Estate",
                                                                                    "Century 21 Judge Fite - Colleyville",
                                                                                    "Coldwell Banker Bain - Bellevue",
                                                                                    "Berkshire Hathaway HomeServices Carolina",
                                                                                    "Century 21 Professional Realty formerly Century 21 Pinnacle Realty (Formerly C21 Seven Oaks Realty)",
                                                                                    "Next Home Neighborhood Realty",
                                                                                    "Coldwell Banker Seal - Lake Oswego",
                                                                                    "Coldwell Banker Seal - Vancouver West")),]
sfcampaign_priority$MSA[sfcampaign_priority$AccountName=="Coldwell Banker Residential - Atlanta"] <- "Atlanta Metro"

sfcampaign_priority <- sfcampaign_priority %>% dplyr::distinct()

# officel level - add shedule date in the future
sfcampaign_scheduledt <- sfcampaign %>%
  dplyr::filter(IsDeleted == "No" & MSA %in% c("Dallas Metro", "Tampa Metro", "Orlando Metro","Atlanta Metro","Charlotte Metro","Seattle Metro","Portland Metro","Denver Metro","Minneapolis-St.Paul Metro") & 
                  RecordType == "Presentation" & Status == "Confirmed" & DateTimeofPresentationScheduled >= as.Date("2018-07-18") & OPOfficeId != "") %>%
  dplyr::select(OPOfficeId,DateTimeofPresentationScheduled) %>%
  dplyr::group_by(OPOfficeId) %>%
  dplyr::summarise(schedule_visit1 = min(DateTimeofPresentationScheduled), 
                   schedule_visit2 = min(DateTimeofPresentationScheduled[DateTimeofPresentationScheduled!=min(DateTimeofPresentationScheduled)], na.rm = T),
                   schedule_visit3 = max(DateTimeofPresentationScheduled, na.rm = T))
sfcampaign_scheduledt$schedule_visit3[sfcampaign_scheduledt$schedule_visit3 == sfcampaign_scheduledt$schedule_visit1 |
                                                  sfcampaign_scheduledt$schedule_visit3 == sfcampaign_scheduledt$schedule_visit2] <- NA
str(sfcampaign_scheduledt)
length(unique(sfcampaign_scheduledt$OPOfficeId)) #123 offices

# read in lean in zipcode for analysis
zip <- read_excel("zipcode.xlsx")
zip_vector <- zip$Zip

# data table for analysis
sfcampaign_byoffice <- sfcampaign_pre_dt %>%
  dplyr::group_by(OPOfficeId) %>%
  dplyr::summarise(n_visit = n(), frst_dt = min(DateTimeofPresentationScheduled), last_dt = max(DateTimeofPresentationScheduled)) %>%
  dplyr::mutate(tenure = difftime(as.Date("2018-07-18"), frst_dt, units = "days"), recency = difftime(as.Date("2018-07-18"), last_dt, units = "days")) %>%
  dplyr::mutate(inter_visit_time1 = (last_dt - frst_dt)/(n_visit-1), inter_visit_time2 = tenure/n_visit) %>%
  dplyr::left_join(sfcampaign_priority, by = "OPOfficeId") %>%
  dplyr::left_join(sfcampaign_scheduledt, by = "OPOfficeId")

sfcampaign_byoffice$OfficeZipCode <- as.numeric(sfcampaign_byoffice$OfficeZipCode)
sfcampaign_byoffice$leanin_zip_flag <- 0
sfcampaign_byoffice$leanin_zip_flag[sfcampaign_byoffice$OfficeZipCode %in% zip_vector] <- 1
summary(sfcampaign_byoffice$leanin_zip_flag)
str(sfcampaign_byoffice)

mean(sfcampaign_byoffice$inter_visit_time2[sfcampaign_byoffice$leanin_zip_flag == 1])  #208.7 days
median(sfcampaign_byoffice$inter_visit_time2[sfcampaign_byoffice$leanin_zip_flag == 1]) 

mean(sfcampaign_byoffice$inter_visit_time2[sfcampaign_byoffice$leanin_zip_flag == 0]) #261.1 days
median(sfcampaign_byoffice$inter_visit_time2[sfcampaign_byoffice$leanin_zip_flag == 0])

# summary(sfcampaign_byoffice)
# head(sfcampaign_byoffice)
# length(unique(sfcampaign_byoffice$OPOfficeId))  #756
# length(unique(sfcampaign_byoffice$AccountName)) #741
# tail(data.frame(sfcampaign_byoffice), n =100)

write.csv(sfcampaign_byoffice, "bdm_visit_report_0719.csv", row.names = F)
