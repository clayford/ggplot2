# Clay Ford
# jcf2d@virginia.edu

# getting and munging data for ggplot2 workshop

# Albmarle county
# Office of Geographic Data Services
# http://www.albemarle.org/department.asp?department=gds&relpage=3914#Parcels

# Real Estate Information - Primary Card Level Data - this file includes data
# such as year built, finished square footage, number of rooms, and condition. 
# http://www.albemarle.org/gds/gisdata/CAMA/CAMA_CardLevelData_TXT.zip

library(dplyr)

link <- "http://www.albemarle.org/gds/gisdata/CAMA/CAMA_CardLevelData_TXT.zip"
download.file(link, destfile = basename(link))
unzip(basename(link), list = TRUE) # list files, but don't extract
unzip(basename(link), "CAMA_CardLevelData.txt") # extract file to working directory
card_level <- read.csv("CAMA_CardLevelData.txt", stringsAsFactors = FALSE)
names(card_level)


# Real Estate Information - Parcel Level Data.  This file contains information
# about the parcel itself such as owner information, deed acreage value, and
# assessed value

# http://www.albemarle.org/gds/gisdata/CAMA/CAMA_ParcelInfo_TXT.zip

link2 <- "http://www.albemarle.org/gds/gisdata/CAMA/CAMA_ParcelInfo_TXT.zip"
download.file(link2, destfile = basename(link2))
unzip(basename(link2), list = TRUE) # list files, but don't extract
unzip(basename(link2), "CAMA_ParcelInfo.txt") # extract file to working directory
parcel_level <- read.csv("CAMA_ParcelInfo.txt", stringsAsFactors = FALSE)
# subset for parcels with only 1 card
parcel_level <- subset(parcel_level, Cards == 1)


# merge primary card level data with parcel level data
datAll <- left_join(card_level, parcel_level,by = c("TMP" = "ParcelID"))
# datAll <- merge(card_level, parcel_level,by.x = "TMP",by.y = "ParcelID")
rm(link, link2)

# select only certain cities
datAll <- subset(datAll, City %in% c("CHARLOTTESVILLE", "EARLYSVILLE", "CROZET", 
                             "NORTH GARDEN","SCOTTSVILLE", "KESWICK"))



# drop any rows with missing data
# datAll <- na.omit(datAll)

# miscellaneous clean up and variable derivation
# CardType = R
# Cards = 1
datAll <- subset(datAll, datAll$CardType == "R")
datAll$CardType <- NULL
datAll$TMP <- NULL
datAll <- subset(datAll, YearBuilt > 0)
datAll$Remodeled <- ifelse(datAll$YearRemodeled > 0, 1, 0)
datAll <- subset(datAll, TotalRooms > 0 & TotalValue > 0)
datAll <- subset(datAll, Bedroom < 20)
datAll <- subset(datAll, TotalRooms < 60)
datAll <- subset(datAll, TotalValue < 12000000)
datAll <- subset(datAll, FinSqFt > 0)
datAll$Condition <- trimws(datAll$Condition)
datAll <- subset(datAll, Condition != "")

vars <- c("YearBuilt", "YearRemodeled", "Condition", "NumStories", "FinSqFt", "Bedroom", "FullBath", 
          "HalfBath", "TotalRooms", "LotSize", "TotalValue", "City")
datAll <- datAll[,vars]

write.csv(x = datAll, file = "albemarle_real_estate.csv", row.names = FALSE)

homes <- read.csv("albemarle_real_estate.csv")

save(card_level, parcel_level, homes, file = "AC_homes.Rda")

