# Clay Ford
# jcf2d@virginia.edu

# getting and munging data for ggplot2 workshop

# Albemarle county
# Office of Geographic Data Services
# https://www.albemarle.org/government/community-development/gis-mapping/gis-data

# Real Estate Information
# Under PARCELS - Primary Card Level Data - this file includes data
# such as year built, finished square footage, number of rooms, and condition. 
# https://gisweb.albemarle.org/gisdata/CAMA/GIS_CardLevelData_new_TXT.zip


link <- "https://gisweb.albemarle.org/gisdata/CAMA/GIS_CardLevelData_new_TXT.zip"
download.file(link, destfile = basename(link))
unzip(basename(link), list = TRUE) # list files, but don't extract
unzip(basename(link)) # extract file to working directory
card_level <- read.csv("GIS_CardLevelData_new.txt", stringsAsFactors = FALSE, 
                       na.strings = "NULL")
names(card_level)


# Real Estate Information - Parcel Level Data.  This file contains information
# about the parcel itself such as owner information, deed acreage value, and
# assessed value

# https://gisweb.albemarle.org/gisdata/CAMA/GIS_View_Redacted_ParcelInfo_TXT.zip

link2 <- "https://gisweb.albemarle.org/gisdata/CAMA/GIS_View_Redacted_ParcelInfo_TXT.zip"
download.file(link2, destfile = basename(link2))
unzip(basename(link2), list = TRUE) # list files, but don't extract
unzip(basename(link2)) # extract file to working directory
parcel_level <- read.csv("GIS_View_Redacted_ParcelInfo.txt", stringsAsFactors = FALSE,
                         na.strings = "NULL")
# subset for parcels with only 1 card
parcel_level <- subset(parcel_level, Cards == 1)


# merge primary card level data with parcel level data
datAll <- dplyr::left_join(card_level, parcel_level,by = c("TMP" = "ParcelID"))
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
datAll$Remodeled <- ifelse(datAll$YearRemodeled > 0, 1, 0)
datAll <- subset(datAll, TotalRooms > 0 & TotalValue > 0)
datAll <- subset(datAll, Bedroom < 20)
datAll <- subset(datAll, TotalRooms < 60)
datAll <- subset(datAll, TotalValue < 12000000)
datAll <- subset(datAll, FinSqFt > 0)
datAll <- subset(datAll, !is.na(Condition))

vars <- c("YearBuilt", "YearRemodeled", "Condition", "NumStories", 
          "FinSqFt", "Bedroom", "FullBath", 
          "HalfBath", "TotalRooms", "LotSize", "TotalValue", "City")
datAll <- datAll[,vars]

write.csv(x = datAll, file = "albemarle_real_estate.csv", row.names = FALSE)

homes <- read.csv("albemarle_real_estate.csv")

save(card_level, parcel_level, homes, file = "AC_homes.Rda")

