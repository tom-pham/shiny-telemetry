##%######################################################%##
#                                                          #
####           Detections downloads for Shiny           ####
#                                                          #
##%######################################################%##

library(tidyverse)
library(rerddap)
library(lubridate)

# Clear cached data in order to retrieve all latest data
cache_delete_all()

# Establish ERDDAP url and database name
my_url <- "https://oceanview.pfeg.noaa.gov/erddap/"
JSATSinfo <- info('FED_JSATS_taggedfish', url = my_url)
TaggedFish <- tabledap(JSATSinfo, url = my_url) %>% 
  as_tibble()

my_url <- "https://oceanview.pfeg.noaa.gov/erddap/"
JSATSinfo <- info('FED_JSATS_receivers', url = my_url)
ReceiverDeployments <- tabledap(JSATSinfo, url = my_url)

my_url <- "https://oceanview.pfeg.noaa.gov/erddap/"
JSATSinfo <- info('FED_JSATS_detects', url = my_url)

# Function to download the studyID in the proper format for Shiny
download_detections <- function(studyID) {
  
  # Retrieve detection data from ERDDAP
  #
  # Arguments:
  #  studyID: StudyID name to retrieve data for
  #     
  # Return:
  #  df of detection data formatted correctly, add in RKM, Region, Lat, Lon, 
  # Release RKM, Release Lat, Release Lon, format types, rename cols
  
  
  df <- tabledap(JSATSinfo,
                 fields = c('study_id', 'receiver_serial_number', 'fish_id', 
                            'receiver_general_location','local_time', 'latitude', 
                            'longitude'),
                 paste0('study_id=', '"',studyID, '"'),
                 url = my_url,
                 distinct = T
  ) %>% 
    left_join(
      ReceiverDeployments %>% 
        select(receiver_general_location, receiver_general_river_km, receiver_region,
               receiver_general_latitude, receiver_general_longitude) %>% 
        distinct()
    ) %>% 
    left_join(
      TaggedFish %>% 
        select(fish_id, release_river_km, release_latitude, release_longitude) %>% 
        distinct()
    ) %>% 
    distinct()
  
  # Rename columns and change column types as ERDDAP returns data all in 
  # character format
  df <- df %>% 
    rename(
      studyID = study_id,
      SN = receiver_serial_number,
      FishID = fish_id,
      GEN = receiver_general_location,
      LAT = latitude,
      LON = longitude,
      GenRKM = receiver_general_river_km,
      Region = receiver_region,
      GenLat = receiver_general_latitude,
      GenLon =receiver_general_longitude,
      RelRKM = release_river_km
    ) %>% 
    mutate(
      GenLat = ifelse(is.na(GenLat), release_latitude, GenLat),
      GenLon = ifelse(is.na(GenLon), release_longitude, GenLon),
      GenLat = as.numeric(GenLat),
      GenLon = as.numeric(GenLon),
      GenRKM = as.numeric(GenRKM),
      RelRKM = as.numeric(RelRKM),
      time_pst = ymd_hms(local_time),
      GenRKM = ifelse(is.na(GenRKM), RelRKM, GenRKM)
    ) %>% 
    select(-c(RelRKM, release_latitude, release_longitude)) %>% 
    distinct()
  
  write_csv(df, paste0('./data/detections/', studyID, ".csv"))
  
}

# Provide the studyID name you want to download here
studyID <- ""

# Call the download function and provide the studyID you want and it will
# retrieve the data from ERDDAP and save it to csv in the detections folder
download_detections(studyID)


