##############################################################################
# Author: Tom Pham
# Purpose: This script separates out the global data required in the Shiny 
# telemetry app and is sourced in the app.R

# Load data
# --- ReceiverDeployments
# --- VemcoReceiverDeployments
# --- StudyIDs
# --- TaggedFish
# --- CDEC Flows

library(shinydashboard) # shiny tabs
library(shinythemes)
library(leaflet) # web mapping
library(leaflet.extras)
library(leaflet.minicharts) # web mapping animation
library(dygraphs) # interactive time series graphing
library(plotly) # interactive plotting
library(CDECRetrieve) # CDEC retrievals
library(xts) # formatting for dygraphs
library(lubridate) # date time madness
library(tidyverse)
library(suncalc) # getting sunset sunrise times
library(arsenal) # nice summary stats tables
library(gt) # fancy tables
library(rerddap) # ERDDAP data retrievals
library(httr) # Check HTTP status for CDEC/ERDDAP
library(vroom) # Fastest way to read csv
library(sf) # To display gis layers
library(DT)


# Load Receiver Deployments -----------------------------------------------

# Clear cached data in order to retrieve all latest data
cache_delete_all()

# If last dl was > 90 days update files, else read in csv

# Download updates every 90 days
last_checked_date <- read_rds("./data/last_checked_date.RDS")

# If last update check was < 90 days read in CSVs (much faster load times)
if (as.numeric(Sys.Date() - last_checked_date) < 90) {
  ReceiverDeployments <- vroom("./data/rec_deployments.csv")
} else { 
  # Else check if ERDDAP is online, x returns TRUE if database is down or "Timeout"
  # if the http check timeouts out 
  x <- tryCatch(http_error("oceanview.pfeg.noaa.gov/erddap/tabledap/FED_JSATS_receivers.html", 
                           timeout(3)), error=function(e) print("Timeout"))
  
  # If the database isn't working then read csv
  if (x == TRUE | x == "Timeout") {
    ReceiverDeployments <- vroom("./data/rec_deployments.csv")
  } else {
    # If database is working then check for updates
    
    ## Download ReceiverDeployments
    my_url <- "https://oceanview.pfeg.noaa.gov/erddap/"
    JSATSinfo <- info('FED_JSATS_receivers', url = my_url)
    ReceiverDeployments <- tabledap(JSATSinfo, url = my_url)  
    
    # Fix column names and correct column types
    ReceiverDeployments <- ReceiverDeployments %>% 
      rename(
        SN = receiver_serial_number,
        GEN = receiver_general_location,
        Region = receiver_region,
        GPSname = receiver_location,
        LAT = latitude,
        LON = longitude,
        RKM = receiver_river_km,
        GenLat = receiver_general_latitude,
        GenLon = receiver_general_longitude,
        GenRKM = receiver_general_river_km,
        RecMake = receiver_make,
        StartTime = receiver_start,
        EndTime = receiver_end
      ) %>% 
      mutate_at(vars(SN, LAT, LON, RKM, GenLat, GenLon, GenRKM), as.numeric) %>% 
      mutate(
        StartTime = mdy_hm(StartTime),
        EndTime = mdy_hm(EndTime),
        water_year = ifelse(month(StartTime) <= 9, year(StartTime),
                            year(StartTime) + 1)
      ) %>% 
      filter(
        SN != 1
      )
    
    # Save latest update to file
    write_csv(ReceiverDeployments, "./data/rec_deployments.csv")
    
    # Change the last saved date to today
    last_checked_date <- Sys.Date()
    saveRDS(last_checked_date, "./data/last_checked_date.RDS")
  }
}

# Load Vemco Receiver Deployments -----------------------------------------

# This must be manually updated
VemcoReceiverDeployments <- vroom("./data/vemco_rec_deployments.csv") %>%
  left_join(ReceiverDeployments %>% 
              select(GPSname, GEN, GenLat, GenLon) %>% 
              distinct()) %>%
  mutate(
    water_year = ifelse(month(StartTime) <= 9, year(StartTime),
                        year(StartTime) + 1)
  )

# Assigns ReceiverDeployments an ID to be able to be identifiable when clicked
ReceiverDeployments$uid <- 1:nrow(ReceiverDeployments)
VemcoReceiverDeployments$uid <- 1:nrow(VemcoReceiverDeployments)


# Load StudyIDs -----------------------------------------------------------

# Table of studyid, descriptive name, whether it should be included in 
# survival tab, whether it should be included in non-survival tabs
studyid_dictionary <- vroom("./data/studyid_names.csv")

# Descriptive names for studyids to be used in survival
surv_studyid_descript_name <- studyid_dictionary %>% 
  filter(incl_survival == "YES") %>% 
  pull(descript_name)

# Descriptive names for studyids to be used in outmigration animation, 
# data explorer, time of day, movement
other_studyid_descript_name <- studyid_dictionary %>% 
  filter(incl_other == "YES") %>% 
  pull(descript_name)


# Load TaggedFish ---------------------------------------------------------

TaggedFish <- vroom("./data/tagged_fish.csv")

# Update TaggedFish table from ERDDAP every 30 days
if (as.numeric(Sys.Date() - last_checked_date) < 30) { 
  my_url <- "https://oceanview.pfeg.noaa.gov/erddap/"
  JSATSinfo <- info('FED_JSATS_taggedfish', url = my_url)
  
  TaggedFish <- tabledap(JSATSinfo, url = my_url)  
  # Fix column names and correct column types
  TaggedFish <- TaggedFish %>% 
    mutate_at(vars(tag_weight, tag_pulse_rate_interval_nominal, tag_warranty_life,
                   fish_length, fish_weight, release_latitude, release_longitude,
                   release_river_km), as.numeric) 
  
  closeAllConnections()
  # Save latest update to file
  write_csv(TaggedFish, "./data/tagged_fish.csv")
}

TaggedFish <- vroom("./data/tagged_fish.csv")


# Load CDEC hydrology data ------------------------------------------------

# Read saved CDEC flow data
comb_flow <- vroom("./data/comb_flow.csv", col_types = c(Index = "D", KES = "d",
                                                         BND = "d", BTC = "d",
                                                         WLK = "d"))

# Update file with new flow data  from CDEC if it has been over 30 days since 
# last download
if (as.numeric(Sys.Date() - max(comb_flow$Index)) > 30) {
  # Choose CDEC gauges to display
  gauges <- c("KES", "BND", "BTC", "WLK")
  
  # NOT NECESSARY to get data from all the way in the beginning, just add 
  # data from last date to today's date
  start_date <- max((lapply(lapply(gauges, cdec_datasets),
                            function(x) filter(x, sensor_number %in%
                                                 c("20", "23"))) %>%
                       bind_rows())$start)
  
  # The last date of downloaded data in comb_flow + 1
  last_date <- max(comb_flow$Index) + 1
  
  # apply the list of gauges to function that queries CDEC to get daily flow 
  # then turns it into an xts (time series object) object which is needed for dygraphs
  flows <- lapply(gauges,
                 function(x) {
                   if (x == "KES") { # If Keswick, use reservoir outflow (23) instead 
                     y <- cdec_query(x, 23, "D", last_date, Sys.Date())
                   }else {
                     y <- cdec_query(x, 41, "D", last_date, Sys.Date())
                   }
                   y <- y %>% 
                     select(location_id, datetime, parameter_value) %>% 
                     drop_na() %>% 
                     filter(parameter_value > 0)
                   y <- as.xts(y$parameter_value,y$datetime, order.by = y$datetime)
                 }
  )
  
  comb_flow <- do.call(cbind, flows)
  names(comb_flow) <- gauges
  
  # Convert XTS to dataframe and add the Index as a column which is date
  # Step necessary because write.zoo doesn't allow overwrites
  comb_flow2 <- as.data.frame(comb_flow) %>% 
    rownames_to_column("Index")
  
  rm(comb_flow)
  write_csv(comb_flow2, "./data/comb_flow.csv", append = T)
  rm(comb_flow2)
}

comb_flow <- as.xts(read.csv.zoo("./data/comb_flow.csv"))

cdec_stations <- vroom("./data/cdec_stations.csv")


# Helper functions --------------------------------------------------------

# Convert a studyid descriptive name to short studyid name
convert_descriptive_to_studyid <- function(descript_studyid) {
  studyid_dictionary$study_id[studyid_dictionary$descript_name == descript_studyid]
}


