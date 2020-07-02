library(shiny)
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
library(slickR) # JS image carousel
library(waiter) # Loading animations
library(httr) # Check HTTP status for CDEC/ERDDAP
library(vroom)
library(sf)

# Global ------------------------------------------------------------------

## Download data from ERDAPP - ReceiverDeployments
my_url <- "https://oceanview.pfeg.noaa.gov/erddap/"
JSATSinfo <- info('FED_JSATS_receivers', url = my_url)

# Save latest table to file, if ERDDAP is down use last saved file 
if (http_error("oceanview.pfeg.noaa.gov/erddap/tabledap/FED_JSATS_receivers.html") == FALSE) {
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
  write_csv(ReceiverDeployments, "./data/ReceiverDeployments.csv")
} else {
  ReceiverDeployments <- vroom("./data/ReceiverDeployments.csv")
}

VemcoReceiverDeployments <- read_csv("./data/VemcoReceiverDeployments.csv") %>%
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


## Download data from ERDAPP - TaggedFish
my_url <- "https://oceanview.pfeg.noaa.gov/erddap/"
JSATSinfo <- info('FED_JSATS_taggedfish', url = my_url)

# Save latest table to file, if ERDDAP is down use last saved file 
if (http_error("oceanview.pfeg.noaa.gov/erddap/tabledap/FED_JSATS_taggedfish.html") == FALSE) {
  TaggedFish <- tabledap(JSATSinfo, url = my_url)  
  
  # Fix column names and correct column types
  TaggedFish <- TaggedFish %>% 
    mutate_at(vars(tag_weight, tag_pulse_rate_interval_nominal, tag_warranty_life,
                   fish_length, fish_weight, release_latitude, release_longitude,
                   release_river_km), as.numeric) 

  # Save latest update to file
  write_csv(TaggedFish, "./data/TaggedFish.csv")
} else {
  TaggedFish <- read_csv("./data/TaggedFish.csv")
}

## Download data from ERDAPP - Detections
my_url <- "https://oceanview.pfeg.noaa.gov/erddap/"
JSATSinfo <- info('FED_JSATS_detects', url = my_url)

# Retrieve list of valid studyIDs in ERDDAP but first check if site is down
#If ERDDAP is not down retrieve list of studyIDs and save to file
if (http_error("https://oceanview.pfeg.noaa.gov/erddap/tabledap/FED_JSATS_detects.html") == FALSE) {
  studyid_list <- tabledap(JSATSinfo,
                           fields = c('study_id'),
                           url = my_url,
                           distinct = TRUE
  ) %>%
    filter(study_id != "2017_BeaconTag") %>%
    pull(study_id)

  # If any of the currently observed studyIDs is not in the saved file, rewrite it with the new
  # StudyIDs
  saved_studyid_list <- readRDS("studyid_list.RDS")
  if (any(studyid_list != saved_studyid_list)) {
    saveRDS(studyid_list, file = "studyid_list.RDS")
    #########
    ####  INCLUDE SOMETHING HERE THAT WILL ALERT THAT A NEW STUDYID IS ONLINE
    #########
  }
}else {
  studyid_list <- readRDS("studyid_list.RDS")
}

#### List of studyIDs relevant to the project and selectable in the app
#### Update this list as needed to allow for additional studyIDs to be selected
studyid_list <- c("BC_Jumpstart_2019", "CNFH_FMR_2019", "ColemanFall_2012", "ColemanFall_2013",
                  "ColemanFall_2016", "ColemanFall_2017", "ColemanLateFall_2018", "ColemanLateFall_2019",
                  "ColemanLateFall_2020", "DeerCk_SH_Wild_2018", "DeerCk_Wild 2019", "DeerCk_Wild_2017",
                  "DeerCk_Wild_2018", "FR_Spring_2013", "FR_Spring_2014", "FR_Spring_2015", "FR_Spring_2019",
                  "MillCk_SH_Wild_2015", "MillCk_SH_Wild_2016", "MillCk_Wild 2019", "MillCk_Wild_2013",
                  "MillCk_Wild_2014", "MillCk_Wild_2015", "MillCk_Wild_2016", "MillCk_Wild_2016_DS",
                  "MillCk_Wild_2017", "MillCk_Wild_2018", "Nimbus_Fall_2016", "Nimbus_Fall_2017",
                  "Nimbus_Fall_2018", "RBDD_2017", "RBDD_2018", "SB_Spring_2015", "SB_Spring_2016",
                  "SB_Spring_2017", "SB_Spring_2018", "SB_Spring_2019", "Winter_H_2013", "Winter_H_2014",
                  "Winter_H_2015", "Winter_H_2016", "Winter_H_2017", "Winter_H_2018", "Winter_H_2019")

# Check to see that there is a detection file for each studyID in the studyid_list, if not
# download it into the data folder
check_detects_downloaded <- function(studyID) {
  # Detection files are named by studyID now get vector of file studyID names without extension
  files <- unlist(strsplit(list.files("./data/detections"), ".csv"))
  
  # Look at the files in the detections folder, see if there is a match between the StudyID name
  # and the list of file names, if there is not download that studyID and put into the folder
  if (!(studyID %in% files)) {
    JSATSinfo <- info('FED_JSATS_detects', url = my_url)
    
    detects_fields <- c('study_id', 'fish_id', 'time', 'receiver_general_location', 
                        'receiver_serial_number', 'latitude', 'longitude')
    
    detections <- tabledap(JSATSinfo,
                           fields = detects_fields,
                           paste0('study_id=', '"',studyID, '"'),
                           url = my_url,
                           distinct = TRUE) %>% 
      mutate(time = ymd_hms(time)) %>% 
      rename(
        studyID = study_id,
        FishID = fish_id,
        GEN = receiver_general_location,
        SN = receiver_serial_number,
        LAT = latitude,
        LON = longitude
      ) %>% 
      left_join(ReceiverDeployments %>% 
                  select(GEN, Region, GenLat, GenLon, GenRKM) %>% 
                  distinct(), by = "GEN")
    
    file_path <- paste0("./data/detections/", studyID, ".csv")
    write_csv(detections, file_path)
  }
}

lapply(studyid_list, check_detects_downloaded)

## Hydrology

# Gather flow data from CDEC, save to file to reduce calls to CDEC which is intermittently down
comb_flow <- read_csv("./data/comb_flow.csv")

# Update file with new flow data if it has been over 30 days since last download
if (as.numeric(Sys.Date() - max(comb_flow$Index)) > 30) {
  
  # Choose CDEC gauges to display
  gauges <- c("KES", "BND", "BTC", "WLK")
  
  # Apply cdec_datasets() on list of gauges, to get station metadata
  # Then apply function to filter to only look at information from sensor 20 and 23 (river flow sensors)
  # Then bind the rows together, then find what the max start date was, ultimately to get the start_date I want to use
  # That all the gauges are included in
  start_date <- max((lapply(lapply(gauges, cdec_datasets), function(x) filter(x, sensor_number %in% c("20", "23"))) %>% bind_rows())$start)
  
  
  # apply the list of gauges to function that queries CDEC to get daily flow then turns it into 
  # an xts (time series object) object which is needed for dygraphs
  flows = lapply(gauges,
                 function(x) {
                   if (x == "KES") { # If Keswick, use reservoir outflow (23) instead 
                     y <- cdec_query(x, 23, "D", start_date, Sys.Date())
                   }else {
                     y <- cdec_query(x, 41, "D", start_date, Sys.Date())
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
  
  write_csv(comb_flow2, "./data/comb_flow.csv")
  
}else {
  comb_flow <- as.xts(read.csv.zoo("./data/comb_flow.csv"))
}

cdec_stations <- read_csv("./data/cdec_stations.csv")

# UI ----------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("flatly"),
  use_waiter(),
  use_waitress(),
  navbarPage("Central Valley Enhanced Acoustic Tagging Project",
             tabPanel("Background", 
                      mainPanel(
                        uiOutput("background"),
                        slickROutput("slick_output", width = '500px', height = '500px')
                      )
             ),
             tabPanel("Receiver Deployments",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("receiverType", "Receiver type",
                                       c("Autonomous", "Real-time", "Vemco")
                          ),
                          selectInput("water_year",
                                      "Water Year:",
                                      ""),
                          helpText("Note: water year is defined as the 12 month period starting 
                                   October 1 to September 30 of the following calendar year. 
                                   The water year is designated by the calendar year in which 
                                   it ends and which includes 9 of the 12 months. Thus, the year
                                   ending September 30, 1999 is called the 1999 water year."),
                          htmlOutput("map_marker_click")
                        ),
                        mainPanel(
                          leafletOutput("map", width = "100%", height = "650"),
                          tableOutput("receiver_table")
                        )
                      )
             ),
             tabPanel("Hydrology",
                      box(dygraphOutput("dygraph", width = "100%")),
                      box(leafletOutput("hydromap", width = "75%")),
                      textOutput("text1"),
                      textOutput("text2"),
                      textOutput("text3"),
                      downloadButton("downloadData", "Download")

             ),
             tabPanel("Outmigration Animation",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("anim_dataset", "Choose a study population",
                                      #choices = strsplit(list.files("./data/Timestep"), ".csv")),
                                      choices = studyid_list),
                          helpText("This tool visualizes study group detection data into an animated time series",
                                   "of fish outmigration. Unique fish detections are identified at each general",
                                   "receiver location and summed by day. Note that is it possible for a fish to",
                                   "be detected at more than one general receiver location in a single day.")
                        ),
                        mainPanel(
                          leafletOutput("timemap", width = "100%", height = "650")
                        )
                      )
             ),
             tabPanel("Data Explorer",
                      headerPanel("Select Options"),
                      sidebarPanel(
                        selectInput("data_explorer_datasets", "Study Populations",
                                    choices = studyid_list,
                                    multiple = TRUE,
                                    selectize = TRUE),
                        selectInput("variable", "Variable",
                                    choices = c("Weight", "Length")),
                        selectInput("plot_type", "Plot type", 
                                    choices = c("boxplot", "histogram", "density")),
                        # Show this bin width slider only if histogram is chosen
                        conditionalPanel(
                          condition = "input.plot_type == 'histogram'",
                          # Place holder for bin slider input, created in server side
                          uiOutput("bin_slider")
                        )
                      ),
                      mainPanel(
                        plotlyOutput("plotly_plot"),
                        tableOutput("summarytable")
                      )
              ),
             tabPanel("Time of Day",
                      headerPanel("Select Options"),
                      mainPanel(
                        selectInput("time_of_day_input", "Choose a study population",
                                    choices = studyid_list
                                    ),
                        radioButtons("time_of_day_radio", "Choose all detections or by receiver GEN",
                                     c("All Detections", "By GEN")
                        ),
                        conditionalPanel(
                          condition = "input.time_of_day_radio == 'By GEN'",
                          selectInput("time_of_day_GEN", "Select a GEN",
                                      "")
                        ),
                        plotlyOutput("time_of_day_plot"),
                        uiOutput("time_of_day_caption")
                      )
             ),
             tabPanel("Survival",
                      tabsetPanel(
                        tabPanel("Unique Detections",
                                 headerPanel("Select Options"),
                                 sidebarPanel(
                                   selectInput("survival_datasets", "Study Populations",
                                               choices = studyid_list,
                                               selectize = T,
                                               multiple = T)
                                 ),
                                 mainPanel(
                                   gt_output("gt_output")
                                 )
                        )
                        # ,
                        # tabPanel("Estimated Survival",
                        #          headerPanel("Select Options"),
                        #          sidebarPanel(
                        #            selectInput("cumsurvival_datasets", "Study Group",
                        #                        ### These need to be manually updated
                        #                        choices = c("Coleman Fall", "Deer Creek", "Mill Creek",
                        #                                    "RBDD", "Sutter Bypass", "Winter"))
                        #          ),
                        #          mainPanel(
                        #            plotlyOutput("plotly_survival_output")
                        #          )
                        # )
                      )
             ),
             tabPanel("Movement",
                      headerPanel("Select Options"),
                      sidebarPanel(
                        selectInput(
                          "movement_dataset", "Choose a study population",
                          choices = studyid_list
                        )
                      ),
                      mainPanel(
                        gt_output(
                          "movement_gt"
                        )
                      )
             )
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  waiter <- Waiter$new(html = spin_wandering_cubes(), id = c("plotly_plot", "time_of_day_plot",
                                                             "gt_output"))
  
  waitress <- Waitress$new("#background", infinite = T, hide_on_render = T)

  # Background --------------------------------------------------------------
  output$background <- renderUI({
    waitress$start(h3("Loading..."))
    withTags({
      div(class="header", checked=NA,
          h1("Background"),
          p("Since 2012, we have been tagging juvenile salmon using JSATS (Juvenile 
            Salmon Acoustic Telemetry System) technology to track their movement and 
            survival over 500 river kilometers to the Pacific Ocean. Acoustic tags are 
            implanted into the peritoneal cavity of the study fish and closed with sutures. 
            After being released, tagged fish are presumed to exhibit a strictly downstream 
            movement, while being detected by underwater hydrophones located throughout the 
            riverine and estuary environments. Survival estimates are calculated by assuming 
            a fish has died when it is not detected at downstream receivers."),
          br(),
          h3("Receiver Deployments"),
          p("A map of all the receivers that were deployed by water year which
            is defined as the 12 month period beginning October 1 to September 30
            of the following calendar year i.e. water year 2017 spans 10/1/17-9/30/18.
            It is filtered by receiver type: autonomous (in-house customized 
            ATS Trident receivers), Real-time (ATS SR3017, Tekno), and Vemco (VR2W, VR2AR)."),
          br(),
          h3("Hydrology"),
          p("The hydrology tab shows an interactive graph of Sacramento River flows (cubic feet per second) 
            over time at four different ", a(href="https://cdec.water.ca.gov/index.html", "CDEC"), 
            " (California Data Exchange Center) stations: ", a(href="https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=KES", "KES"), 
            "(Keswick Reservoir), ", a(href="https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=BND", "BND"), " Bend Bridge, ",
            a(href="https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=BTC", "BTC"), "(Butte City), ", 
            a(href="https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=WLK", "WLK"), " (Wilkins Slough).
            This plot was created using the R package ", a(href="https://rstudio.github.io/dygraphs/", "dygraphs"),"."),
          br(),
          h3("Outmigration Animation"),
          p("The Outmigration Animation tab visualizes fish outmigration using detection data over time.
            What is represented is unique fish detections at each receiver location by day. This animation
            was created using the R packages ", a(href="https://rstudio.github.io/leaflet/", "leaflet"),
            "and leaflet.extras."),
          br(),
          h3("Data Explorer"),
          p("Data explorer allows users to look at the tagged fish data individually or across different
            study populations in a number of ways. Plots can be created interactively, choosing variables
            to explore (length, weight), and with different plot types (boxplot, histogram, density) as 
            well as summary tables."),
          br(),
          h3("Time of Day"),
          p("Time of day allows users to visually explorer behavioral differences in fish movement between
            night and day travel. Using detection times we can look at the distribution across time for
            an entire study group or across time by a specific receiver location."),
          br(),
          h3("Survival"),
          p("Ultimately acoustic telemetry helps us understand survival of tagged fish. Here we present
            raw surival numbers and survival estimates created by CJS survival models in RMark.")
      )
    })
  })
  
  output$slick_output <- renderSlickR({
    imgs <- list.files("./photos/", full.names = TRUE)
    slickR(imgs, width = '500px', height = '500px') +
      settings(autoplay = TRUE, autoplaySpeed = 3000)
  })

  
  # Receiver Deployments --------------------------------------------------
  
  # Updates allowable water_year selection input based on radio button selection
  observe({
    updateSelectInput(session, "water_year",
                      choices = outVar()
    )
  })
  
  # Reactive list of years depending on radio button selection
  outVar <-  reactive({
    mydata <- as.character(input$receiverType)
    
    # If the radio button is "Real-time" get the years they were deployed
    if (mydata == "Real-time") {
      vars <- ReceiverDeployments %>%
        filter(RecMake %in% c("ATS SR3017", "Tekno RT") & 
                 SN != "999" & GPSname != "999") %>% 
        select(water_year) %>% 
        distinct() %>% 
        drop_na() %>% 
        pull(water_year)
      # If the radio button is "Autonomous" get the years they were deployed
    }else if(mydata == "Autonomous") {
      vars <- ReceiverDeployments %>%
        filter(!(RecMake %in% c("ATS SR3017", "Tekno RT")) &
                 SN != "999" & GPSname != "999") %>% 
        select(water_year) %>% 
        distinct() %>% 
        drop_na() %>% 
        pull(water_year)
    }else if(mydata == "Vemco") {
      vars <- VemcoReceiverDeployments %>%
        select(water_year) %>% 
        distinct() %>% 
        drop_na() %>% 
        pull(water_year)
    }
    sort(vars)
  })
  
  # Create receiver deployments map
  output$map <- renderLeaflet({
    
    # Use user input to query receiver type and assign colors
    if (input$receiverType == "Autonomous") {
      receivers <- subset(ReceiverDeployments, water_year == input$water_year & !(RecMake %in% c("ATS SR3017", "Tekno RT")))
      color <- "purple"
    }else if (input$receiverType == "Real-time"){
      receivers <- subset(ReceiverDeployments, water_year == input$water_year & RecMake %in% c("ATS SR3017", "Tekno RT"))
      color <- "red"
    }else if(input$receiverType == "Vemco"){ 
      receivers <- subset(VemcoReceiverDeployments, water_year == input$water_year)
      color <- "orange"
    }
    
    receivers <- subset(receivers, GPSname != "999")
    
    # Create the leaflet map
    leaflet(data = receivers) %>% 
      addTiles(group = "Open Street Map") %>% 
      # Provide optional basemap tiles
      addProviderTiles(providers$Stamen.Terrain, group = "Stamen Terrain",
                       options = providerTileOptions(noWrap = TRUE)) %>% 
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Esri Nat Geo",
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery",
                       options = providerTileOptions(noWrap = TRUE)) %>%
      # Set the default view
      setView(-122.159729,39.119407, 7) %>% 
      # Display receiver deploments as circle markers
      addCircleMarkers(~GenLon, ~GenLat, 
                       popup = paste(receivers$GEN),
                       layerId = ~uid,
                       radius = 3,
                       stroke = FALSE,
                       color = color, 
                       fillOpacity = 1) %>% 
      # Give control for selecting basemaps
      addLayersControl(
        baseGroups = c("Open Street Map", "Stamen Terrain", "Esri Nat Geo", "Esri World Imagery"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      # Button to reset to original view
      addResetMapButton()
  })
  
  # Produces table output in response to clicks on receiver deployment map markers
  observeEvent(input$map_marker_click, {
    # Essentially the attributes of the clicked receiver marker is assigned to p
    p <- input$map_marker_click
    
    # Display table info for clicked marker
    output$receiver_table <- renderTable({
      
      if (input$receiverType == "Vemco") {
        # Give me the GEN for the clicked marker
        GEN_click = VemcoReceiverDeployments$GEN[VemcoReceiverDeployments$uid == p$id]
        
        # Display all the GPSnames that are associated with that GEN for the given
        # water year along with SN, StartTime, and EndTime
        VemcoReceiverDeployments %>%
          filter(
            GEN == GEN_click & water_year == input$water_year
          ) %>%
          select(
            GPSname, VemcoSN, StartTime, EndTime
          ) %>%
          arrange(StartTime) %>%
          mutate(
            VemcoSN = as.character(VemcoSN),
            StartTime = as.character(StartTime),
            EndTime = as.character(EndTime)
          )
      
      }else {
        GEN_click = ReceiverDeployments$GEN[ReceiverDeployments$uid == p$id]
        
        # Display all the GPSnames that are associated with that GEN for the given
        # water year along with SN, StartTime, and EndTime
        ReceiverDeployments %>%
          filter(
            GEN == GEN_click & water_year == input$water_year
          ) %>%
          arrange(StartTime) %>%
          mutate(
            SN = as.character(SN),
            StartTime = as.character(StartTime),
            #EndTime = as.character(EndTime),
            # If the receiver is RT and its EndTime is NA make it say 'Active' instead
            EndTime = ifelse(RecMake %in% c("ATS SR3017", "Tekno RT") & is.na(EndTime), 
                             "Active", as.character(EndTime))
          ) %>% 
          select(
            GPSname, SN, StartTime, EndTime
          )
      }
    })
  })
  
  
  
  # Hydrology  --------------------------------------------------
  
  # Create a dygraph - interactive flow graph of different CDEC gauges: KES, BND, BTC, WLK
  output$dygraph <- renderDygraph({
    dygraph(comb_flow, main = "Sacramento River Flows (CFS)") %>%
      # Adds date range selector tool, use a year from today through todays date as the default range
      dyRangeSelector(dateWindow = c(Sys.Date() - 365, Sys.Date())) %>% 
      # Individual series customizations
      dySeries("KES", strokeWidth = 1.5, strokePattern = "solid", color = "#008033") %>%
      dySeries("BND", strokeWidth = 1.5, strokePattern = "solid", color = "#668000") %>%
      dySeries("BTC", strokeWidth = 1.5, strokePattern = "solid", color = "#003380") %>%
      dySeries("WLK", strokeWidth = 1.5, strokePattern = "solid", color = "#660080") %>%
      dyOptions(fillGraph = TRUE, fillAlpha = 0.2) %>%
      # Make legend wide enough to fit single line
      dyLegend(width = 400) %>% 
      # Makes it so that mouse hovered series is highlighted
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))
  })
  
  # Create leaflet map that corresponds to the dygraph, it displays the gauge locations 
  output$hydromap <- renderLeaflet({
    # Set colors for each gauge
    pal <- colorFactor(
      palette = c("#668000", "#003380", "#008033", "#660080"),
      domain = c("KES", "BND", "BTC", "WLK")
    )
    
    # Bring in a Sacramento River shapefile
    rivers <- st_read("C:/Users/Tombo/Google Drive/UCSC/R/Shiny/data/GIS/sac_river_dissolve.shp")
    # Transform datum
    rivers <- st_transform(rivers, crs = '+proj=longlat +datum=WGS84')
    # Need to drop z dimension: https://gis.stackexchange.com/questions/253898/adding-a-linestring-by-st-read-in-shiny-leaflet
    rivers <- st_zm(rivers, drop = T, what = "ZM")
    

    leaflet(data = cdec_stations) %>% 
      addPolygons(
        data = rivers, color = "#668db3",
        weight = 3, opacity = 1
      ) %>% 
      addProviderTiles(
        providers$Stamen.Terrain, group = "Stamen Terrain",
        options = providerTileOptions(noWrap = TRUE)) %>% 
      addProviderTiles(
        providers$Stamen.TonerLite, group = "Stamen Toner Lite",
        options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(
        providers$Esri.NatGeoWorldMap, group = "Esri Nat Geo",
        options = providerTileOptions(noWrap = TRUE)) %>%
      setView(mean(cdec_stations$longitude), mean(cdec_stations$latitude), 7) %>% 
      addCircleMarkers(
        ~longitude, ~latitude, 
        popup = ~station_id,
        color = ~pal(station_id),
        fillOpacity = 1.5,
        label = ~station_id,
        # Make labels always appear and offset them to the right of the markr
        labelOptions = labelOptions(
          noHide = T, 
          direction = "right", 
          offset = c(10,0))) %>% 
      addLayersControl(
        baseGroups = c("Stamen Terrain", "Stamen Toner Lite", "Esri Nat Geo"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # Explanatory text for the download button
  output$text1 <- renderText("Click the button below to download the hydrology data,
                             in CSV, currently in view based on the date range selector.")
  output$text2 <- renderText(paste0("Start Date: ", as.Date(ymd_hms(input$dygraph_date_window[[1]]))))
  output$text3 <- renderText(paste0("End Date: ", as.Date(ymd_hms(input$dygraph_date_window[[2]]))))
  
  
  # Reactive value of hydrology data based on date range selection
  hydroDataset <- reactive({
    start_date <- as.Date(ymd_hms(input$dygraph_date_window[[1]]))
    end_date <- as.Date(ymd_hms(input$dygraph_date_window[[2]]))
    
    # First convert comb_flow xts to a dataframe and extract index from rowname into a 
    # column
    comb_flow_subset <- data.frame(date=index(comb_flow), coredata(comb_flow))
    
    # Filter the data based on date range selection values
    comb_flow_subset %>% 
      filter(
        date >= start_date,
        date <= end_date
      )
  })
  
  # Adds download button and allows user to download csv of the hydrology data
  # Currently in, view filtered by user's date range selected
  output$downloadData <- downloadHandler(
    filename = function() {
      start_date <- as.Date(ymd_hms(input$dygraph_date_window[[1]]))
      end_date <- as.Date(ymd_hms(input$dygraph_date_window[[2]]))
      paste(start_date, "_", end_date, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(hydroDataset(), file, row.names = FALSE)
    }
  )
  

  # Outmigration Animation  --------------------------------------------------

  # Reactive expression that retrieves detections from ERDDAP and formats it for 
  # the time step animation
  timestepVar<-  reactive({
    req(input$anim_dataset)
    
    files <- list.files("./data/detections/", full.names = T)
    
    # Choose the file that matches the studyID and read it in
    file <- files[str_detect(files, input$anim_dataset)]
    detections <- read_csv(file)
    
    # Get release info from TaggedFish
    release <- TaggedFish %>% 
      filter(study_id == input$anim_dataset) %>% 
      select(release_latitude, release_longitude, release_river_km) %>% 
      distinct()
    
    detections <- detections %>% 
      mutate(
        date = as.Date(time),
        GenLat = ifelse(SN == 1, LAT, GenLat),
        GenLon = ifelse(SN == 1, LON, GenLon),
        GenRKM = ifelse(SN == 1, release$release_river_km, GenRKM)
      )
    
    # Get the list of unique receiver general locations for the study group
    receiver_loc <- detections %>% 
      select(GEN, GenRKM, GenLat, GenLon) %>% 
      arrange(desc(GenRKM)) %>% 
      distinct()
    
    # Create a grid of all receiver general locations by all dates from the earliest detection date to the 
    # latest detection date
    timestep <- expand.grid(receiver_loc$GEN, seq(min(detections$date), max(detections$date), by = 1),
                            stringsAsFactors = F)
    colnames(timestep) <- c("GEN", "date")
    
    # Summarise detections counts by GEN and date and join into the grid
    timestep <- timestep %>% 
      left_join(detections %>% 
                  select(FishID, GEN, date) %>% 
                  distinct() %>% 
                  group_by(GEN, date) %>% 
                  summarise(num_fish = n()), by = c('GEN', 'date')
      ) %>% 
      mutate(
        num_fish = ifelse(is.na(num_fish), 0, num_fish)
      ) %>% 
      left_join(receiver_loc %>% 
                  select(-GenRKM), 
                by = 'GEN')
  })
  
  # Output leaflet Outmigration Animation of fish outmigration
  # Takes data that has been pre-processed, and visualizes magnitude of unique detections by day at every receiver location
  # Achieved by using addMinicharts which takes a time argument
  output$timemap <- renderLeaflet({
    # data <- read_csv(paste0("./data/Timestep/", input$anim_dataset, ".csv"))
    data <- timestepVar()

      leaflet(data = data, width = "100%", height = "800px") %>%
        addProviderTiles(providers$Stamen.TerrainBackground) %>%
        setView(mean(data$GenLon), mean(data$GenLat), 7) %>%
        addMinicharts(
          data$GenLon, data$GenLat,
          chartdata = data$num_fish,
          time = data$date,
          fillColor = "blue",
          width = 60, height = 60,
          popup = popupArgs(
            showValues = FALSE,
            supValues = data %>% select(GEN, num_fish),
            supLabels = c("GEN", "N = ")
                  ),
          showLabels = TRUE,
          opacity = .7
          )
  })

  # Data Explorer  --------------------------------------------------
  
  # Reactive function that returns the filtered TaggedFish table by selected studyID
  taggedfishVar <-  reactive({
    req(input$data_explorer_datasets)
    
    waiter$show()
    
    # List of studyIDs to query for
    studyids <- input$data_explorer_datasets
    
    # Filter TaggedFish by studyID's
    TaggedFish %>% 
      filter(study_id %in% studyids) %>% 
      rename(
        StudyID = study_id,
        FishID = fish_id,
        Length = fish_length,
        Weight = fish_weight
      )
    
    # # Basic set up for RERDDAP 
    # my_url <- "https://oceanview.pfeg.noaa.gov/erddap/"
    # JSATSinfo <- info('FED_JSATS_taggedfish', url = my_url)
    # 
    # # Function that accepts a studyid and calls tabledap, allows repeated calls since
    # # You can not simply do an %in% query
    # get_erddap <- function(studyid) {
    #   tabledap(JSATSinfo,
    #            fields = c('study_id', 'fish_id',
    #                       'fish_length', 'fish_weight'),
    #            paste0('study_id=', '"',studyid, '"'),
    #            url = my_url,
    #            distinct = T
    #   ) 
    # }
    # 
    # # Retreive ERDDAP data with list of user selected studyIDs, bind together and get distinct rows
    # lapply(studyids, get_erddap) %>% 
    #   bind_rows() %>% 
    #   rename(
    #     StudyID = study_id,
    #     FishID = fish_id,
    #     Length = fish_length,
    #     Weight = fish_weight
    #   ) %>% 
    #   mutate_at(vars(Length, Weight), as.numeric)
    
  })
  
  # Finds the optimal bin size using Freedman-Diaconis rule
  # https://stats.stackexchange.com/questions/798/calculating-optimal-number-of-bins-in-a-histogram
  # Sets that optimal bin width to the default setting in the sliderInput
  output$bin_slider <- renderUI({
    tagged_fish <- taggedfishVar()
    
    bins <- nclass.FD(tagged_fish[,input$variable])
    sliderInput("bin_width", "Bin width", min = 1, max = 50, value = bins)
  })
  
  # Output a summary table - using package arsenal
  output$summarytable <- renderTable({
    
    mylabels <- list(Length = "Length (mm)", Weight ="Weight (g)")
    as.data.frame(summary(tableby(StudyID ~ Length + Weight, data = taggedfishVar(),
                                  control = tableby.control(digits = 2)), labelTranslations = mylabels, text = "html"))
    
  }, sanitize.text.function = function(x) x)
  
  # Create plotly plot
  output$plotly_plot <- renderPlotly({
    tagged_fish <- taggedfishVar()
    
    # Based on the user selected plot type, use different ggplot options 
    plot_type <- switch(input$plot_type,
                        "boxplot" 	= 	geom_boxplot(),
                        "histogram" =	geom_histogram(bins = input$bin_width, alpha=0.5,position="identity"),
                        "density" 	=	geom_density(alpha=.75)
    )
    
    # Create the plotly plot differently depending on plot type
    if (input$plot_type == "boxplot") {
      ggplotly(ggplot(tagged_fish, 
             aes(
               x = StudyID,
               y = get(input$variable),
               fill = StudyID
             )
      ) + 
        plot_type +
        ylab(input$variable) +
        scale_fill_brewer(palette="Accent") +
        theme_classic() +
        theme(legend.position="top", axis.text=element_text(size=12))) 
    } else {
      ggplotly(ggplot(tagged_fish, 
             aes(
               x = get(input$variable),
               fill = StudyID
             )
      ) + 
        plot_type +
        scale_y_continuous(expand = c(0, 0)) +
        xlab(input$variable) +
        scale_fill_brewer(palette="Accent") +
        theme_classic() +
        theme(legend.position="top", axis.text=element_text(size=12))) 
    }
  })

# Time of Day -------------------------------------------------------------
  
  # Reactive list of GEN depending on StudyID selected
  GEN_list <-  reactive({
    # file <- list.files("./data/detections")[str_detect(list.files("./data/detections"), input$time_of_day_input)]
    # detections <- read_csv(paste0("./data/detections/", file))
    detections <- timeofdayVar()
    
    unique(detections$GEN)
  })
  
  # Updates allowable GEN selection input using GEN_list()
  observe({
    updateSelectInput(session, "time_of_day_GEN",
                      choices = GEN_list()
    )
  })
  
  # Reactive expression that retrieves detections from ERDDAP and formats it for 
  # the time step animation
  timeofdayVar<-  reactive({
    req(input$time_of_day_input)
    
    waiter$show()
    
    # Directory of detection files
    files <- list.files("./data/detections/", full.names = T)
    
    # Function to read in appropriate csv by given studyID
    read_detects_files <- function(studyID) {
      file <- files[str_detect(files, studyID)]
      detections <- read_csv(file)
    }
    
    # Read all studyID CSVs from input and bind together
    detections <- lapply(input$time_of_day_input, read_detects_files) %>% 
      bind_rows() %>% 
      mutate(hour = hour(time))
    
    
    # # Basic set up for RERDDAP 
    # my_url <- "https://oceanview.pfeg.noaa.gov/erddap/"
    # JSATSinfo <- info('FED_JSATS_detects', url = my_url)
    # 
    # detections <- tabledap(JSATSinfo,
    #                        fields = c('fish_id', 'time', 'receiver_general_location', 
    #                                   'receiver_river_km'),
    #                        paste0('study_id=', '"',input$time_of_day_input, '"'),
    #                        url = my_url,
    #                        distinct = T
    # ) %>% 
    #   left_join(
    #     ReceiverDeployments %>% 
    #       select(GEN, GenLat, GenLon) %>% 
    #       distinct(), by = c("receiver_general_location" = "GEN")
    #   ) %>% 
    #   mutate(
    #     time = ymd_hms(time),
    #     hour = hour(time)
    #     ) %>% 
    #   rename(
    #     GEN = receiver_general_location,
    #     FishID = fish_id,
    #     GenRKM = receiver_river_km 
    #   ) %>% 
    #   mutate(
    #     GenRKM = as.numeric(GenRKM)
    #   )
  })

  output$time_of_day_plot <- renderPlotly({
    # Retrieve the file name in the detections folder that corresponds to the StudyID selected by the user
    # Look at all files in the detections folder, choose the one that contains the StudyID string in its name
    # file <- list.files("./data/detections")[str_detect(list.files("./data/detections"), input$time_of_day_input)]
    
    # Read in the appropriate csv
    # detections <- read_csv(paste0("./data/detections/", file))
    
    # # Handle for if a csv DT is given in character format
    # if (class(detections$DT) == "character") {
    #   # Convert to POSIXct using these formats (hopefully these catch)
    #   detections <- detections %>%
    #     mutate(
    #       datetime = as.POSIXct(detections$DT, tz = 'UTC',
    #                             tryFormats = c("%m/%d/%Y %H:%M",
    #                                            "%m/%d/%Y %H:%M:%S")),
    #       hour = hour(datetime)
    #     )
    # }else {
    #   # Case if DT is not character, Excel serial numeric format
    #   detections <- detections %>%
    #     mutate(
    #       datetime = as.POSIXct(DT * (60*60*24), tz = "UTC", origin = "1899-12-30"),
    #       hour = hour(datetime)
    #     )
    # }
    
    detections <- timeofdayVar()
    
    # Get first detection time of each fish at each GEN
    filtered <- detections  %>% 
      group_by(FishID, GEN) %>% 
      summarise(first_detection = min(hour))

    # If user selected to examine time of arrivals by GEN filter the detection file by the GEN they chose
    if (input$time_of_day_radio == "By GEN") {
      filtered <- filtered %>% 
        filter(GEN == input$time_of_day_GEN)
    }
    
    # Bins data by hour explicitly from 0:23 hours
    # Necessary to do it this way instead of dplyr group_by b/c not all hours may be represented
    # hour_freq <- data.frame("hour" = 0:23, freq = 0)
    
    hour_freq <- data.frame("hour" = 0:23) %>% 
      left_join(filtered %>% 
                  group_by(first_detection) %>% 
                  count(), by = c("hour" = "first_detection")) %>% 
      # Replace NA with 0
      rename(freq = n) %>% 
      mutate(freq = ifelse(is.na(freq), 0, freq),
             percent_pass = ((freq / sum(freq)) * 100)
             )
    
    # for (i in 1:nrow(filtered)) {
    #   hour1 = filtered$first_detection[i]
    #   
    #   hour_freq[hour_freq$hour == hour1,]$freq <- hour_freq[hour_freq$hour == hour1,]$freq +1
    #   
    # }
    # 
    # hour_freq$percent_pass <- (hour_freq$freq / sum(hour_freq$freq)) * 100
    
    # Find Sunset, Sunrise for earliest and latest date using mean lat/lon
    receivers <- detections %>% 
      select(GEN, LAT, LON) %>% 
      distinct()
    
    avg_lat = mean(receivers$LAT)
    avg_lon = mean(receivers$LON)
    
    # Get earliest date and latest dates to compare differences in these times
    earliest_date <- as.Date(min(detections$time))
    latest_date <- as.Date(max(detections$time))
    
    earliest_sr_ss <- getSunlightTimes(earliest_date, avg_lat, avg_lon, keep = c("sunrise", "sunset"), tz = "America/Los_Angeles")
    latest_sr_ss <- getSunlightTimes(latest_date, avg_lat, avg_lon, keep = c("sunrise", "sunset"), tz = "America/Los_Angeles")
    
    earliest_sunrise <- hour(earliest_sr_ss$sunrise) + (minute(earliest_sr_ss$sunrise)/60) + (second(earliest_sr_ss$sunrise)/3600)
    earliest_sunset <- hour(earliest_sr_ss$sunset) + (minute(earliest_sr_ss$sunset)/60) + (second(earliest_sr_ss$sunset)/3600)
    
    latest_sunrise <- hour(latest_sr_ss$sunrise) + (minute(latest_sr_ss$sunrise)/60) + (second(latest_sr_ss$sunrise)/3600)
    latest_sunset <- hour(latest_sr_ss$sunset) + (minute(latest_sr_ss$sunset)/60) + (second(latest_sr_ss$sunset)/3600)
    
    # Convert hour to factor and give it specific levels, so that it will appear as 12:23, 0:11 on the x-axis
    hour_freq$hour <- factor(hour_freq$hour, levels = c(12:23, 0:11))
    
    # Create bar plot to show proportion of detections by hour
    p <- ggplot(data = hour_freq, mapping = aes(x = hour, y = percent_pass)) +
      # Add rectangle representing "nighttime", using the earliest sunrise and earliest sunset values, had to use 
      # ymax = 999999 because plotly won't take Inf
      # ggplot version = geom_rect(xmin = earliest_sunrise, xmax = earliest_sunset, ymin = 0, ymax = 999999, fill = "grey70", alpha = 0.5) +
      geom_rect(data=hour_freq, aes(NULL,NULL,xmin=earliest_sunrise,xmax=earliest_sunset),
                ymin=0,ymax=999999, size=0.5, alpha=0.2) +
      geom_col() +
      # Add lines to represent the latest sunrise and latest sunset values
      geom_vline(xintercept = latest_sunrise, linetype = "dashed", size = 1.25) +
      geom_vline(xintercept = latest_sunset, linetype = "dashed", size = 1.25) +
      ylab("% Smolts Passed") +
      xlab("Time of day (h)") + 
      scale_y_continuous(expand = c(0, 0)) +
      theme_classic() +
      theme(
        text = element_text(size = 15),
        # Make caption text small and left justify
        plot.caption = element_text(size = 10, hjust = 0),
        plot.margin = margin(b = 20)
      )
    
    if (input$time_of_day_radio == "By GEN") {
      # Get the GenRKM value for this GEN being examined, for use in labeling in plot
      genrkm <- detections %>% 
        filter(GEN == input$time_of_day_GEN) %>% 
        select(GenRKM) %>% 
        distinct() %>% 
        unlist()
      
      p <- (p + labs(
        title = paste0("% Smolt arrivals at ", input$time_of_day_GEN),
        subtitle = paste0("GenRKM = (", genrkm, ")")))
    } else {
      p <- (p + labs(
        title = "% Smolt arrivals"))
    }
    
    ggplotly(p)
  })
  
  # Output the plot caption using renderUI instead of in ggplot because it 
  # will autowidth the same length as the plot
  # Couldn't figure out how to autowidth in ggplot
  output$time_of_day_caption <- renderUI({
    # Retrieve the file name in the detections folder that corresponds to the StudyID selected by the user
    # Look at all files in the detections folder, choose the one that contains the StudyID string in its name
    # file <- list.files("./data/detections")[str_detect(list.files("./data/detections"), input$time_of_day_input)]
    # 
    # # Read in the appropriate csv
    # detections <- read_csv(paste0("./data/detections/", file))
    # 
    # # Handle for if a csv DT is given in character format
    # if (class(detections$DT) == "character") {
    #   # Convert to POSIXct using these formats (hopefully these catch)
    #   detections <- detections %>% 
    #     mutate(
    #       datetime = as.POSIXct(detections$DT, tz = 'UTC', 
    #                             tryFormats = c("%m/%d/%Y %H:%M",
    #                                            "%m/%d/%Y %H:%M:%S")),
    #       hour = hour(datetime)
    #     )
    # }else {
    #   # Case if DT is not character, Excel serial numeric format
    #   detections <- detections %>% 
    #     mutate(
    #       datetime = as.POSIXct(DT * (60*60*24), tz = "UTC", origin = "1899-12-30"),
    #       hour = hour(datetime)
    #     )
    # }
    
    detections <- timeofdayVar() %>% 
      mutate(time = ymd_hms(time))
    # Get earliest date and latest dates to compare differences in these times
    earliest_date <- as.Date(min(detections$time))
    latest_date <- as.Date(max(detections$time))
    
    paste0("The histogram displays the frequency of smolt detections as a function of hour of day. The gray box represents \nhours of nighttime (sunset to sunrise) at the earliest date of detections (", earliest_date, "). The dotted lines represent \nhours of nighttime at the latest date of detections (", latest_date, ").")
  })
  

# Survival ----------------------------------------------------------------

  # Takes user selected studyIDs and retrieves data from ERDDAP
  gtVar <-  reactive({
    req(input$survival_datasets)
    
    waiter$show()
    
    # List of studyIDs to query for
    studyids <- input$survival_datasets
    
    # Directory of detection files
    files <- list.files("./data/detections/", full.names = T)
    
    # Function to read in appropriate csv by given studyID
    read_detects_files <- function(studyID) {
      file <- files[str_detect(files, studyID)]
      detections <- read_csv(file)
    }

    # Read all studyID CSVs from input and bind together
    lapply(studyids, read_detects_files) %>% 
      bind_rows() %>% 
      left_join(
        TaggedFish %>% 
          select(FishID = fish_id, Length = fish_length, Weight = fish_weight),
        by = "FishID"
      ) %>% 
      rename(StudyID = studyID)

    
    # # Basic set up for RERDDAP 
    # my_url <- "https://oceanview.pfeg.noaa.gov/erddap/"
    # JSATSinfo <- info('FED_JSATS_detects', url = my_url)
    # 
    # # Function that accepts a studyid and calls tabledap, allows repeated calls since
    # # You can not simply do an %in% query
    # get_erddap <- function(studyid) {
    #   tabledap(JSATSinfo,
    #            fields = c('study_id',  'fish_id', 'fish_length', 'fish_weight', 
    #                       'receiver_general_location', 'receiver_region'),
    #            paste0('study_id=', '"',studyid, '"'),
    #            url = my_url
    #   )
    # }
    # 
    # # Retreive ERDDAP data with list of user selected studyIDs, bind together and get distinct rows
    # df<- lapply(studyids, get_erddap) %>% 
    #   bind_rows() %>% 
    #   distinct()
    
  })
  
  output$gt_output <- render_gt({

    detections <- gtVar()
    # Format the GT table
    make_gt_table <- function(x) {
      
      release <- TaggedFish %>% 
        filter(study_id %in% input$survival_datasets) %>% 
        mutate(k_factor = 100 * (fish_weight / (fish_length/10)^3)) %>% 
        group_by(study_id) %>% 
        summarise(
          count = n_distinct(fish_id),
          Length = mean(fish_length),
          Weight = mean(fish_weight),
          Condition = mean(k_factor)
        )
      
      detects <- detections %>% 
        filter(GEN %in% c("I80-50_Br", "BeniciaE", "BeniciaW", "GoldenGateE", "GoldenGateW")) %>% 
        mutate(
          GEN = ifelse(GEN == "GoldenGateE", "GoldenGate", GEN),
          GEN = ifelse(GEN == "GoldenGateW", "GoldenGate", GEN),
          GEN = ifelse(GEN == "BeniciaE", "Benicia", GEN),
          GEN = ifelse(GEN == "BeniciaW", "Benicia", GEN),
          GEN = ifelse(GEN == "I80-50_Br", "I80", GEN)
        ) %>% 
        select(StudyID, FishID, GEN) %>% 
        distinct() %>% 
        group_by(StudyID, GEN) %>% 
        summarise(
          count = n()
        ) 
      
      # Transform data so that each GEN is a column and unique detects is the value
      detects <- pivot_wider(detects, names_from = "GEN", values_from = "count")
      detects <- detects %>% select(StudyID, I80, Benicia, GoldenGate)
      
      
      summary <- release %>% 
        left_join(detects, by = c("study_id" = "StudyID"))
      
      summary %>% 
        gt(rowname_col = "study_id") %>% 
        tab_header(
          title = "Minimum Survival Summary"
        ) %>% 
        tab_stubhead(label = md("**Study Group**")) %>% # md() wrapper allows text styling with MarkDown
        tab_footnote(
          footnote = "Mean values",
          cells_column_labels(columns = c(2, 3, 4))
        ) %>% 
        cols_label(
          count = "Fish Tagged",
          Condition = "K Factor",
          I80 = "Fish detected at I-80 Bridge",
          Benicia = "Fish detected at Benicia",
          GoldenGate = "Fish detected at Golden Gate"
        ) %>% 
        fmt_number(
          columns = vars(Length, Weight, Condition),
          decimals = 2
        )
    }
    
    # Make a GT table using the reactive function gtVar
    make_gt_table(gtVar())
  })
  
  # Reactive expression for cumulative survival input
  cumsurvivalVar<-  reactive({
    req(input$cumsurvival_datasets)
    
    name <- input$cumsurvival_datasets
    
    file <- list.files("./Survival/outputs/Cumulative Survival", name, full.names = T)
    read_csv(file)
  })
  
  output$plotly_survival_output <- renderPlotly({
    # plot <- ggplot(data = cumsurvivalVar(), mapping = aes(x = RKM, y = cum.phi, color = StudyID)) +
    #   geom_point() +
    #   geom_line() +
    #   scale_x_reverse()
    
    plot_ly(cumsurvivalVar(), x = ~RKM, y = ~cum.phi, color = ~StudyID, type = 'scatter', mode = 'lines+markers',
            hoverinfo = 'text',
            text = ~paste('</br> cum.phi: ', cum.phi,
                          '</br> LCI: ', LCI,
                          '</br> UCI: ', UCI,
                          '</br> GEN: ', GEN,
                          '</br> RKM: ', RKM,
                          '</br> Raw number of fish to site: ', n_fish
                          )) %>% 
      layout(xaxis = list(autorange = "reversed",
                          showline = FALSE,
                          zeroline = FALSE)) %>% 
      layout(yaxis = list(showline = FALSE,
                          zeroline = FALSE))

  })
  
  

# Movement ----------------------------------------------------------------

  # Takes user selected studyIDs and retrieves data from ERDDAP
  movementVar <-  reactive({
    req(input$movement_dataset)
    
    waiter$show()
    
    # List of studyIDs to query for
    studyids <- input$movement_dataset
    
    # Directory of detection files
    files <- list.files("./data/detections/", full.names = T)
    
    read_detects_files <- function(studyID) {
      file <- files[str_detect(files, studyID)]
      detections <- read_csv(file)
    }
    
    df <- lapply(studyids, read_detects_files) %>% 
      bind_rows()
    
    # # Basic set up for RERDDAP 
    # my_url <- "https://oceanview.pfeg.noaa.gov/erddap/"
    # JSATSinfo <- info('FED_JSATS_detects', url = my_url)
    # 
    # # Function that accepts a studyid and calls tabledap, allows repeated calls since
    # # You can not simply do an %in% query
    # get_erddap <- function(studyid) {
    #   tabledap(JSATSinfo,
    #            fields = c('study_id',  'fish_id', 'time', 'fish_release_date', 'release_river_km',
    #                       'receiver_general_location', 'receiver_general_river_km ', 'receiver_region'),
    #            paste0('study_id=', '"',studyid, '"'),
    #            url = my_url
    #   )
    # }
    # 
    # # Retreive ERDDAP data with list of user selected studyIDs, bind together and get distinct rows
    # df <- lapply(studyids, get_erddap) %>% 
    #   bind_rows() %>% 
    #   distinct()
    
    # df <- df %>% 
    #   mutate(
    #     time = ymd_hms(time),
    #     fish_release_date = mdy_hms(fish_release_date),
    #     release_river_km = as.numeric(release_river_km),
    #     receiver_general_river_km = as.numeric(receiver_general_river_km)
    #   ) %>% 
    #   rename(
    #     FishID = fish_id,
    #     GEN = receiver_general_location,
    #     GenRKM = receiver_general_river_km,
    #     Region = receiver_region
    #   )
    
    df <- df %>% 
      left_join(
        TaggedFish %>% 
          select(FishID = fish_id, fish_release_date, release_river_km),
        by = "FishID"
      ) %>% 
      mutate(fish_release_date = mdy_hms(fish_release_date))
    
    # Calculate time from release to receiver and dist to receiver
    min_detects <- df %>% 
      group_by(FishID, GEN, GenRKM, fish_release_date, release_river_km) %>% 
      summarize(
        min_time = min(time)
      ) %>% 
      mutate(
        km_from_rel = release_river_km - GenRKM,
        days_since_rel = difftime(min_time, fish_release_date, units = "days")
      ) %>% 
      arrange(FishID, min_time) %>% 
      mutate(km_day = km_from_rel / as.numeric(days_since_rel)) %>% 
      filter(km_from_rel > 0,
             abs(km_day) < 200)
    
    speeds <- min_detects %>% 
      group_by(GEN, GenRKM) %>% 
      summarize(
        N = n(),
        min_travel_days = min(days_since_rel),
        median_travel_days = median(days_since_rel),
        max_travel_days = max(days_since_rel),
        mean_km_day = mean(km_day),
        median_km_day = median(km_day)
      ) %>% 
      ungroup() %>% 
      mutate_if(is.difftime, as.numeric) %>% 
      arrange(desc(GenRKM))
    
    
    # # Build reach names to look at reach specific travel
    # # Need to filter out Delta and bypasses
    # list_of_GEN <- df %>% 
    #   select(GEN, GenRKM, Region) %>% 
    #   filter(
    #     !Region %in% c("Sutter Bypass", "Yolo Bypass", "East Delta", "West Delta",
    #                    "North Delta", "South Delta")
    #   ) %>% 
    #   distinct() %>% 
    #   arrange(desc(GenRKM)) %>% 
    #   pull(GEN)
    # 
    # reach_names <- c()
    # for (i in 1:(length(list_of_GEN)-1)) {
    #   reach_names <- c(reach_names, paste0(list_of_GEN[i], "_to_", 
    #                                        list_of_GEN[i+1]))
    # }
    # 
    # 
      
  })
  
  output$movement_gt <- render_gt({
    
    df <- movementVar()
    
    gt_tbl <- gt(data = df, rowname_col = "GEN")
    
    gt_tbl %>% 
      tab_header(
        title = md("**Travel time summary statistics**"),
        subtitle = "Days and travel rates calculated from point of release to general
        receiver location"
      ) %>%
      tab_stubhead(label = "General Receiver Location") %>% 
      fmt_number(
        decimals = 1,
        columns = vars(min_travel_days, median_travel_days, max_travel_days,
                       mean_km_day, median_km_day)
      ) %>% 
      cols_label(
        min_travel_days = "Min",
        median_travel_days = "Median", 
        max_travel_days = "Max", 
        mean_km_day = "Mean", 
        median_km_day = "Median"
      ) %>% 
      tab_spanner(
        label = "Time (days)",
        columns = vars(min_travel_days, median_travel_days, max_travel_days)
      ) %>%
      tab_spanner(
        label = "Travel rate (km/day)",
        columns = vars(mean_km_day, median_km_day)
      )
    
  })  
  
}

shinyApp(ui = ui, server = server)
