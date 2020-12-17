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
#library(slickR) # JS image carousel
library(waiter) # Loading animations
# library(httr) # Check HTTP status for CDEC/ERDDAP
library(vroom) # Fastest way to read csv
library(sf) # To display gis layers
library(DT)

# Global ------------------------------------------------------------------

# Load data
# --- ReceiverDeployments
# --- TaggedFish
# --- VemcoReceiverDeployments
# --- Detections
# --- CDEC Flows

# Check for changes every 90 days
  # If last dl was > 90 days update files
  # else read in csv

# Download updates every 90 days
last_checked_date <- read_rds("last_checked_date.RDS")

## Load ReceiverDeployments and TaggedFish
# If last update check was < 90 days read in CSVs (much faster load times)
if (as.numeric(Sys.Date() - last_checked_date) < 90) {
  ReceiverDeployments <- vroom("./data/ReceiverDeployments.csv")
  TaggedFish <- vroom("./data/TaggedFish.csv")
  
} else { 
  # Else check if ERDDAP is online, x returns TRUE if database is down or "Timeout"
  # if the http check timeouts out 
  x <- tryCatch(http_error("oceanview.pfeg.noaa.gov/erddap/tabledap/FED_JSATS_receivers.html", 
                           timeout(3)), error=function(e) print("Timeout"))
  
  # If the database isn't working then read csv
  if (x == TRUE | x == "Timeout") {
    ReceiverDeployments <- vroom("./data/ReceiverDeployments.csv")
    TaggedFish <- vroom("./data/TaggedFish.csv")
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
    write_csv(ReceiverDeployments, "./data/ReceiverDeployments.csv")
    
    ## Download TaggedFish
    my_url <- "https://oceanview.pfeg.noaa.gov/erddap/"
    JSATSinfo <- info('FED_JSATS_taggedfish', url = my_url)
    
    TaggedFish <- tabledap(JSATSinfo, url = my_url)  
    
    # Fix column names and correct column types
    TaggedFish <- TaggedFish %>% 
      mutate_at(vars(tag_weight, tag_pulse_rate_interval_nominal, tag_warranty_life,
                     fish_length, fish_weight, release_latitude, release_longitude,
                     release_river_km), as.numeric) 
    
    # Save latest update to file
    write_csv(TaggedFish, "./data/TaggedFish.csv")
    
    # Change the last saved date to today
    last_checked_date <- Sys.Date()
    saveRDS(last_checked_date, "last_checked_date.RDS")
  }
}
  
# This must be manually updated
VemcoReceiverDeployments <- vroom("./data/VemcoReceiverDeployments.csv") %>%
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

#### List of studyIDs relevant to the project and selectable in the app
#### Update this list as needed to allow for additional studyIDs to be selected
studyid_list <- c("BC_Jumpstart_2019", "CNFH_FMR_2019", "ColemanFall_2012", 
                  "ColemanFall_2013", "ColemanFall_2016", "ColemanFall_2017", 
                  "ColemanLateFall_2018", "ColemanLateFall_2019", 
                  "ColemanLateFall_2020", "DeerCk_SH_Wild_2018", 
                  "DeerCk_Wild 2019", "DeerCk_Wild_2017","DeerCk_Wild_2018", 
                  "FR_Spring_2013", "FR_Spring_2014", "FR_Spring_2015", 
                  "FR_Spring_2019","MillCk_SH_Wild_2015", "MillCk_SH_Wild_2016", 
                  "MillCk_Wild 2019", "MillCk_Wild_2013", "MillCk_Wild_2014", 
                  "MillCk_Wild_2015", "MillCk_Wild_2016", "MillCk_Wild_2016_DS",
                  "MillCk_Wild_2017", "MillCk_Wild_2018", "Nimbus_Fall_2016", 
                  "Nimbus_Fall_2017", "Nimbus_Fall_2018", "RBDD_2017", "RBDD_2018", 
                  "SB_Spring_2015", "SB_Spring_2016", "SB_Spring_2017", 
                  "SB_Spring_2018", "SB_Spring_2019", "Winter_H_2013", 
                  "Winter_H_2014", "Winter_H_2015", "Winter_H_2016", 
                  "Winter_H_2017", "Winter_H_2018", "Winter_H_2019")

survivalStudyIDs <- files <- unlist(strsplit(
  list.files("./data/Survival/Reach Survival Per 10km"), "_reach_survival.csv"))


## Load Hydrology data from CDEC

# Gather flow data from CDEC, save to file to reduce calls to CDEC which is 
# intermittently down
comb_flow <- vroom("./data/comb_flow.csv", col_types = c(Index = "D", KES = "d",
                                                         BND = "d", BTC = "d",
                                                         WLK = "d"))


# Update file with new flow data if it has been over 30 days since last download
if (as.numeric(Sys.Date() - max(comb_flow$Index)) > 30) {
  # Choose CDEC gauges to display
  gauges <- c("KES", "BND", "BTC", "WLK")
  
  # Apply cdec_datasets() on list of gauges, to get station metadata
  # Then apply function to filter to only look at information from sensor 20 
  # and 23 (river flow sensors) Then bind the rows together, then find what the 
  # max start date was, ultimately to get the start_date I want to use
  # That all the gauges are included in
  start_date <- max((lapply(lapply(gauges, cdec_datasets), 
                            function(x) filter(x, sensor_number %in% 
                                                 c("20", "23"))) %>% 
                       bind_rows())$start)
  
  
  # apply the list of gauges to function that queries CDEC to get daily flow 
  # then turns it into an xts (time series object) object which is needed for dygraphs
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
  
  # Close connection with comb_flow.csv so that I can overwrite it with new data
  rm(comb_flow)
  write_csv(comb_flow2, "./data/comb_flow.csv")
  
}else {
  comb_flow <- as.xts(read.csv.zoo("./data/comb_flow.csv"))
}

cdec_stations <- vroom("./data/cdec_stations.csv")

# UI ----------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("flatly"),
  use_waitress(),
  navbarPage("Central Valley Enhanced Acoustic Tagging Project",
             tabPanel("Background", 
                      mainPanel(
                        uiOutput("background")#,
                        #slickROutput("slick_output", width = '500px', height = '500px')
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
                          dataTableOutput("receiver_table")
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
                                     c("All Detections", "By General Location")
                        ),
                        conditionalPanel(
                          condition = "input.time_of_day_radio == 'By General Location'",
                          selectInput("time_of_day_GEN", "Select a General Location",
                                      "")
                        ),
                        plotlyOutput("time_of_day_plot"),
                        uiOutput("time_of_day_caption")
                      )
             ),
             tabPanel("Survival",
                      tabsetPanel(
                        tabPanel("Cumulative Survival",
                                 headerPanel("Select Options"),
                                 sidebarPanel(
                                   uiOutput("cumSurvSelect"),
                                   # selectInput("cumsurvival_datasets", "Study Group",
                                   #             choices = survivalStudyIDs),
                                   # ### These need to be manually updated
                                   # choices = c("ColemanFall", "ColemanLateFall", "DeerCk", "MillCk",
                                   #             "RBDD")) #, "Sutter Bypass", "Winter"
                                   radioButtons("cumsurvival_radio", 
                                                "View",
                                                c("Plot", "Table")
                                   ),
                                   leafletOutput("survival_map2")
                                 ),
                                 mainPanel(
                                   conditionalPanel(
                                     condition = "input.cumsurvival_radio == 'Plot'",
                                     plotlyOutput("plotly_survival_output")
                                   ),
                                   conditionalPanel(
                                     condition = "input.cumsurvival_radio == 'Table'",
                                     dataTableOutput('cumSurvDT')
                                   )
                                 )
                        ),
                        tabPanel("Reach Survival",
                                 headerPanel("Select Options"),
                                 sidebarPanel(
                                   uiOutput("reachSurvSelect"),
                                   # selectInput("reachSurvInput", "Study Group",
                                   #             choices = survivalStudyIDs),
                                   # ### These need to be manually updated
                                   # choices = c("ColemanFall", "ColemanLateFall", "DeerCk", "MillCk",
                                   #             "RBDD")) #, "Sutter Bypass", "Winter"
                                   radioButtons("reachSurvRadio", 
                                                "View",
                                                c("Plot", "Table")
                                   ),
                                   leafletOutput("reachSurvMap")
                                 ),
                                 mainPanel(
                                   conditionalPanel(
                                     condition = "input.reachSurvRadio == 'Plot'",
                                     plotlyOutput("reachSurvPlotly")
                                   ),
                                   conditionalPanel(
                                     condition = "input.reachSurvRadio == 'Table'",
                                     dataTableOutput('reachSurvDT')
                                   )
                                 )
                        )
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
  
  waitress <- Waitress$new("#background", infinite = T, hide_on_render = T)

  # Background --------------------------------------------------------------
  output$background <- renderUI({
    waitress$start(h3("Loading..."))
    withTags({
      div(class="header", checked=NA,
          h1("Background"),
          p("Since 2012, juvenile salmon have been tagged and tracked throughout 
            California's Central Valley (CCV) using Juvenile Salmon Acoustic 
            Telemetry System (JSATS) technology. This technology allows 
            researchers to monitor the movement and survival rates of various 
            populations over 500 river kilometers, from Redding to the Pacific 
            Ocean. The data compiled here is a result of the hard work and 
            coordination between various federal and state agencies, universities, 
            and private consultants, with the goal of conserving and restoring 
            California's once abundant, but now imperiled, Chinook salmon 
            populations. This data is open access and is managed and hosted by 
            the National Marine Fisheries Service here:", 
            a(href="https://oceanview.pfeg.noaa.gov/erddap/tabledap/FED_JSATS_detects.html", 
              "https://oceanview.pfeg.noaa.gov/erddap/tabledap/FED_JSATS_detects.html"),
            "."),
          br(),
          h3("Receiver Deployments"),
          p("An interactive map of acoustic receivers (JSATS, Vemco, Real-time) 
            deployed throughout the CCV by water year, which is defined as the 
            12 month period beginning October 1 and ending September 30 of the 
            following calendar year (i.e. water year 2017 spans 10/1/17-9/30/18). 
            The deployment period of individual receivers is displayed in a 
            table below the map, once a general location is clicked. This map is 
            useful to identify a particular site of interest, and to see when 
            coverage existed in that area and which receiver(s) potentially 
            recorded migrating fish. "),
          br(),
          h3("Hydrology"),
          p("The hydrology of the Sacramento River is highly variable, and is 
            largely driven by storm events and associated runoff in the winter, 
            followed by dam controlled releases for agricultural purposes in the 
            summer. This interactive graph of Sacramento River flows (cubic feet 
            per second) at four different ", 
            a(href="https://cdec.water.ca.gov/index.html", "CDEC"), 
            " (California Data Exchange Center) stations: ", 
            a(href="https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=KES", "KES"), 
            " (Keswick Reservoir), ", 
            a(href="https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=BND", "BND"), 
            " (Bend Bridge), ",
            a(href="https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=BTC", "BTC"), 
            " (Butte City), ", 
            " and ",
            a(href="https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=WLK", "WLK"), 
            " (Wilkins Slough), displays the annual and inter-annual variation 
            in flows from upstream to downstream gauging stations. This plot was 
            created using the R package ",
            a(href="https://rstudio.github.io/dygraphs/", "dygraphs"),"."),
          br(),
          h3("Outmigration Animation"),
          p("The Outmigration Animation tab visualizes fish outmigration using 
            detection data over time. Unique fish detections at each receiver 
            location are plotted by day, and numbers of unique detections per 
            receiver are overlaid in each point. This animation helps to 
            visualize the movement of fish across a broad geographic landscape, 
            and demonstrates the differences in outmigration timing across space 
            and time among specific populations. This animation was created 
            using the R packages  ", 
            a(href="https://rstudio.github.io/leaflet/", "leaflet"),
            "and ",
            a(href="https://github.com/bhaskarvk/leaflet.extras", "leaflet.extras"),
            "."),
          br(),
          h3("Data Explorer"),
          p("Data explorer allows users to look at the tagged fish data 
            individually or across different study populations in a number of 
            ways. Plots can be created interactively, choosing variables to 
            explore (length, weight), and with different plot types (boxplot, 
            histogram, density) as well as summary tables."),
          br(),
          h3("Time of Day"),
          p("Time of day allows users to visually explore behavioral differences 
            in fish movement between night and day throughout the migratory 
            corridor. Using detection times we can look at the distribution in 
            movement times for an entire study group, or the movement times for 
            a specific receiver location. "),
          br(),
          h3("Survival"),
          p("Survival estimates are calculated by summarizing the detection 
            history of study populations, and assuming a fish has died when it 
            is not detected by subsequent downstream receivers. We used a CJS 
            survival model in RMark to estimate reach specific (survival per 10 
            river kilometers) and cumulative (from release location to last 
            downstream detection) survival rates. We used a simple survival model 
            (survival and detection efficiency is a function of time) to derive 
            these estimates. For each survival tab, a map is displayed with all 
            receiver locations used to generate survival for each population, 
            and a figure and table displays the estimates and associated error. 
            The unique number of fish detected at each receiver location is also 
            provided."),
          br(),
          h3("Movement"),
          p("Fish movement is summarized by study population in a table format. 
            For each receiver location, the minimum, median, and maximum travel 
            time is calculated in days and kilometers per day. The number of 
            unique fish detected at each receiver location is displayed as well. 
            We will update this page to display travel times in an interactive 
            plot soon, so stay tuned.")
      )
    })
  })
  
  # output$slick_output <- renderSlickR({
  #   imgs <- list.files("./photos/", full.names = TRUE)
  #   slickR(imgs, width = '500px', height = '500px') +
  #     settings(autoplay = TRUE, autoplaySpeed = 3000)
  # })

  
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
      receivers <- subset(ReceiverDeployments, water_year == input$water_year & 
                            !(RecMake %in% c("ATS SR3017", "Tekno RT")))
      color <- "purple"
    }else if (input$receiverType == "Real-time"){
      receivers <- subset(ReceiverDeployments, water_year == input$water_year & 
                            RecMake %in% c("ATS SR3017", "Tekno RT"))
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
        baseGroups = c("Open Street Map", "Stamen Terrain", "Esri Nat Geo", 
                       "Esri World Imagery"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      # Button to reset to original view
      addResetMapButton()
  })
  
  prevRecType <- reactiveVal("")
  prevYear <- reactiveVal("")

  # Produces table output in response to clicks on receiver deployment map markers
  observeEvent(input$map_marker_click, {
    # Essentially the attributes of the clicked receiver marker is assigned to p
    p <- input$map_marker_click
    
    if (input$receiverType == "Vemco") {
      
      # Give me the GEN for the clicked marker
      GEN_click = VemcoReceiverDeployments$GEN[VemcoReceiverDeployments$uid == p$id]
      
      prevRecType(input$receiverType)
      prevYear(input$water_year)
      
      df <- VemcoReceiverDeployments %>%
        filter(
          GEN == GEN_click & water_year == input$water_year
        ) %>%
        select(
          GPSname, VemcoSN, StartTime, EndTime, GenLat, GenLon
        ) %>%
        arrange(StartTime) %>%
        mutate(
          VemcoSN = as.character(VemcoSN),
          StartTime = as.character(StartTime),
          EndTime = as.character(EndTime)
        )
      
      output$receiver_table <- renderDataTable(df)
      
      
    } else {
      GEN_click = ReceiverDeployments$GEN[ReceiverDeployments$uid == p$id]
      
      prevRecType(input$receiverType)
      prevYear(input$water_year)
      
      # Display all the GPSnames that are associated with that GEN for the given
      # water year along with SN, StartTime, and EndTime
      df <-  ReceiverDeployments %>%
        filter(
          GEN == GEN_click & water_year == input$water_year
        ) %>%
        arrange(StartTime) %>%
        mutate(
          SN = as.character(SN),
          StartTime = as.character(StartTime),
          # If the receiver is RT and its EndTime is NA make it say 'Active' instead
          EndTime = ifelse(RecMake %in% c("ATS SR3017", "Tekno RT") & is.na(EndTime),
                           "Active", as.character(EndTime))
        ) %>%
        select(
          GPSname, SN, StartTime, EndTime, GenLat, GenLon
        )
      
      output$receiver_table <- renderDataTable(df)
    }
    
  })
  
  # If user switches receiver type display an empty table
  observeEvent(input$receiverType, {
    if (!is_empty(prevRecType()) & input$receiverType != prevRecType()) {
      df <- datatable(
        data.frame(GPSname = NA, 
        SN = NA, 
        StartTime = NA, 
        EndTime = NA,
        GenLat = NA,
        GenLon = NA)
      )
      
      output$receiver_table <- renderDataTable(df)
      
    }
  })
  
  # If user switches year display an empty table
  observeEvent(input$water_year, {
    if (!is_empty(prevYear()) & input$water_year != prevYear()) {
      df <- datatable(
        data.frame(GPSname = NA, 
        SN = NA, 
        StartTime = NA, 
        EndTime = NA,
        GenLat = NA,
        GenLon = NA)
      )
      
      output$receiver_table <- renderDataTable(df)
      
    }
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
    rivers <- st_read("./data/GIS/sac_river_dissolve.shp")
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
    
    # Create a grid of all receiver general locations by all dates from the 
    # earliest detection date to the latest detection date
    timestep <- expand.grid(receiver_loc$GEN, seq(min(detections$date), 
                                                  max(detections$date), by = 1),
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
  # Takes data that has been pre-processed, and visualizes magnitude of unique 
  # detections by day at every receiver location, achieved by using 
  # addMinicharts which takes a time argument
  output$timemap <- renderLeaflet({
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
    
  })
  
  # Finds the optimal bin size using Freedman-Diaconis rule
  # https://stats.stackexchange.com/questions/798/calculating-optimal-number-of-bins-in-a-histogram
  # Sets that optimal bin width to the default setting in the sliderInput
  output$bin_slider <- renderUI({
    tagged_fish <- taggedfishVar()
    
    bins <- nclass.FD(unlist(tagged_fish[,input$variable]))
    sliderInput("bin_width", "Bin width", min = 1, max = 50, value = bins)
  })
  
  # Output a summary table - using package arsenal
  output$summarytable <- renderTable({
    
    mylabels <- list(Length = "Length (mm)", Weight ="Weight (g)")
    as.data.frame(summary(tableby(StudyID ~ Length + Weight, 
                                  data = taggedfishVar(),
                                  control = tableby.control(digits = 2)), 
                          labelTranslations = mylabels, text = "html"))
    
  }, sanitize.text.function = function(x) x)
  
  # Create plotly plot
  output$plotly_plot <- renderPlotly({
    tagged_fish <- taggedfishVar()
    
    # Based on the user selected plot type, use different ggplot options 
    plot_type <- switch(input$plot_type,
                        "boxplot" 	= 	geom_boxplot(),
                        "histogram" =	geom_histogram(bins = input$bin_width, 
                                                     alpha=0.5,
                                                     position="identity"),
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
      mutate(
        time_pst = with_tz(time, "US/Pacific"),
        hour = hour(time_pst)
      )
    
  })

  output$time_of_day_plot <- renderPlotly({
    # Retrieve the file name in the detections folder that corresponds to the 
    # StudyID selected by the user, Look at all files in the detections folder, 
    # choose the one that contains the StudyID string in its name

    detections <- timeofdayVar()
    
    # Get first detection time of each fish at each GEN
    filtered <- detections  %>% 
      group_by(FishID, GEN) %>% 
      summarise(first_detection = min(hour))

    # If user selected to examine time of arrivals by GEN filter the detection 
    # file by the GEN they chose
    if (input$time_of_day_radio == "By General Location") {
      filtered <- filtered %>% 
        filter(GEN == input$time_of_day_GEN)
    }
    
    # Bins data by hour explicitly from 0:23 hours
    # Necessary to do it this way instead of dplyr group_by b/c not all hours 
    # may be represented

    hour_freq <- data.frame("hour" = 0:23) %>% 
      left_join(filtered %>% 
                  group_by(first_detection) %>% 
                  count(), by = c("hour" = "first_detection")) %>% 
      # Replace NA with 0
      rename(freq = n) %>% 
      mutate(freq = ifelse(is.na(freq), 0, freq),
             percent_pass = ((freq / sum(freq)) * 100)
             )
    
    # Find Sunset, Sunrise for earliest and latest date using mean lat/lon
    receivers <- detections %>% 
      select(GEN, LAT, LON) %>% 
      distinct()
    
    avg_lat = mean(receivers$LAT)
    avg_lon = mean(receivers$LON)
    
    # Get earliest date and latest dates to compare differences in these times
    earliest_date <- as.Date(min(detections$time))
    latest_date <- as.Date(max(detections$time))
    
    earliest_sr_ss <- getSunlightTimes(earliest_date, 
                                       avg_lat, 
                                       avg_lon, 
                                       keep = c("sunrise", "sunset"), 
                                       tz = "America/Los_Angeles")
    latest_sr_ss <- getSunlightTimes(latest_date, 
                                     avg_lat, 
                                     avg_lon, 
                                     keep = c("sunrise", "sunset"), 
                                     tz = "America/Los_Angeles")
    
    earliest_sunrise <- hour(earliest_sr_ss$sunrise) + 
      (minute(earliest_sr_ss$sunrise)/60) + 
      (second(earliest_sr_ss$sunrise)/3600)
    
    earliest_sunset <- hour(earliest_sr_ss$sunset) + 
      (minute(earliest_sr_ss$sunset)/60) + (second(earliest_sr_ss$sunset)/3600)
    
    latest_sunrise <- hour(latest_sr_ss$sunrise) + 
      (minute(latest_sr_ss$sunrise)/60) + (second(latest_sr_ss$sunrise)/3600)
    
    latest_sunset <- hour(latest_sr_ss$sunset) + 
      (minute(latest_sr_ss$sunset)/60) + (second(latest_sr_ss$sunset)/3600)
    
    # # Convert hour to factor and give it specific levels, so that it will appear 
    # # as 12:23, 0:11 on the x-axis
    # hour_freq$hour <- factor(hour_freq$hour, levels = c(12:23, 0:11))
    
    hour_freq$hour <- factor(hour_freq$hour, levels = c(0:23))
    
    # Create bar plot to show proportion of detections by hour
    p <- ggplot(data = hour_freq, mapping = aes(x = hour, y = percent_pass)) +
      # # Add rectangle representing "nighttime", using the earliest sunrise and earliest sunset values, had to use 
      # # ymax = 999999 because plotly won't take Inf
      # geom_rect(data=hour_freq, aes(NULL,NULL,xmin=earliest_sunrise,xmax=earliest_sunset),
      #           ymin=0,ymax=999999, size=0.5, alpha=0.2) +
      
      # Have to add +1 to geom_rect and geom_vline because x-axis set as factor
      # and is offset by 1 for some reason
      geom_rect(aes(NULL,NULL,xmin=0,xmax=min(c(earliest_sunrise, latest_sunrise)) + 1),
                ymin=0,ymax=999999, size=0.5, alpha=0.2) +
      geom_rect(aes(NULL,NULL,xmin=max(c(earliest_sunset, latest_sunset)) + 1,xmax=25),
                ymin=0,ymax=999999, size=0.5, alpha=0.2) +
      geom_col() +
      # Add lines to represent the latest sunrise and latest sunset values
      geom_vline(xintercept = max(c(earliest_sunrise, latest_sunrise)) + 1, linetype = "dashed", size = 1.25) +
      geom_vline(xintercept = min(c(earliest_sunset, latest_sunset)) + 1, linetype = "dashed", size = 1.25) +
      ylab("% Fish Passed") +
      xlab("Time of day (h)") + 
      scale_y_continuous(expand = c(0, 0)) +
      theme_classic() +
      theme(
        text = element_text(size = 15),
        # Make caption text small and left justify
        plot.caption = element_text(size = 10, hjust = 0),
        plot.margin = margin(l = 20, b = 20)
      )
    
    if (input$time_of_day_radio == "By General Location") {
      # Get the GenRKM value for this GEN being examined, for use in labeling in plot
      genrkm <- detections %>% 
        filter(GEN == input$time_of_day_GEN) %>% 
        select(GenRKM) %>% 
        distinct() %>% 
        unlist()
      
      p <- (p + labs(
        title = paste0("% Fish arrivals at ", input$time_of_day_GEN),
        subtitle = paste0("GenRKM = (", genrkm, ")")))
    } else {
      p <- (p + labs(
        title = "% Fish arrivals"))
    }
    
    ggplotly(p)
  })
  
  # Output the plot caption using renderUI instead of in ggplot because it 
  # will autowidth the same length as the plot
  # Couldn't figure out how to autowidth in ggplot
  output$time_of_day_caption <- renderUI({
   
    detections <- timeofdayVar() %>% 
      mutate(time = ymd_hms(time))
    # Get earliest date and latest dates to compare differences in these times
    earliest_date <- as.Date(min(detections$time))
    latest_date <- as.Date(max(detections$time))
    
    paste0("The histogram displays the frequency of fish detections as a function of hour of day. The gray box represents \nhours of nighttime between the earliest sunrise and latest sunset. The dotted lines represent \nhours of nighttime between the latest sunrise and earliest sunset. The first date of detection was at: ", earliest_date, " and the last date of detection was at: ", latest_date, ".")
  })
  

# Survival ----------------------------------------------------------------

  # Synchronize studyID selection between tabs
  # https://stackoverflow.com/questions/44516768/r-shiny-synchronize-filters-on-multiple-tabs
  studyIDSelect <- reactiveVal("")  
  
  output$cumSurvSelect <- renderUI({
    selectInput(inputId = "id1", 
                label = "Select", 
                choices = survivalStudyIDs, 
                selected = studyIDSelect())
  })
  
  output$reachSurvSelect <- renderUI({
    selectInput(inputId = "id2", 
                label = "Select", 
                choices = survivalStudyIDs, 
                selected = studyIDSelect())
  })
  
  observeEvent(input$id2,{
    studyIDSelect(input$id2)
  })
  
  observeEvent(input$id1,{
    studyIDSelect(input$id1)
  })
  
  # Reactive expression for cumulative survival input
  cumsurvivalVar<-  reactive({
    req(studyIDSelect())
    
    name <- studyIDSelect()
    
    file <- list.files("./data/Survival/Cumulative Survival", name, full.names = T)
    df <- read_csv(file)
    
    df %>% 
      mutate_at(c("cum.phi", "cum.phi.se", "LCI", "UCI"), round, digits = 3) %>% 
      mutate(
        RKM = round(RKM), digits = 2,
        id = seq.int(nrow(df))
      ) %>% 
      dplyr::rename(
        Survival = cum.phi,
        SE = cum.phi.se,
        Count = count

      )
  })
  
  output$plotly_survival_output <- renderPlotly({
    
    df <- cumsurvivalVar()
    
    df %>% 
    plot_ly(x = ~RKM, y = ~Survival, type = 'scatter', mode = 'lines+markers',
            hoverinfo = 'text',
            text = ~paste('</br> Survival: ', Survival,
                          '</br> LCI: ', LCI,
                          '</br> UCI: ', UCI,
                          '</br> GEN: ', GEN,
                          '</br> GenRKM: ', RKM,
                          '</br> Raw number of fish to site: ', Count
                          ),
            # https://rpubs.com/chelsea/68601
            error_y = ~list(
              type = "data", 
              symmetric = FALSE, 
              arrayminus = Survival - LCI,
              array = UCI - Survival)) %>% 
      layout(title = paste0("Cumulative Survival for ", studyIDSelect())) %>%
      layout(xaxis = list(autorange = "reversed",
                          showline = FALSE,
                          zeroline = FALSE)) %>% 
      layout(yaxis = list(showline = FALSE,
                          zeroline = FALSE))

  })
  
  
  output$survival_map2 <- renderLeaflet({
    
    df <- cumsurvivalVar()
    
    # release <- TaggedFish %>% 
    #   filter(study_id == input$cumsurvival_datasets) %>% 
    #   select(release_location, release_latitude, release_longitude, 
    #          release_river_km) %>% 
    #   group_by(release_location, release_latitude, release_longitude, 
    #            release_river_km) %>% 
    #   summarise(count = n())

    df %>% 
      # filter(reach_num != 0) %>% 
    leaflet() %>% 
      addProviderTiles(
        providers$Stamen.Terrain, group = "Stamen Terrain",
        options = providerTileOptions(noWrap = TRUE)) %>% 
      addProviderTiles(
        providers$Stamen.TonerLite, group = "Stamen Toner Lite",
        options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(
        providers$Esri.NatGeoWorldMap, group = "Esri Nat Geo",
        options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(
        ~GenLon, ~GenLat, 
        layerId = as.character(df$id),
        popup = paste0( "<b>Receiver Location: </b>", 
                        df$GEN,
                        "</br>",
                        "<b>RKM: </b>", 
                        df$RKM,
                        "</br>",
                        "<b>Survival: </b>", 
                        df$Survival,
                        "</br>",
                        "<b># Fish: </b>",
                        df$Count
        ),
        label = ~GEN
      ) %>% 
      # addMarkers(
      #   data = release,
      #   lng = ~release_longitude, 
      #   lat = ~release_latitude, 
      #   popup = paste0( "<b>Release Location: </b>"
      #                   , release$release_location
      #                   , "<br>"
      #                   , "<b># Fish Tagged: </b>"
      #                   , release$Count
      #                   
      #   ),
      #   label = ~release_location,
      #   icon = makeIcon(
      #     iconUrl = "starred.png",
      #     iconWidth = 25,
      #     iconHeight = 25
      #   )
      # ) %>% 
      addLayersControl(
        baseGroups = c("Stamen Terrain", "Stamen Toner Lite", "Esri Nat Geo"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  output$cumSurvDT <- renderDataTable({
    dat <- cumsurvivalVar() %>% 
      select(GEN, RKM, Region, Survival, LCI, UCI, Count, id)
    
    # Put DL button on hold till I can figure it out properly
    datatable(dat, selection = "single", #extensions = 'Buttons',
              options=list(stateSave = TRUE,
                           dom = 'Bfrtip',
                           # buttons =
                           #   list('copy', 'print', list(
                           #     extend = 'collection',
                           #     buttons = list(
                           #       list(extend = 'csv',
                           #            filename = paste0(studyIDSelect(), "_cumulative_survival"),
                           #            header = TRUE),
                           #       list(extend = 'excel', filename = paste0(studyIDSelect(), "_cumulative_survival"),
                           #            title = paste0(studyIDSelect(), "_cumulative_survival"),
                           #            header = TRUE),
                           #       list(extend = 'pdf', filename = paste0(studyIDSelect(), "_cumulative_survival"),
                           #            title = paste0(studyIDSelect(), "_cumulative_survival"),
                           #            header = TRUE)),
                           #     text = 'Download'
                           #   )),
                           rownames = FALSE))
  })
  
  # to keep track of previously selected row
  prev_row <- reactiveVal()
  
  # new icon style for selected rows
  my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')
  
  # new icon style for release site
  my_icon2 = makeAwesomeIcon(icon = 'star', markerColor = 'purple', iconColor = 'white')
  
  
  # https://stackoverflow.com/questions/48781380/shiny-how-to-highlight-an-object-on-a-leaflet-map-when-selecting-a-record-in-a
  observeEvent(input$cumSurvDT_rows_selected, {
    row_selected <-  cumsurvivalVar()[input$cumSurvDT_rows_selected,]
    proxy <- leafletProxy('survival_map2')
    proxy %>%
      addAwesomeMarkers(
        popup = paste0( "<b>Receiver Location: </b>", 
                        row_selected$GEN,
                        "</br>",
                        "<b>RKM: </b>", 
                        row_selected$RKM,
                        "</br>",
                        "<b>Survival: </b>", 
                        row_selected$Survival,
                        "</br>",
                        "<b># Fish: </b>",
                        row_selected$Count
        ),
        label = row_selected$GEN,
        layerId = as.character(row_selected$id),
        lng=row_selected$GenLon,
        lat=row_selected$GenLat,
        icon = my_icon)
    
    # Reset previously selected marker
    if(!is.null(prev_row()))
    {
      proxy %>%
        addMarkers(popup=as.character(prev_row()$GEN), 
                   layerId = as.character(prev_row()$id),
                   lng=prev_row()$GenLon, 
                   lat=prev_row()$GenLat)
    }
    # set new value to reactiveVal 
    prev_row(row_selected)
  })
  
  observeEvent(input$survival_map2_marker_click, {
    clickId <- input$survival_map2_marker_click$id
    print(clickId)
    dataTableProxy("cumSurvDT") %>%
      selectRows(which(cumsurvivalVar()$id == clickId)) %>%
      selectPage(which(input$cumSurvDT_rows_all == clickId) %/% input$cumSurvDT_state$length + 1)
  })
  
  # Reset markers when switching from table view to plot view
  observeEvent(input$cumsurvival_radio, {
    # Reset previously selected marker
    if(!is.null(prev_row()) & input$cumsurvival_radio == "Plot")
    {
      proxy <- leafletProxy('survival_map2')
      proxy %>%
        addMarkers(popup=as.character(prev_row()$GEN), 
                   layerId = as.character(prev_row()$id),
                   lng=prev_row()$GenLon, 
                   lat=prev_row()$GenLat)
    }
  })
  
  # Reactive expression for reach survival input
  reachSurvVar<-  reactive({
    req(studyIDSelect())
    
    name <- studyIDSelect()
    
    file <- list.files("./data/Survival/Reach Survival Per 10km", name, full.names = T)
    df <- read_csv(file)
    
    df %>% 
      mutate_at(c("estimate", "se", "lcl", "ucl"), round, digits = 3) %>% 
      mutate_at(c("rkm_start", "rkm_end"), round, digits = 2) %>% 
      mutate(id = seq.int(nrow(df))) %>% 
      rename(
        Survival = estimate,
        SE = se,
        LCI = lcl,
        UCI = ucl
      ) %>% 
      filter(reach_end != "GoldenGateW")
  })
  
  output$reachSurvMap <- renderLeaflet({
    
    df <- reachSurvVar()
    
    # release <- TaggedFish %>% 
    #   filter(study_id == input$reachSurvInput) %>% 
    #   select(release_location, release_latitude, release_longitude, 
    #          release_river_km) %>% 
    #   group_by(release_location, release_latitude, release_longitude, 
    #            release_river_km) %>% 
    #   summarise(count = n())
    
    df %>% 
      # filter(reach_num != 0) %>% 
      leaflet() %>% 
      addProviderTiles(
        providers$Stamen.Terrain, group = "Stamen Terrain",
        options = providerTileOptions(noWrap = TRUE)) %>% 
      addProviderTiles(
        providers$Stamen.TonerLite, group = "Stamen Toner Lite",
        options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(
        providers$Esri.NatGeoWorldMap, group = "Esri Nat Geo",
        options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(
        ~GenLon_end, ~GenLat_end, 
        layerId = df$id,
        popup = paste0( "<b>Reach: </b>", 
                        df$reach_start, " to ", df$reach_end,
                        "</br>",
                        "<b>RKM: </b>", 
                        df$rkm_start, " to ", df$rkm_end,
                        "</br>",
                        "<b>Survival: </b>", 
                        df$Survival,
                        "</br>",
                        "<b># Fish at reach start: </b>",
                        df$count_at_start,
                        "</br>",
                        "<b># Fish at reach end: </b>",
                        df$count_at_end
        ),
        label = ~Reach
      ) %>% 
      # Add the release site as a unique marker
      addAwesomeMarkers(
        data = df %>% filter(id == 1),
        lng = ~GenLon_start,
        lat = ~GenLat_start,
        popup = paste0( "<b>Release Location: </b>"
                        , df$reach_start[1]
                        , "<br>"
                        , "<b># Fish Tagged: </b>"
                        , TaggedFish %>% 
                          filter(study_id == df$StudyID[1]) %>% 
                          count() %>% 
                          unlist()
        ),
        label = ~reach_start,
        icon = my_icon2
      ) %>%
      addLayersControl(
        baseGroups = c("Stamen Terrain", "Stamen Toner Lite", "Esri Nat Geo"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  output$reachSurvPlotly <- renderPlotly({
    
    name <- studyIDSelect()
    df <- reachSurvVar()
    
    df %>% 
      mutate(reach_end = factor(reach_end, levels = reach_end)) %>% 
      plot_ly(x = ~reach_end, y = ~Survival, type = 'scatter', mode = 'markers',
              hoverinfo = 'text',
              text = ~paste('</br> Survival: ', Survival,
                            '</br> LCI: ', LCI,
                            '</br> UCI: ', UCI,
                            '</br> Reach: ', paste0(reach_start, " to ", reach_end),
                            '</br> RKM: ', paste0(rkm_start, " to " , rkm_end),
                            '</br> Count Start: ', count_at_start,
                            '</br> Count End: ', count_at_end
              ),
              error_y = ~list(
                type = 'data',
                symmetric = FALSE,
                array = UCI-Survival,
                arrayminus = Survival-LCI)
      ) %>% 
      layout(title = paste0("Reach Survival for ", name)) %>% 
      layout(xaxis = list(showline = FALSE,
                          zeroline = FALSE,
                          title = "Receiver Location")) %>% 
      layout(yaxis = list(showline = FALSE,
                          zeroline = FALSE,
                          title = "Survival per 10km"))
    
  })
  
  output$reachSurvDT <- renderDataTable({
    dat <- reachSurvVar() %>% 
      select('Reach Start' = reach_start, 'Reach End' = reach_end, 
             'RKM Start' = rkm_start, 'RKM End' = rkm_end, Region, 
             Survival, LCI, UCI, 'Count Start' = count_at_start,
             'Count End' = count_at_end, id)
    
    # Put DL button on hold till I can figure it out properly
    datatable(dat, selection = "single", #extensions = 'Buttons',
              options=list(stateSave = TRUE,
                           dom = 'Bfrtip',
                           # buttons =
                           #   list('copy', 'print', list(
                           #     extend = 'collection',
                           #     buttons = list(
                           #       list(extend = 'csv', filename = studyIDSelect()),
                           #       list(extend = 'excel', filename = studyIDSelect()),
                           #       list(extend = 'pdf', filename = studyIDSelect())),
                           #     text = 'Download'
                           #   )),
              rownames = FALSE))
  })
  
  # to keep track of previously selected row
  prev_row2 <- reactiveVal()
  
  observeEvent(input$reachSurvDT_rows_selected, {
    row_selected <-  reachSurvVar()[input$reachSurvDT_rows_selected,]
    proxy <- leafletProxy('reachSurvMap')
    print(row_selected)
    proxy %>%
      addAwesomeMarkers(
        popup = paste0( "<b>Reach: </b>", 
                        row_selected$reach_start, " to ", row_selected$reach_end,
                        "</br>",
                        "<b>RKM: </b>", 
                        row_selected$rkm_start, " to ", row_selected$rkm_end,
                        "</br>",
                        "<b>Survival: </b>", 
                        row_selected$Survival,
                        "</br>",
                        "<b># Fish at reach start: </b>",
                        row_selected$count_at_start,
                        "</br>",
                        "<b># Fish at reach end: </b>",
                        row_selected$count_at_end
        ),
        label = row_selected$Reach,
        layerId = as.character(row_selected$id),
        lng=row_selected$GenLon_end,
        lat=row_selected$GenLat_end,
        icon = my_icon)
    
    # Reset previously selected marker
    if(!is.null(prev_row2()))
    {
      print('prevrow2')
      print(prev_row2)
      proxy %>%
        addMarkers(popup = paste0( "<b>Reach: </b>", 
                                   prev_row2()$reach_start, " to ", prev_row2()$reach_end,
                                   "</br>",
                                   "<b>RKM: </b>", 
                                   prev_row2()$rkm_start, " to ", prev_row2()$rkm_end,
                                   "</br>",
                                   "<b>Survival: </b>", 
                                   prev_row2()$Survival,
                                   "</br>",
                                   "<b># Fish at reach start: </b>",
                                   prev_row2()$count_at_start,
                                   "</br>",
                                   "<b># Fish at reach end: </b>",
                                   prev_row2()$count_at_end
        ), 
        layerId = as.character(prev_row2()$id),
        lng=prev_row2()$GenLon_end, 
        lat=prev_row2()$GenLat_end)
    }
    # set new value to reactiveVal 
    prev_row2(row_selected)
  })
  
  observeEvent(input$reachSurvMap_marker_click, {
    clickId <- input$reachSurvMap_marker_click$id
    print(input$reachSurvMap_marker_click)
    dataTableProxy("reachSurvDT") %>%
      selectRows(which(reachSurvVar()$id == clickId)) %>%
      selectPage(which(input$reachSurvDT_rows_all == clickId) %/% input$reachSurvDT_state$length + 1)
  })
  
  # Reset markers when switching from table view to plot view
  observeEvent(input$reachSurvRadio, {
    # Reset previously selected marker
    if(!is.null(prev_row2()) & input$reachSurvRadio == "Plot")
    {
      proxy <- leafletProxy('reachSurvMap')
      proxy %>%
        addMarkers(popup = paste0( 
          "<b>Survival: </b>", 
          prev_row2()$Survival,
          "<b>LCI: </b>", 
          prev_row2()$LCI,
          "<b>UCI: </b>", 
          prev_row2()$UCI,
          "</br>",
          "<b>Reach: </b>", 
          paste0(prev_row2()$reach_start, ' to ', prev_row2()$reach_end),
          "</br>",
          "<b>RKM: </b>", 
          paste0(prev_row2()$rkm_start, ' to ', prev_row2()$rkm_end),
          "</br>",
          "<b># Count Start: </b>",
          prev_row2()$count_at_start,
          "</br>",
          "<b># Count End: </b>",
          prev_row2()$count_at_end), 
          layerId = as.character(prev_row2()$id),
          lng=prev_row2()$GenLon, 
          lat=prev_row2()$GenLat)
    }
  })
  

# Movement ----------------------------------------------------------------

  # Takes user selected studyIDs and retrieves data from ERDDAP
  movementVar <-  reactive({
    req(input$movement_dataset)
    
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
