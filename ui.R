##############################################################################
# Author: Tom Pham
# Purpose: This script defines the UI component of the Shiny telemetry app
# Here the layout, and input selections are defined


ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage("Central Valley Enhanced Acoustic Tagging Project",
                           tabPanel("Background", 
                                    mainPanel(
                                      uiOutput("background")
                                    )
                           ),
                           tabPanel("Receiver Deployments",
                                    sidebarLayout(
                                      sidebarPanel(
                                        radioButtons("receiverType", 
                                                     "Receiver type",
                                                     c("JSATS", 
                                                       "Real-time", "Vemco")
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
                                        leafletOutput("map", width = "100%", 
                                                      height = "650"),
                                        dataTableOutput("receiver_table")
                                      )
                                    )
                           ),
                           tabPanel("Hydrology",
                                    box(dygraphOutput("dygraph", 
                                                      width = "100%")),
                                    box(leafletOutput("hydromap", 
                                                      width = "75%")),
                                    textOutput("text1"),
                                    textOutput("text2"),
                                    textOutput("text3"),
                                    downloadButton("downloadData", "Download")
                                    
                           ),
                           tabPanel("Outmigration Animation",
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("anim_dataset", 
                                                    "Choose a study population",
                                                    choices = other_studyid_descript_name),
                                        helpText("This tool visualizes study group detection data into an animated time series",
                                                 "of fish outmigration. Unique fish detections are identified at each general",
                                                 "receiver location and summed by day. Note that is it possible for a fish to",
                                                 "be detected at more than one general receiver location in a single day.")
                                      ),
                                      mainPanel(
                                        leafletOutput("timemap", width = "100%", 
                                                      height = "650")
                                      )
                                    )
                           ),
                           tabPanel("Data Explorer",
                                    headerPanel("Select Options"),
                                    sidebarPanel(
                                      selectizeInput("data_explorer_datasets", 
                                                     "Study Populations",
                                                     # Adding empty string first 
                                                     # allows no default selection
                                                     choices = c("", other_studyid_descript_name),
                                                     options = list(maxItems = 4)),
                                      selectInput("variable", "Variable",
                                                  choices = c("Weight", "Length")),
                                      selectInput("plot_type", "Plot type", 
                                                  choices = c("boxplot", 
                                                              "histogram", 
                                                              "density")),
                                      # Show this bin width slider only if 
                                      # histogram is chosen
                                      conditionalPanel(
                                        condition = "input.plot_type == 'histogram'",
                                        # Place holder for bin slider input, 
                                        # created in server side
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
                                      selectInput("time_of_day_input", 
                                                  "Choose a study population",
                                                  choices = c("", other_studyid_descript_name),
                                      ),
                                      radioButtons("time_of_day_radio", 
                                                   "Choose all detections or by receiver GEN",
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
                                                 helpText("Note: These survival results are
                                          preliminary and for discussion purposes
                                          only. Detection data has not been
                                          filtered for predator detections, and
                                          survival estimates have not been
                                          adjusted for any potential premature
                                          tag failures."),
                                                 uiOutput("cumsurvival_text"),
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
                                                   dataTableOutput('cumSurvDT'),
                                                   downloadButton("cumSurvDownload")
                                                 )
                                               )
                                      ),
                                      tabPanel("Reach Survival",
                                               headerPanel("Select Options"),
                                               sidebarPanel(
                                                 uiOutput("reachSurvSelect"),
                                                 helpText("Note: These survival results are
                                          preliminary and for discussion purposes
                                          only. Detection data has not been
                                          filtered for predator detections, and
                                          survival estimates have not been
                                          adjusted for any potential premature
                                          tag failures."),
                                                 uiOutput("reachSurv_text"),
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
                                                   dataTableOutput('reachSurvDT'),
                                                   downloadButton("reachSurvDownload")
                                                 )
                                               )
                                      )
                                    )
                                    
                           ),
                           tabPanel("Movement",
                                    tabsetPanel(
                                      tabPanel("Travel Time Table",
                                               headerPanel("Select Options"),
                                               sidebarPanel(
                                                 selectInput(
                                                   "movement_dataset", "Choose a study population",
                                                   choices = other_studyid_descript_name
                                                 ),
                                                 uiOutput(
                                                   "movement_second_select"
                                                 )
                                               ),
                                               mainPanel(
                                                 gt_output(
                                                   "movement_gt"
                                                 )
                                               )
                                      )
                                      # ,
                                      # tabPanel("Movement Plot",
                                      #          headerPanel("Select Options"),
                                      #          sidebarPanel(
                                      #            selectInput(
                                      #              "movement_plot_dataset", "Choose a study population",
                                      #              choices = studyid_list
                                      #            )
                                      #          ),
                                      #          mainPanel(
                                      #            plotlyOutput(
                                      #              "movement_plot"
                                      #            )
                                      #          )
                                      # )
                                    )
                                    
                           )
                )
)

