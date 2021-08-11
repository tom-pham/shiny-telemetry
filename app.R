##############################################################################
# Author: Tom Pham
#
# Purpose: Shiny app for UCSC/NOAA Acoustic Telemetry Project
# This app visualizes data for the acoustic project in several ways
# -Background project data
# -Receiver Deployment map/table
# -Sacramento River hydrology graph at several CDEC stations
# -Outmigration animation of downstream movement of fish
# -Time of day in which fish are detected
# -Survival (reach per 10km, cumulative) plots/tables
# -Movement: travel time from rel to GEN

library(shiny)

source('global.R')
source('ui.R', local = TRUE)
source('server.R')

shinyApp(ui = ui, server = server)
