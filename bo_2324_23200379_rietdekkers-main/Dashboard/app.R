library(shiny)
library(bslib)
library(shinyWidgets)
library(leaflet)
library(bsicons)
library(RPostgres)
library(shinyjs)

# Load UI
source("ui.R")

# Load server function
source("server.R")

# Run the application
shinyApp(ui, server)