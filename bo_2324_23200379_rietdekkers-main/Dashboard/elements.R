library(shiny)
library(bslib)
library(shinyWidgets)
library(leaflet)
library(bsicons)
library(RPostgres)
library(shinyjs)
library(sf)
library(lubridate)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)

### Database variabelen ###
dbname <- "bo_23200379_rietdekkers"
host <- "dilab-sb.postgres.database.azure.com"
port <- 5432
user <- "bo_23200379_rietdekkers"
password <- "Vr3IsY54W83DNN0n"

#### Custom thema grafieken ####
custom_theme <- function() {
  theme_minimal(base_family = "Roboto Condensed") +
    theme(
      text = element_text(family = "Roboto Condensed"),
      axis.title = element_text(size=10, face="bold"),
      legend.title = element_text(face = "bold")
    )
}

line_colors <- c( "#fdb833", "#ee21c9", "#36ee6d", "#1789fc", "#e60049", "#5fcfff", "#a418f5", "#01e4b1")

