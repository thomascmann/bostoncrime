library(shiny)
library(shinydashboard)
library(ggiraph)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(shinyWidgets)
library(forecast)
library(zoo)
library(ClustGeo)
library(RColorBrewer)
library(cluster)
library(factoextra)
library(mapproj)
library(shinycssloaders)

options(shiny.sanitize.errors = TRUE)

centroids <- read_csv("centroids.csv")
districts <- read_csv("district_shapes.csv")
dat <- read_csv("crimedata_both.csv", col_types = cols(
  .default = col_character(),
  REPORTING_AREA = col_double(),
  OCCURRED_ON_DATE = col_character(),
  YEAR = col_double(),
  MONTH = col_double(),
  HOUR = col_double(),
  Lat = col_double(),
  Long = col_double(),
  n = col_double()
)) %>%
  mutate(
    OCCURRED_ON_DATE = ymd_hms(OCCURRED_ON_DATE)
  ) %>%
  left_join(
    unique(districts[, c("Name", "SqMiles")]), by = "Name"
  )

neighborhoods <- sort(unique(dat$Name))
incidents_group <- sort(unique(dat$OFFENSE_CODE_GROUP))
incidents <- sort(unique(dat$OFFENSE_DESCRIPTION))
incidents_both <- unique(dat[c("OFFENSE_DESCRIPTION","OFFENSE_CODE_GROUP")])

createMapLink <- function(lat,long) {
  sprintf('<a href="https://www.google.com/maps/search/?api=1&query=%s,%s" 
          target="_blank" class="btn btn-primary">Map</a>',
          lat,long)
}

