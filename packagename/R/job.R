# Based on https://github.com/Sarah-2510/R-Shiny-Project---AIR-QUALITY-INDEX/blob/main/Rshiny%20final.R
# Last update: 2024-08-28
# attention to tz; # local path to files

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinycssloaders)
library(jsonlite)
library(DT)
library(data.table)
library(leaflet)
library(leaflegend)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(ggplot2)
library(RColorBrewer)
library(data.table)
library(tidyverse)
library(devtools)
library(openair)
library(openairmaps)
library(dplyr)
library(zoo)
library(plotly)
#  --------------------------------------------------------------------------------------------------------
#                                              READING THE FILES
#  --------------------------------------------------------------------------------------------------------

source("./scripts/01-AQI_calculation_thermo_data.R")

