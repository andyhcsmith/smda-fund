###############################################################################
# Entrypoint for the shiny app
#
# Author: Andrew Smith
# Created 2020-10-02
###############################################################################

# Dependencies ------------------------------------------------------------
library(shiny)
library(tidyverse)
library(janitor)
library(lubridate)
library(shinycssloaders)
library(sp)
library(plotly)
library(shinymaterial)
library(quantmod)
library(ECharts2Shiny)
library(echarts4r)
library(shinyWidgets)
library(rsconnect)
library(googlesheets)

#  Clean Scripts ----------------------------------------------------------

source("src/helper-functions.R")
picks = read_csv('data/dummy-picks.csv',col_types = cols())
