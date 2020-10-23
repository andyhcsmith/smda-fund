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
library(gt)

#  Clean Scripts ----------------------------------------------------------

source("src/helper-functions.R")

picks = getPicksData()
data = runPriceFetcher()
