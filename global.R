###############################################################################
# Entrypoint for the shiny app
#
# Author: Andrew Smith
# Created 2020-10-02
###############################################################################

# Dependencies ------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(googlesheets4)
library(DT)
library(ECharts2Shiny)
library(echarts4r)
library(quantmod)
library(rsconnect)

setwd("C:/Users/asmi797/OneDrive/Documents/Leisure/data-projects/smda")

#  Clean Scripts ----------------------------------------------------------

source("src/helper-functions.R")

gs4_deauth()

# picks = getPicksData()
# data = runPriceFetcher()
