library("shiny")
library("shinydashboard")
library("tidyverse")
library("leaflet")
library("plotly")
library("DT")
library("fs")
library("wbstats")

port <- Sys.getenv('PORT')

shiny::runApp(
        appDir = getwd(),
        host = '0.0.0.0',
        port = as.numeric(port)
)