#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinySignals)   # devtools::install_github("hadley/shinySignals")
library(dplyr)
library(shinydashboard)
library(bubbles) 
library(leaflet)

#source('~/Documents/OCESA1/Datos_cargar.R', echo=TRUE)
source('Datos_cargar.R')
# Define UI for application that draws a histogram
dashboardPage(
  dashboardHeader(title = "OCESA- Inmueble"),
  dashboardSidebar(
    selectInput("entretenimiento",
                "Contenido:",
                choices =  StudentData$Var1),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard")
     # menuItem("Raw data", tabName = "rawdata")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
                valueBoxOutput("rate"),
                valueBoxOutput("count"),
                valueBoxOutput("users")
              ),
              fluidRow(
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "ARIMA",
                  plotOutput("distPlot")
                ),
                box(
                  width = 6, status = "info",solidHeader = TRUE,
                  title = "VaR Sensibility",
                  plotOutput("risk")
                )
              ),
              
              fluidRow(
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "MAPA",
                  leafletOutput("mapa")
                ),
                
                box(
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "ARIMA",
                  DT::dataTableOutput("Inmuebles")
                )
                
                
                
              )
      ) #,
     # tabItem("rawdata",
      #        numericInput("maxrows", "Rows to show", 25),
       #       verbatimTextOutput("rawtable"),
        #      downloadButton("downloadCsv", "Download as CSV")
      )
    )
  
)
