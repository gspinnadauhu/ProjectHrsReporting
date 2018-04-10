library(shiny)
library(plotly)
library(tidyverse)
weekdates<-readRDS("./data/project_hrs_weekly_long.rds")[,1]
# Define UI for application visualizing weekly est vs actual hrs
shinyUI(
  fluidPage(
    # Application title
    titlePanel("Project Hours: Planned vs Actual"),
    sidebarLayout(
      #Side pannel to select week dates to include
      sidebarPanel(
        sliderInput("DateInput",
                    "Period End Date:",
                    min = min(weekdates),
                    max = max(weekdates),
                    value = min(weekdates),
                    timeFormat = "%F",
                    step=7
                    )#,
        #submitButton("Apply Selection")
        ),
    # Show a radar plot of cumulative estimated vs actual hrs
    mainPanel(
       plotlyOutput("radarplot")
    )
  )
))
