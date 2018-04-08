library(shiny)
library(plotly)
library(tidyverse)
#data set
weekly<-readRDS("./data/project_hrs_weekly_long.rds")
# Define server logic required to draw radar plot from slider input
shinyServer(
  #select daterange to display based on radiobutton input
  function(input, output) {
    selection<-reactive({
      weekly %>%
        filter(Date<=input$DateInput) %>%
        group_by(Dept)%>%
        summarize(TotalHrs=sum(Hrs),
                  TotalEst=sum(d_Est))
    })
    TotalHrs<-reactive({
      selection$TotalHrs
    })
    TotalEst<-reactive({
      selection$TotalEst
    })
    Dept<-reactive({
      selection$Dept
    })
    MaxHrs<-reactive({
      max(TotalHrs,TotalEst)+10
    })
    output$radarplot<-renderPlotly({
      plot_ly(
        type='scatterpolar',
        fill='toself',
        mode='lines'
        ) %>%
      add_trace(
        r=TotalEst,
        theta=Dept,
        name="Estimated Hours"
        ) %>%
      add_trace(
        r=TotalHrs,
        theta=Dept,
        name="Actual Hours"
        ) %>%
      layout(
        polar=list(
          radialaxis=list(
            visible=FALSE,
            range=c(0,MaxHrs)
          )
        )
      )
    })
  }
)
