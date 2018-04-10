library(shiny)
library(plotly)
library(tidyverse)
#data set
weekly<-readRDS("./data/project_hrs_weekly_long.rds")
# Define server logic required to draw radar plot from slider input
shinyServer(
  #select up-to week based on slider input
  function(input, output) {
    selection<-reactive({
      weekly %>%
        filter(Date<=input$DateInput) %>%
        group_by(Dept)%>%
        summarize(Total_Hrs=sum(Hrs),
                  Total_Est=sum(d_Est))
    })
    TotalHrs<-reactive({
      selection()$Total_Hrs
    })
    TotalEst<-reactive({
      selection()$Total_Est
    })
    Deptmt<-reactive({
      selection()$Dept
    })
    MaxHrs<-reactive({
      max(TotalHrs(),TotalEst())+10
    })
    output$radarplot<-renderPlotly({
      plot_ly(
        type='scatterpolar',
        fill='toself',
        mode='lines'
        ) %>%
      add_trace(
        r=TotalEst(),
        theta=Deptmt(),
        name="Estimated Hours"
        ) %>%
      add_trace(
        r=TotalHrs(),
        theta=Deptmt(),
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
