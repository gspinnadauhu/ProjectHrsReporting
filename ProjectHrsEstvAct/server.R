library(shiny)
library(plotly)
library(tidyverse)
#data set
weekly<-readRDS("./data/project_hrs_weekly_long.rds")
#find max hrs for radial axis
#MaxHrs<-weekly_long %>%
#  group_by(Dept)%>%
#  summarize(S_Hrs=sum(Hrs),
#            S_Est=sum(d_Est))
#MaxHrs<-with(MaxHrs,max(S_Hrs,S_Est))
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
      max(TotalHrs,TotalEst)
    })
    output$radarplot<-renderPlotly({
      plot_ly(
        type='scatterpolar',
        fill='toself',
        mode='none'
        ) %>%
      add_trace(
        r=TotalEst(),
        theta=Deptmt(),
        name="Estimated Hours",
        opacity = 0.5,
        fillcolor = '#1b9e77'
        ) %>%
      add_trace(
        r=TotalHrs(),
        theta=Deptmt(),
        name="Actual Hours",
        opacity=0.5,
        fillcolor = '#d95f02'
        ) %>%
      layout(
        polar=list(
          radialaxis=list(
            visible=TRUE,
            range=c(0,MaxHrs)
            )
          )
        )
      })
  }
)
