library(shiny)
library(plotly)

shinyUI(fluidPage(theme = "bootstrap.css",
                  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  
  sidebarLayout(
    sidebarPanel(
      titlePanel("ED hourly utilization"),
       dateInput("date", "Day of interest", value = "2016-03-26",
                 min = "2016-01-01", max = "2016-12-31"),
       radioButtons("hospital", "Hospital",
                    c("A", "B"), "A"),
      hr(style = "border-top: 3px double #8c8b8b"),      
      helpText(tags$h4("Methodology"),
               "The two graphs on the right represent hourly utilization of the",
               "ED. For the purpose of the top graph, we construe the patient journey",
               "as going through possibly four stages:",
               tags$ol(
                 tags$li("Registration,"),
                 tags$li("First Assessment by a physician,"),
                 tags$li("Admission (i.e. when a bed is called for them),"),
                 tags$li("Discharge.")
               ),
               "The top graph therefore counts the number of patients who",
               "went through any of these four stages during the hour."),
      br(),
      helpText("The bottom graph counts the total number of patients in the ED",
               "on the hour (i.e. Census). It also counts the total number of patients",
               "for whom a bed has been called but who are still waiting in the ED (i.e. BC4)."),
      width = 4
    ),
    
    mainPanel(
      fluidRow(
        plotlyOutput("utilPlot"),
        plotlyOutput("censusPlot"),
        width = 7
      )
    )
  )
))
