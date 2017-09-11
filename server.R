library(shiny)
library(plotly)
library(tidyverse)
library(magrittr)
library(lubridate)

result <- readRDS("data_shiny.rds") 

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

colours_util <- gg_color_hue(4)

count_state_changes <- function(nhrs, day, data) {
  begin <- ymd_hms(paste(day, "00:00:00"), 
                   tz = "America/Regina") + dhours(nhrs)
  end <- begin + dhours(1)
  
  output <- data %>%
    transmute(registered = regtime < end & regtime >= begin,
              assessed = physiciantime < end & physiciantime >= begin,
              admitted = bedtime < end & bedtime >= begin,
              discharged = dischargetime < end & dischargetime >= begin) %>%
    summarise(nReg = sum(registered, na.rm = TRUE),
              nAssessed = sum(assessed, na.rm = TRUE),
              nAdmit = sum(admitted, na.rm = TRUE),
              nDisch = sum(discharged, na.rm = TRUE)) %>%
    mutate(Time = begin)
}

hour_data <- data.frame(TimeCensus = paste(0:23, ":00:00", sep=""))

shinyServer(function(input, output) {
  hour_data_for_specific_day <- reactive({hour_data %>%
    mutate(TimeCensus = ymd_hms(paste(input$date,
                                      TimeCensus),
                                tz = "America/Regina")) %>%
    mutate(foo = 1)})
  
  result_per_day <- reactive({
    begin <- ymd_hms(paste(input$date + ddays(0), "00:00:00"), 
                     tz = "America/Regina")
    end <- ymd_hms(paste(input$date + ddays(1), "00:00:00"), 
                   tz = "America/Regina")
    
    result %>%
      filter(regtime < end, dischargetime >= begin)
  })
  
  subresult <- reactive({
    result_per_day() %>%
      filter(Site == input$hospital)
  })
  
  hourly_change_states <- reactive({
    map_df(0:23, count_state_changes, 
                                 data = subresult(), day = input$date)
  })
  
  # output$censusPlot <- renderPlot({
  #   
  #   hourly_change_states() %>%
  #     mutate(Status = factor(Status, levels = c("nReg", "nAssessed",
  #                                               "nAdmit", "nDisch"))) %>%
  #     arrange(Status) %>% 
  #     ggplot(aes(x = Time, y = Count, fill = Status)) +
  #     geom_area() +
  #     scale_fill_discrete(labels = c("Registered", 
  #                                    "Assessed",
  #                                    "Admitted",
  #                                    "Discharged"),
  #                         breaks = c("nReg", "nAssessed",
  #                                    "nAdmit", "nDisch")) +
  #     ggtitle(paste("Change in status for every hour on", input$date)) +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1),
  #           legend.position = "bottom") + xlab("")
  #   
  # })
  # 
  # output$utilPlot <- renderPlot({
  # 
  #   results_for_specific_day <- subresult() %>%
  #     mutate(foo = 1) %>%
  #     full_join(hour_data_for_specific_day(), by = 'foo') %>%
  #     select(-foo) %>%
  #     mutate(Present = as.numeric(regtime < TimeCensus & 
  #                                   dischargetime >= TimeCensus),
  #            WaitingForBed = as.numeric(bedtime < TimeCensus & 
  #                                         dischargetime >= TimeCensus)) %>%
  #     group_by(TimeCensus) %>%
  #     summarise(Census = sum(Present),
  #               BC4 = sum(WaitingForBed, na.rm = TRUE)) %>%
  #     gather("Type", "Census", Census, BC4)
  # 
  #   if (input$hospital == "SCH") {
  #     results_for_specific_day %<>%
  #       filter(Type == "Census")
  #   }
  #     
  #   results_for_specific_day %>%
  #     mutate(Type = factor(Type, levels = c("Census", "BC4"))) %>%
  #     arrange(Type) %>% 
  #     ggplot(aes(x = TimeCensus, y = Census, colour = Type)) +
  #     geom_line() + geom_point()+
  #     geom_smooth(se = FALSE, method = 'loess') +
  #     ggtitle(paste("Hourly census on", input$date)) +
  #     xlab("") + theme(axis.text.x = element_text(angle = 45, hjust = 1),
  #                      legend.position = "bottom") + 
  #     ylim(c(0, max(results_for_specific_day$Census)))
  # 
  # })
  
  output$utilPlot <- renderPlotly({
    
    p <- hourly_change_states() %>%
      plot_ly(x = ~Time, y = ~nReg+nAssessed+nAdmit+nDisch, 
              name = 'Discharged', text = ~nDisch,
              type = 'scatter', mode = 'none', hoverinfo = "text+name",
              fill = 'tozeroy', fillcolor = colours_util[4]) %>%
      add_trace(y = ~nReg+nAssessed+nAdmit, fillcolor = colours_util[3], 
                name = 'Admitted', text = ~nAdmit) %>%
      add_trace(y = ~nReg+nAssessed, fillcolor = colours_util[2], 
                name = 'Assessed', text = ~nAssessed) %>%
      add_trace(y = ~nReg, fillcolor = colours_util[1], 
                name = 'Registered', text = ~nReg) %>%
      layout(title = paste("Change in status for every hour on", input$date),
             xaxis = list(title = ""),
             yaxis = list(title = "Count"),
             hovermode = "x+y") %>%
      config(displayModeBar = FALSE)
    
    p
  })
  
  output$censusPlot <- renderPlotly({
    
    results_for_specific_day <- subresult() %>%
      mutate(foo = 1) %>%
      full_join(hour_data_for_specific_day(), by = 'foo') %>%
      select(-foo) %>%
      mutate(Present = as.integer(regtime < TimeCensus & 
                                    dischargetime >= TimeCensus),
             WaitingForBed = as.integer(bedtime < TimeCensus & 
                                          dischargetime >= TimeCensus)) %>%
      group_by(TimeCensus) %>%
      summarise(Census = sum(Present),
                BC4 = sum(WaitingForBed, na.rm = TRUE)) %>%
      gather("Type", "Census", Census, BC4)
    
    results_for_specific_day %>%
      mutate(Type = factor(Type, levels = c("Census", "BC4"))) %>%
      arrange(Type) %>%
      plot_ly(x = ~TimeCensus, y = ~Census, color = ~Type, colors = gg_color_hue(2),
              type = 'scatter', mode = 'lines+markers') %>%
      layout(yaxis = list(rangemode = "tozero"),
             title = paste("Hourly census on", input$date),
             xaxis = list(title = ""),
             yaxis = list(title = "Census"),
             hovermode = "x+y") %>%
      config(displayModeBar = FALSE)
  })
  
  
})
