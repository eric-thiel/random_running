### function to put stats online

### add passing attempts and rushing attempts????????????????
### big brain SZN


library("shiny")   
library(DT)
library(googlesheets4)
library(shinycssloaders)
library(dplyr)
library(rdrop2)
library(readr)
library(vroom)
library(tidyverse)
library(shinythemes)
library(lubridate)
library(shinybusy)

header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

generate_run = function(){
  distance = round(runif(1, 4, 12),2)
  mean_distance = 8
  pace = round(runif(1, 7.25, 9),2)
  mean_pace = 8
  
  pace_mod = sqrt(sqrt(sqrt(distance / mean_distance)))
  pace = round(pace * pace_mod,2)
  
  decimal <- pace - floor(pace)
  decimal = round(decimal * 0.6,2)
  non_decimal = floor(pace)
  
  
  
  pace_in_time = decimal + non_decimal
  
  distance = ifelse(runif(1, 0,1)<0.1, "off day", distance)
  pace_in_time = ifelse(distance == "off day", "off day", pace_in_time)
  workout = list(distance, pace_in_time)
  
  return(workout)
}



ui = fluidPage(add_busy_spinner(spin = "self-building-square", position = "full-page"),
                 theme = shinytheme("spacelab"),
                pageWithSidebar(
                  headerPanel("RunLog"),
                  
                  actionButton("button",label = "Push Me Once Per Day"),
                  
                  mainPanel(
                    DT::dataTableOutput("mytable"),
                  )
                  
                    
                )
                  
                )


server = shinyServer(
  function(input,output,session){
    Sys.sleep(5)
    observeEvent(input$button, {
      g = generate_run()
      Distance = g[[1]]
      Pace = g[[2]]
      Date = Sys.Date()
      Date = as.Date(Date)
      Date = round_date(Date, unit = "day")
      bruh = data.frame(Date, Distance, Pace)
      sheet_append("https://docs.google.com/spreadsheets/d/1wvxYJrN2jjSGHe9DrLNaQqZJKeJzX5tA_txFpmkv5H8/edit#gid=0", bruh, sheet = "Log")
      session$reload()
       }) 
    
    output$mytable = DT::renderDataTable({   
      x = read_sheet("https://docs.google.com/spreadsheets/d/1wvxYJrN2jjSGHe9DrLNaQqZJKeJzX5tA_txFpmkv5H8/edit#gid=0",
                 "Log")      
      x$Date = ymd(x$Date)
      datatable(x)
      })
      
    })
  


shinyApp(ui = ui, server = server)















