

library(shinydashboard)
library(shiny)
library(tidyverse)
library(lubridate)

source("read_data.R")

header <- dashboardHeader(title = "COVID-19 Dashboard")
sidebar <- dashboardSidebar(
        selectInput(
            inputId = "country",
            label = "Select Country:",
            choices = df %>% select(country) %>% distinct(),
            selected = "Saudi Arabia"
        )
)
body <- dashboardBody(
    textOutput("update", inline = FALSE),
    valueBoxOutput("ncc", width = 4),
    valueBoxOutput("nrc", width = 4),
    valueBoxOutput("ndc", width = 4),
    valueBoxOutput("tcc", width = 4),
    valueBoxOutput("trc", width = 4),
    valueBoxOutput("tdc", width = 4)
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
    df_country <- reactive({
        df %>% filter(country == input$country)
    })
    
    last_values_country <- reactive({
        df_country() %>% filter(date == max(date))
    })
    
    output$update <- renderText({
        paste("Last Update:", stamp("Wednesday October 7, 2020")(ymd(last_values_country()$date)))
    })
    
    output$ncc <- renderValueBox({
        valueBox(
            subtitle = "New Confirmed Cases",
            prettyNum(last_values_country()$confirmed_daily, big.mark = ","),
            color = "aqua"
            #icon = icon("credit-card")
        )
    })
    
    output$nrc <- renderValueBox({
        valueBox(
            subtitle = "New Recovered Cases",
            prettyNum(last_values_country()$recovered_daily, big.mark = ","),
            color = "green"
            
        )
    })
    
    output$ndc <- renderValueBox({
        valueBox(
            subtitle = "New Death Cases",
            prettyNum(last_values_country()$deaths_daily, big.mark = ","),
            color = "red"
        )
    })
    
    output$tcc <- renderValueBox({
        valueBox(
            subtitle = "Total Confirmed Cases",
            prettyNum(last_values_country()$confirmed, big.mark = ","),
            color = "aqua"
            #icon = icon("credit-card")
        )
    })
    
    output$trc <- renderValueBox({
        valueBox(
            subtitle = "Total Recovered Cases",
            prettyNum(last_values_country()$recovered, big.mark = ","),
            color = "green"
            
        )
    })
    
    output$tdc <- renderValueBox({
        valueBox(
            subtitle = "Total Death Cases",
            prettyNum(last_values_country()$deaths, big.mark = ","),
            color = "red"
        )
    })
}

shinyApp(ui, server)