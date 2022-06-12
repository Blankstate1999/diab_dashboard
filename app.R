## app.R ##
library(shinydashboard)
library(shiny)
library(dplyr)
library(tidyverse)
library(plotly)

# Import data
insulin <- read_csv("insulin.csv")

# Convert to data frame
tdd_df <- as.data.frame(insulin)

# Format Time column, create date column
tdd_df$Time <- as.POSIXct(tdd_df$Time, format = "%d/%m/%Y %H:%M", tz=Sys.timezone())
tdd_df$date = as.Date(tdd_df$Time, format = "%Y-%m-%d")

# Rename tdd variable
tdd_df <- tdd_df %>%
  rename(total_daily_dose = "Total daily dose",
         total_daily_basal = "Total daily basal")

# Filter out rows with null values in the tdd variable  
tdd_date <- filter(tdd_df, (!is.na(tdd_df$total_daily_dose)))

tdd_date$percent <- (tdd_date$total_daily_basal / tdd_date$total_daily_dose)
tdd_date$percent <- (tdd_date$percent*100)
tdd_date$percent <- round(tdd_date$percent, digits = 0)

str(tdd_date)

ui <- dashboardPage(
  dashboardHeader(title = "diab dash"),

sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem(dateRangeInput("range_date", "Date Range",
                              start = min(tdd_date$date),
                              end = max(tdd_date$date),
                              min = min(tdd_date$date),
                              max = max(tdd_date$date),
                              format = "dd/mm/yy")),
      menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
      menuItem("Data", tabName = "data", icon = icon("chart-line"))
      
  )
),

body <- dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      tabItem(tabName = "summary",
              h2("Overview")
              ),
      
      tabItem(tabName = "data",
              h2("Data"),
              tabsetPanel(
                id = "tabset",
                tabPanel("CGM", "CGM data", icon=icon("chart-line")),
                tabPanel("Insulin", "Insulin data", icon=icon("syringe"),
                         plotOutput("tdd"),
                         plotOutput("basal")),
                tabPanel("Carbs", "Carbs data", icon=icon("utensils")))
      
      )
      
    )
    
  )

)

server <- function(input, output) {
  
  output$tdd <- renderPlot({
    data1 <- tdd_date[tdd_date$date>=input$range_date[1] & tdd_date$date<=input$range_date[2],]
    ggplot(data1, 
           aes(x = `date`, y = `total_daily_dose`)) +
      geom_point(alpha = .25) +
      geom_smooth(method=lm, col='black', size=1) +
      scale_x_date() +
      xlab("Date") + ylab("TDD") +
      ylim(0,16) +
      labs(title = "Total Daily Dose (TDD)")}
    
  )
    
    output$basal <- renderPlot({
      data1 <- tdd_date[tdd_date$date>=input$range_date[1] & tdd_date$date<=input$range_date[2],]
      ggplot(data1,
             aes(x = `date`, y = `percent`)) + 
        geom_col(fill="coral1", col = "coral1") +
        geom_smooth(method=lm, col='black', size=.5, se=FALSE) +
        ylim(0,100) +
        xlab("date") + ylab("% basal") +
        labs(title = "Basal as a % of TDD")
  })
  
}

shinyApp(ui, server)