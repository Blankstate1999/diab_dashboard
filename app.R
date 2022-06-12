## app.R ##
library(shinydashboard)
library(shiny)
library(dplyr)
library(tidyverse)

# Import data
insulin <- read_csv("insulin.csv")

# Convert to data frame
tdd_df <- as.data.frame(insulin)

# Format Time column, create date column
tdd_df$Time <- as.POSIXct(tdd_df$Time, format = "%d/%m/%Y %H:%M", tz=Sys.timezone())
tdd_df$date = as.Date(tdd_df$Time, format = "%Y-%m-%d")

# Rename tdd variable
tdd_df <- tdd_df %>%
  rename(total_daily_dose = "Total daily dose")

# Filter out rows with null values in the tdd variable  
tdd_date <- filter(tdd_df, (!is.na(tdd_df$total_daily_dose))) 

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
                tabPanel("Insulin", "Insulin data", icon=icon("syringe")),
                tabPanel("Carbs", "Carbs data", icon=icon("utensils")))
      
      )
      
    )
    
  )

)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)