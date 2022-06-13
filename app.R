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
      menuItem("About", tabName="about", icon=icon("question")),
      menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
      menuItem("Data", tabName = "data", icon = icon("line-chart"))
      
  )
),

body <- dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      tabItem(tabName="about",
              h3("Diabetes Dashboard"),
              h5("A web-based dashboard for reviewing all of Skyla's key diabetes statistics at a glance"),
              hr(),
              h5(strong("Created"), "Lee Mercer"),
              h5(strong("GitHub Username"), "BlankState1999"),
              h5(strong("Email"), "leecraigmercer@gmail.com")
              ),
        
      
      tabItem(tabName = "summary",
              h2("Overview"),
              fluidRow(
                # InfoBoxes
                valueBox("82%", color="green", "Time in Range", icon = icon("stopwatch")),
                valueBox("7.7", color="orange", "Average (mmol/L)", icon=icon("dot-circle")),
                valueBox("22.3", color="red", "Highest Value (mmol/L)", icon=icon("arrow-up")),
                valueBox("11.5", "Total Daily Dose", icon = icon("prescription-bottle")),
                valueBox("3.2", "Standard deviation (mmol/L)", icon=icon("arrows-alt-v")),
                valueBox("2.2", color="red", "Lowest Value (mmol/L)", icon=icon("arrow-down"))),
      ),
      
      tabItem(tabName = "data",
              h2("Data view"),
              dateRangeInput("range_date", "Choose date range",
                             start = min(tdd_date$date),
                             end = max(tdd_date$date),
                             min = min(tdd_date$date),
                             max = max(tdd_date$date),
                             format = "dd/mm/yy"),
              tabsetPanel(
                
                id = "tabset",
                tabPanel("CGM", "CGM data", icon=icon("chart-line")),
                tabPanel("Insulin", ".", icon=icon("syringe"),
                         fluidRow(
                                     box(title = "Total Daily Dose", solidHeader=TRUE, status = "primary", collapsible = TRUE, plotlyOutput("tdd")),
                                     box(title = "Basal as a % of TDD", solidHeader=TRUE, status = "primary", collapsible = TRUE, plotlyOutput("basal"))),
                         ),
                tabPanel("Carbs", "Carbs data", icon=icon("utensils")))
      
      )
      
    )
    
  )

)

server <- function(input, output) {
  
  output$tdd <- renderPlotly({
    data1 <- tdd_date[tdd_date$date>=input$range_date[1] & tdd_date$date<=input$range_date[2],]
    ggplot(data1, 
           aes(x = `date`, y = `total_daily_dose`)) +
      geom_point(alpha = .25) +
      geom_smooth(method=lm, col='grey', size=.5) +
      scale_x_date() +
      xlab("Date") + ylab("TDD") +
      ylim(0,16)

    })
    
    output$basal <- renderPlotly({
      data1 <- tdd_date[tdd_date$date>=input$range_date[1] & tdd_date$date<=input$range_date[2],]
      ggplot(data1,
             aes(x = `date`, y = `percent`)) + 
        geom_col(fill="coral1", col = "coral1") +
        geom_smooth(method=lm, col='black', size=.5, se=FALSE) +
        ylim(0,100) +
        xlab("date") + ylab("% basal")
  })
  
}

shinyApp(ui, server)