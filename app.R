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
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Data", tabName = "data", icon = icon("line-chart"))
      
  )
),

body <- dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      tabItem(tabName="about",
              h3("Welcome to Skyla's interactive diabetes dashboard"),
              h5("A web-based dashboard for reviewing all of Skyla's key diabetes statistics at a glance"),
              h5("-"),
              h4("Summary"),
              h5("The summary page is where you will find all of Skyla's headline stats, included her time in range, average blood glucose and total daily dose."),
              h5("-"),
              h4("Data"),
              h5("The", strong("data page"), "is where you will find a more detailed breakdown of Skyla's diabetes statistics. Included on this page are tabs for her continous glucose monitoring (CGM), insulin and carbohydrate data."),
              h5("-"),
              h4("Contact details"),
              h5(strong("Created"), "Lee Mercer"),
              h5(strong("GitHub Username"), "BlankState1999"),
              h5(strong("Email"), "leecraigmercer@gmail.com")
              ),
        
      
      tabItem(tabName = "overview",
              h2("Overview"),
              fluidRow(
                # InfoBoxes
                valueBox("82%", color="green", "Time in Range", icon = icon("stopwatch"), width=2),
                valueBox("11.5", "Total Daily Dose", icon = icon("prescription-bottle"), width=2),
                valueBox("7.7", color="orange", "Average (mmol/L)", icon=icon("bullseye"), width=2),
                valueBox("22.3", color="red", "Highest (mmol/L)", icon=icon("arrow-up"), width=2),
                valueBox("2.2", color="red", "Lowest (mmol/L)", icon=icon("arrow-down"), width=2),
                valueBox("3.2", color="orange", "Std. dev. (mmol/L)", icon=icon("stream"), width=2),),
              
              fluidRow(
                box(title = "Box1", solidHeader=F, status = "primary", collapsible = TRUE, width=4),
                box(title = "Box2", solidHeader=F, status = "warning", collapsible = TRUE, width=4),
                box(title = "Box3", solidHeader=F, status = "primary", collapsible = TRUE, width=4),),
              
              fluidRow(
                box(title = "Box4", solidHeader=F, status = "success", collapsible = TRUE, width=4),
                box(title = "Box5", solidHeader=F, status = "danger", collapsible = TRUE, width=4),
                box(title = "Box6", solidHeader=F, status = "info", collapsible = TRUE, width=4),),
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
                                     box(title = "Total Daily Dose", solidHeader=F, status = "primary", collapsible = TRUE, width=6, plotlyOutput("tdd")),
                                     box(title = "Basal as a % of TDD", solidHeader=F, status = "primary", collapsible = TRUE, width=6, plotlyOutput("basal"))),
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