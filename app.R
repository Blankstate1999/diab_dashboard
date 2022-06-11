## app.R ##
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "diabetes dashboard"),

sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Food", tabName = "food", icon = icon("utensils")),
      menuItem("Insulin", tabName = "insulin", icon = icon("syringe")),
      menuItem("GCM", tabName = "cgm", icon = icon("chart-line"))
  )
),

body <- dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      tabItem(tabName = "dashboard",
              h2("Dashboard tab content")
              ),
      
      tabItem(tabName = "food",
              h2("Food tab content")
              ),
      
      tabItem(tabName = "insulin",
              h2("Insulin tab content")
              ),
      
      tabItem(tabName = "cgm",
              h2("Continuous Glucose Monitoring (CGM) tab content")
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