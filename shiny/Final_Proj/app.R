#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Final Project"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Admission", tabName = "admission", icon = icon("dashboard")),
      menuItem("Affordability", tabName = "affordability", icon = icon("dashboard")),
      menuItem("Education Quality", tabName = "quality", icon = icon("dashboard")),
      menuItem("Diversity", tabName = "diversity", icon = icon("dashboard")),
      menuItem("Earnings", tabName = "earnings", icon = icon("dashboard")),
      menuItem("Completion", tabName = "completion", icon = icon("dashboard"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "admission", 
        fluidPage(
          
          # # Application title
          # titlePanel("Old Faithful Geyser Data"),
          
          # Sidebar with a slider input for number of bins
          sidebarLayout(
            sidebarPanel(
              sliderInput("bins",
                          "Number of bins:",
                          min = 1,
                          max = 50,
                          value = 30)
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
              plotOutput("distPlot")
            )
          )
        )
      ),
      tabItem(
        tabName = "affordability", h2("affordability")
      ),
      tabItem(
        tabName = "quality", h2("quality")
      ),
      tabItem(
        tabName = "diversity", h2("diversity")
      ),
      tabItem(
        tabName = "earnings", h2("earnings")
      ),
      tabItem(
        tabName = "completions", h2("completions")
      )
    )
  )
)

# ui <- 

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

