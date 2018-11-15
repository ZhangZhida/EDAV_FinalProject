#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# To deploy the app, run
#   library(rsconnect)
#   deployApp("/home/zhida/Desktop/STAT5702EDAV/finalproject/shiny/Final_Proj")


library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Final Project"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("info")),
      
      menuItem("Data", tabName = "data", icon = icon("database"),
        menuSubItem("Data Description", tabName = "data_description", icon = icon("angle-right"))
      ),
      menuItem("Cost", tabName = "cost", icon = icon("th"),
        menuSubItem("Admission", tabName = "admission", icon = icon("angle-right")),
        menuSubItem("Affordability", tabName = "affordability", icon = icon("angle-right"))
      ),
      menuItem("Outcome", tabName = "outcome", icon = icon("check"),
         menuSubItem("Education Quality", tabName = "quality", icon = icon("angle-right")),
         menuSubItem("Diversity", tabName = "diversity", icon = icon("angle-right")),
         menuSubItem("Earnings", tabName = "earnings", icon = icon("angle-right")),
         menuSubItem("Completion", tabName = "completion", icon = icon("angle-right"))
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "introduction",
        h2("EDAV Final Project")
      ),
      tabItem(
        tabName = "data_description",
        fluidPage(
          fluidRow(
            column(12,
                   tableOutput('table')
            )
          )
        )
      ),
      
      
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

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  # render table
  output$table <- renderTable(iris)
}


# Run the application 
shinyApp(ui = ui, server = server)

