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
library(DT)
library(xlsx)
library(gridExtra)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Final Project"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("info")),
      
      menuItem("Data", tabName = "data", icon = icon("database"),
        menuSubItem("Raw Data", tabName = "raw_data", icon =  icon("angle-right")),
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
        fluidPage(
          fluidRow(
            column(12,
              h1("EDAV Final Project")
            )
            # column(12,
            #   h3('\t Through our project, we would like to provide a way for students and their 
            #   families to compare across different colleges based on the cost-outcome tradeoffs
            #   catering to their own needs, academic or career goals.')
            # )
          ),
          fluidRow(
            column(6, 
              h3("Scatterplot: Admission rate & SAT average"),
              plotOutput("admission_scatterplot", height = 500, 
                click = "admission_scatterplot_click",
                brush = brushOpts(
                  id = "admission_scatterplot_brush"
                )
              )
            ),
            column(6, height = 500,
              h3("Selected universities"),
              # verbatimTextOutput("admission_scatterplot_brush_info")
              dataTableOutput('admission_scatterplot_brush_info')
            )
          )
        )
      ),
      tabItem(
        tabName = "raw_data",
        fluidPage(
          fluidRow(
            column(12,
                   dataTableOutput('table_raw')
            )
          )
        )
      ),
      tabItem(
        tabName = "data_description",
        fluidPage(
          fluidRow(
            column(12,
                   tableOutput('table_description')
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
          # sidebarLayout(
          #   sidebarPanel(
          #     sliderInput("bins",
          #                 "Number of bins:",
          #                 min = 1,
          #                 max = 50,
          #                 value = 30)
          #   ),
          #   
          #   # Show a plot of the generated distribution
          #   mainPanel(
          #     plotOutput("distPlot"),
          #     plotOutput("scatterplot_admission")
          #   )
          # )
          mainPanel(
            h3("Histogram: Admission Rate and SAT Average"),
            plotOutput("distPlot"),
            h3("Scatter plot: Admission Rate and SAT Average"),
            plotOutput("scatterplot_admission")
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
        tabName = "completion", h2("completion")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  college <- readRDS('data/college.rds')
  
  # bar plot, admission
  output$distPlot <- renderPlot({
    # # generate bins based on input$bins from ui.R
    # x    <- faithful[, 2] 
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    # # draw the histogram with the specified number of bins
    # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    # college <- readRDS('data/college.rds')
    college_no_na <- college[with(college, (!is.na(sat_avg)) & (!is.na(admission_rate))), ]
    
    plot_admi <- ggplot(college_no_na, aes(x = admission_rate)) +
      geom_histogram()
    
    plot_sat <- ggplot(college_no_na, aes(x = sat_avg)) +
      geom_histogram()
    
    grid.arrange(plot_admi, plot_sat, ncol = 2)
  })
  
  # scatter plot, admission
  output$admission_scatterplot <- renderPlot({
    # college <- readRDS('data/college.rds')
    college_no_na <- college[with(college, (!is.na(sat_avg)) & (!is.na(admission_rate))), ]
    sat_avg <- college_no_na$sat_avg
    admission_rate <- college_no_na$admission_rate
    
    baseplt <- ggplot(college_no_na, aes(x = sat_avg, y = admission_rate))
    baseplt + geom_point(alpha= 0.5) +
      labs(title = "Scatter Plot", 
           x = "SAT Average", 
           y = "Admission Rate")
  })
  
  
  # render table
  output$table_raw <- renderDT(
    # readRDS('data/college.rds') , options = list(scrollX = TRUE, autoWidth=TRUE)
    college, options = list(scrollX = TRUE, autoWidth=TRUE)
  )
  output$table_description <- renderTable({
    read.csv("data/dictionary.csv")
  })
  
  # selected universities
  # college_no_na <- reactive({
  #   college_no_na <- college[with(college, (!is.na(sat_avg)) & (!is.na(admission_rate))), ]
  #   college_no_na
  # })
  output$admission_scatterplot_brush_info <- renderDT({
    college_no_na <- college[with(college, (!is.na(sat_avg)) & (!is.na(admission_rate))), ]
    selectedPoints <- brushedPoints(college_no_na, input$admission_scatterplot_brush)
    if(nrow(selectedPoints) == 0)
      return()
    selectedPoints
  }, options = list(scrollX = TRUE, autoWidth=TRUE, pageLength = 5))

  
  

}


# Run the application 
shinyApp(ui = ui, server = server)

