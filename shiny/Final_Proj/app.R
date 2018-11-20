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
library(dplyr)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Final Project"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("info")),
      
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
        tabName = "dashboard",
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
              plotOutput("admission_scatterplot", height = 400, 
                click = "admission_scatterplot_click",
                brush = brushOpts(
                  id = "admission_scatterplot_brush"
                )
              )
            ),
            column(6, 
              h3("Selected universities"),
              # verbatimTextOutput("admission_scatterplot_brush_info")
              div(dataTableOutput('admission_scatterplot_brush_info'), style = "font-size:90%") 
            )
          ),
          fluidRow(
            column(3,
              plotOutput('tuition', height = 270)
            ),
            column(3,
              plotOutput('diversity', height = 270)       
            ),
            column(3,
              plotOutput('faculty', height = 270)       
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
  college_no_na <- college[with(college, (!is.na(sat_avg)) & (!is.na(admission_rate))), ]
  
  # tuition
  output$tuition <- renderPlot({
    college_tuition <- college[c("tuition_instate", "tuition_out")]
    # college_tuition$tuition_instate <- (as.integer(college_tuition$tuition_instate / 1000) + 0.1)
    college_tuition$tuition_out <- college_tuition$tuition_out / 1000
    
    
    
    selectedPoints <- brushedPoints(college_no_na, input$admission_scatterplot_brush)
    s <- input$admission_scatterplot_brush_info_rows_selected
    selected_one_str <- selectedPoints[s, c('college_id')]
    selected_one <- college_no_na[which(college_no_na['college_id'] == as.character(selected_one_str)),]
    
    # cond <- college_tuition$tuition_instate == as.integer(selected_one$tuition_instate / 1000) * 1000
    cond <- as.integer(college_tuition$tuition_instate / 2000) * 2000 == as.integer(selected_one$tuition_instate / 2000) * 2000
    ggplot(college_tuition, aes(x=tuition_instate)) + 
      geom_histogram(data=subset(college_tuition, cond == FALSE), binwidth = 2000, boundary = 0, closed = "left", fill = "lightblue") +
      geom_histogram(data=subset(college_tuition, cond == TRUE), binwidth = 2000, boundary = 0, closed = "left", fill = "blue")
  })
  
  # diversity
  output$diversity <- renderPlot({
    race_cat = c("college_id","race_white", "race_black", "race_hispanic", 
                 "race_asian", "race_native", "race_pacific", 
                 "race_2more", "race_nonresident", "race_unknown")
    race = college %>% select ("name", race_cat)
    colnames(race) = c("name","college_id", "White", "Black", "Hispanic", "Asian",
                       "Native", "Pacific", "Two_more", "Non_resident", "Unknown")
    race <- race %>% mutate(RDI = 1- {(race$White)^2 + (race$Black)^2 + 
        (race$Hispanic)^2 + (race$Asian)^2 + (race$Native)^2 + (race$Pacific)^2 + 
        (race$Two_more)^2 + (race$Non_resident)^2 + (race$Unknown)^2})
    
    selectedPoints <- brushedPoints(college_no_na, input$admission_scatterplot_brush)
    s <- input$admission_scatterplot_brush_info_rows_selected
    selected_one_str <- selectedPoints[s, c('college_id')]
    
    cond <- as.integer(race$RDI / 0.03) * 0.03 == as.integer(race[which(race$college_id == as.character(selected_one_str)), ]$RDI / 0.03) * 0.03

    ggplot(race, aes(x=RDI)) + 
      geom_histogram(data = subset(race, cond == FALSE), binwidth = 0.03, boundary = 0, closed = "left", fill = "lightblue") +
      geom_histogram(data = subset(race, cond == TRUE), binwidth = 0.03, boundary = 0, closed = "left", fill = "blue")
  })
  
  # faculty
  output$faculty <- renderPlot({
    college_faculty <- college[c("college_id", "pct_faculty")]
    
    selectedPoints <- brushedPoints(college_no_na, input$admission_scatterplot_brush)
    s <- input$admission_scatterplot_brush_info_rows_selected
    selected_one_str <- selectedPoints[s, c('college_id')]
    
    cond <- as.integer(college_faculty$pct_faculty / 0.03) * 0.03 == as.integer(college_faculty[which(college_faculty$college_id == as.character(selected_one_str)), ]$pct_faculty / 0.03) * 0.03
    
    ggplot(college_faculty, aes(x=pct_faculty)) + 
      geom_histogram(data=subset(college_faculty, cond == FALSE), binwidth = 0.03, boundary = 0, closed = "left", fill = "lightblue") +
      geom_histogram(data=subset(college_faculty, cond == TRUE), binwidth = 0.03, boundary = 0, closed = "left", fill = "blue")
  })
  
  
  # bar plot, admission
  output$distPlot <- renderPlot({
    # # generate bins based on input$bins from ui.R
    # x    <- faithful[, 2] 
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    # # draw the histogram with the specified number of bins
    # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    # college <- readRDS('data/college.rds')
    # college_no_na <- college[with(college, (!is.na(sat_avg)) & (!is.na(admission_rate))), ]
    
    plot_admi <- ggplot(college_no_na, aes(x = admission_rate)) +
      geom_histogram()
    
    plot_sat <- ggplot(college_no_na, aes(x = sat_avg)) +
      geom_histogram()
    
    grid.arrange(plot_admi, plot_sat, ncol = 2)
  })
  
  # scatter plot, admission
  output$admission_scatterplot <- renderPlot({
    # college <- readRDS('data/college.rds')
    # college_no_na <- college[with(college, (!is.na(sat_avg)) & (!is.na(admission_rate))), ]
    sat_avg <- college_no_na$sat_avg
    admission_rate <- college_no_na$admission_rate
    
    selectedPoints <- brushedPoints(college_no_na, input$admission_scatterplot_brush)
    s <- input$admission_scatterplot_brush_info_rows_selected
    selected_one_str <- selectedPoints[s, c('college_id')]
    selected_one <- college_no_na[which(college_no_na['college_id'] == as.character(selected_one_str)),]
    
    baseplt <- ggplot(college_no_na, aes(x = sat_avg, y = admission_rate))
    baseplt + geom_point(alpha= 0.5) +
      labs(title = "Scatter Plot", 
           x = "SAT Average", 
           y = "Admission Rate") +
      geom_point(data = selected_one, fill = 'red', shape = 24, size = 4) +
      geom_text(data = selected_one, aes(label = name), vjust = -0.5, color = "blue")
    
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
    # college_no_na <- college[with(college, (!is.na(sat_avg)) & (!is.na(admission_rate))), ]
    college_no_na_show <- college_no_na[, c('name', 'sat_avg', 'admission_rate')]
    selectedPoints <- brushedPoints(college_no_na_show, input$admission_scatterplot_brush)
    if(nrow(selectedPoints) == 0)
      return()
    selectedPoints
  }, options = list(scrollX = TRUE, autoWidth=FALSE, pageLength = 10, processing = TRUE), selection = 'single')
  
  
  
  

}


# Run the application 
shinyApp(ui = ui, server = server)

