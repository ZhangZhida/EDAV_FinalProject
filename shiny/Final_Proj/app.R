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
library(extracat)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Final Project"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("info")),
      
      menuItem("Data", tabName = "data", icon = icon("database"),
        menuSubItem("Raw Data", tabName = "raw_data", icon =  icon("angle-right")),
        menuSubItem("Data Description", tabName = "data_description", icon = icon("angle-right")),
        menuSubItem("Missing Pattern", tabName = "missing_pattern", icon = icon("angle-right"))
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidPage(
          div(h4("Instruction: please select an area in the scatterplot, then select a university in the right-hand table."), style = "color:red"),
          fluidRow(
            column(6, 
              box(width = NULL, title = "Interactive select an area", status = "info", solidHeader = TRUE,
                plotOutput("admission_scatterplot", height = 380,
                   click = "admission_scatterplot_click",
                   brush = brushOpts(
                     id = "admission_scatterplot_brush"
                   )
                )
              )
            ),
            column(6, 
              # verbatimTextOutput("admission_scatterplot_brush_info")
              box(width = NULL, title = "Selected university", status = "success", solidHeader = TRUE,
                div(dataTableOutput('admission_scatterplot_brush_info'), style = "font-size:72%") 
              )
            )
          ),
          fluidRow(
            column(3,
               box(width = NULL, status = 'primary',
                   plotOutput('avg_10yr_salary', height = 270)
               )
            ),
            column(3,
              box(width = NULL, status = 'primary',
                plotOutput('faculty', height = 270)   
              )
            ),
            column(3,
              box(width = NULL, status = 'primary',
                plotOutput('diversity', height = 270)    
              )
            ),
            column(3,
              box(width = NULL, status = 'primary',
                plotOutput('completion', height = 270) 
              )
            )
          ),
          withMathJax(textOutput("formula"))
        )
      ),
      tabItem(
        tabName = "raw_data",
        fluidPage(
          titlePanel("Raw Data"),
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
          titlePanel("Data Description"),
          fluidRow(
            column(12,
                   tableOutput('table_description')
            )
          )
        )
      ),tabItem(
        tabName = "missing_pattern",
        fluidPage(
          titlePanel("Data Missing Pattern"),
          fluidRow(
            column(12,
                   plotOutput('missing_pattern', height = 1200)
            )
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  college <- readRDS('data/college.rds')
  college_no_na <- college[with(college, (!is.na(sat_avg)) & (!is.na(admission_rate))), ]
  
  # # tuition
  # output$tuition <- renderPlot({
  #   college_tuition <- college[c("tuition_instate", "tuition_out")]
  #   
  #   selectedPoints <- brushedPoints(college_no_na, input$admission_scatterplot_brush)
  #   s <- input$admission_scatterplot_brush_info_rows_selected
  #   selected_one_str <- selectedPoints[s, c('college_id')]
  #   selected_one <- college_no_na[which(college_no_na['college_id'] == as.character(selected_one_str)),]
  #   
  #   BIN_INTEV <- 2000
  #   cond <- as.integer(college_tuition$tuition_instate / BIN_INTEV) * BIN_INTEV == as.integer(selected_one$tuition_instate / BIN_INTEV) * BIN_INTEV
  #   
  #   ggplot(college_tuition, aes(x=tuition_instate)) + 
  #     geom_histogram(data=subset(college_tuition, cond == FALSE), binwidth = BIN_INTEV, boundary = 0, closed = "left", fill = "lightblue") +
  #     geom_histogram(data=subset(college_tuition, cond == TRUE), binwidth = BIN_INTEV, boundary = 0, closed = "left", fill = "blue")
  # })
  
  # missing pattern
  output$missing_pattern <- renderPlot({
    visna(college, sort = "b")
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
    
    BIN_INTEV <- 0.03
    select_RDI <- race[which(race$college_id == as.character(selected_one_str)), ]$RDI
    cond <- as.integer(race$RDI / BIN_INTEV) * BIN_INTEV == as.integer(select_RDI / BIN_INTEV) * BIN_INTEV

    ggplot(race, aes(x=RDI)) + 
      geom_histogram(data = subset(race, cond == FALSE), binwidth = BIN_INTEV, boundary = 0, closed = "left", fill = "lightblue") +
      labs(x = "RDI(diversity)", y = "amount")+
      geom_histogram(data = subset(race, cond == TRUE), binwidth = BIN_INTEV, boundary = 0, closed = "left", fill = "blue") +
      geom_vline(xintercept = select_RDI, linetype = "dotted")
  })
  
  # faculty
  output$faculty <- renderPlot({
    college_faculty <- college[c("college_id", "pct_faculty")]
    
    selectedPoints <- brushedPoints(college_no_na, input$admission_scatterplot_brush)
    s <- input$admission_scatterplot_brush_info_rows_selected
    selected_one_str <- selectedPoints[s, c('college_id')]
    
    BIN_INTEV <- 0.03
    selected_pct_faculty <- college_faculty[which(college_faculty$college_id == as.character(selected_one_str)), ]$pct_faculty
    cond <- as.integer(college_faculty$pct_faculty / BIN_INTEV) * BIN_INTEV == as.integer(selected_pct_faculty / BIN_INTEV) * BIN_INTEV
    
    ggplot(college_faculty, aes(x=pct_faculty)) + 
      geom_histogram(data=subset(college_faculty, cond == FALSE), binwidth = BIN_INTEV, boundary = 0, closed = "left", fill = "lightblue") +
      labs(x = "percentage of faculty", y = "amount") + 
      geom_histogram(data=subset(college_faculty, cond == TRUE), binwidth = BIN_INTEV, boundary = 0, closed = "left", fill = "blue") +
      geom_vline(xintercept = selected_pct_faculty, linetype = "dotted")
  })
  
  # avg_10yr_salary
  output$avg_10yr_salary <- renderPlot({
    college_avg_10yr_salary<- college[c("college_id", "avg_10yr_salary")]
    
    selectedPoints <- brushedPoints(college_no_na, input$admission_scatterplot_brush)
    s <- input$admission_scatterplot_brush_info_rows_selected
    selected_one_str <- selectedPoints[s, c('college_id')]
    
    BIN_INTEV <- 5000
    selected_salary <- college_avg_10yr_salary[which(college_avg_10yr_salary$college_id == as.character(selected_one_str)), ]$avg_10yr_salary
    cond <- as.integer(college_avg_10yr_salary$avg_10yr_salary / BIN_INTEV) * BIN_INTEV == as.integer(selected_salary / BIN_INTEV) * BIN_INTEV
    
    ggplot(college_avg_10yr_salary, aes(x=avg_10yr_salary)) + 
      geom_histogram(data=subset(college_avg_10yr_salary, cond == FALSE), binwidth = BIN_INTEV, boundary = 0, closed = "left", fill = "lightblue") +
      labs(x = "10 year salary(average)", y = "amount") +
      geom_histogram(data=subset(college_avg_10yr_salary, cond == TRUE), binwidth = BIN_INTEV, boundary = 0, closed = "left", fill = "blue") +
      geom_vline(xintercept = selected_salary, linetype = "dotted")
  })
  
  
  # completion
  output$completion <- renderPlot({
    college_completion <- college[c("college_id", "completion_rate")]
    
    selectedPoints <- brushedPoints(college_no_na, input$admission_scatterplot_brush)
    s <- input$admission_scatterplot_brush_info_rows_selected
    selected_one_str <- selectedPoints[s, c('college_id')]
    
    BIN_INTEV <- 0.03
    selected_completion_rate <- college_completion[which(college_completion$college_id == as.character(selected_one_str)), ]$completion_rate
    cond <- as.integer(college_completion$completion_rate / BIN_INTEV) * BIN_INTEV == as.integer( selected_completion_rate / BIN_INTEV) * BIN_INTEV
    
    ggplot(college_completion, aes(x=completion_rate)) + 
      geom_histogram(data=subset(college_completion, cond == FALSE), binwidth = BIN_INTEV, boundary = 0, closed = "left", fill = "lightblue") +
      labs(x = "completion rate", y = "amount") +
      geom_histogram(data=subset(college_completion, cond == TRUE), binwidth = BIN_INTEV, boundary = 0, closed = "left", fill = "blue") +
      geom_vline(xintercept = selected_completion_rate, linetype = "dotted")
  })
  
  # scatter plot, admission
  output$admission_scatterplot <- renderPlot({
    sat_avg <- college_no_na$sat_avg
    admission_rate <- college_no_na$admission_rate
    
    selectedPoints <- brushedPoints(college_no_na, input$admission_scatterplot_brush)
    s <- input$admission_scatterplot_brush_info_rows_selected
    selected_one_str <- selectedPoints[s, c('college_id')]
    selected_one <- college_no_na[which(college_no_na['college_id'] == as.character(selected_one_str)),]
    
    baseplt <- ggplot(college_no_na, aes(x = sat_avg, y = admission_rate))
    baseplt + geom_point(alpha= 0.8, color = "navy", stroke = 0) +
      labs(title = "Scatterplot: Admission rate & SAT average", 
           x = "SAT Average", 
           y = "Admission Rate") +
      geom_point(data = selected_one, color = 'red', shape = 24, size = 4) +
      theme_classic(13)+
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
  
  output$admission_scatterplot_brush_info <- renderDT({
    college_no_na_show <- college_no_na[, c('name', 'sat_avg', 'admission_rate', 'tuition_instate', 'tuition_out')]
    selectedPoints <- brushedPoints(college_no_na_show, input$admission_scatterplot_brush)
    if(nrow(selectedPoints) == 0)
      return()
    selectedPoints
  }, options = list(scrollX = TRUE, autoWidth=FALSE, pageLength = 10, processing = TRUE), selection = 'single')
  
  # RDI explanation
  output$formula <- renderPrint({
    print(paste0("*Racial Diversity Index (RDI)*, which is defined by the probability that any two students selected at random would have different races. An RDI close to 1 implies high racial diversity while an RDI close to 0 implies low. This index was developed by Missouri State University (https://diversity.missouristate.edu/DiversityIndex.htm)."))
  })
  
  
  

}


# Run the application 
shinyApp(ui = ui, server = server)

