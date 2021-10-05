
## IN DEVELOPMENT. 

library(tidyverse)
library(pins)
library(echarts4r)
library(reactable)
library(shiny)
library(shinydashboard)
library(shinyWidgets)

board_register("rsconnect",
               server = "srv-gcp-ms-connect:3939",
               key = Sys.getenv("CONNECT_API_KEY"))


# UI ----------------------------------------------------------------------

ui <- dashboardPage(skin = "red", 
  
  dashboardHeader(
    title = "YPHWS Dashboard Demo"
  ),
  
  dashboardSidebar(
    
    # pick variable to compare by 
    selectInput("comp", label = "Select what to compare by (default is sex)",
                 choices = list("Sex" = "sex"), 
                 selected = "sex", multiple = T),
    
    # pick survey topic
    uiOutput("topic"),

    # pick questions
    uiOutput("questions")
  ),
  
  dashboardBody(
      
    fluidRow(
      
      tabBox(
        title = "",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "250px",
        
        tabPanel("Summary", htmlOutput("text_summary")),
        
        tabPanel("Plot", 
                 echarts4rOutput("plot")),
        
        tabPanel("Differences found",
                 "Tab content"),
        
        tabPanel("Data Table", 
                 "Tab content 2")
        
        )
      #TODO modularise tab box so each question selected would result in 1 tab box 
      )
    
    # br(), 
    # 
    # fluidRow(
    #   
    #   tabBox(
    #     title = "",
    #     # The id lets us use input$tabset1 on the server to find the current tab
    #     id = "tabset2", height = "250px",
    #     
    #     tabPanel("Plot", 
    #              echarts4rOutput("plot2")),
    #     
    #     tabPanel("Differences found",
    #              "Tab content"),
    #     
    #     tabPanel("Data Table", 
    #              "Tab content 2")
    #     
    #   )
    #   
    # )
    )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {

  rv <- reactiveValues()
  rv$params <- get_params()
  rv$schools <- pin_get("ayu/school_data", board = "rsconnect")
  rv$lookup <- read.csv("data-raw/responses_lookup.csv")
  
  data <- reactive({
    
    get_data(data = pin_get("bkimpton/youth_survey_responses", board = "rsconnect"),
             q_coded = isolate(rv$lookup), 
             school_data = isolate(rv$schools))
    
  })
    
  stats <- reactive({
    
    get_sum_stats(data = data(), 
                  var = "sex") # TODO
    
  })
  
  diffs <- reactive({
    
    get_diffs(stats = stats(),
              var = "sex") # TODO
    
  })

  output$topic <- renderUI({
    
    checkboxGroupInput("topic", label = "Select/type in an indicator (multiple can be selected)",
                choices = as.character(unique(isolate(rv$lookup$question_theme))), 
                selected = "Mental Health and Wellbeing")  #TODO)
    
  })
  
  output$questions <- renderUI({
    
    questions <- isolate(rv$lookup) %>% 
      mutate(survey_text = as.character(survey_text)) %>% 
      filter(question_theme %in% input$topic) 
    
    pickerInput("questions", label = "Select/type in an indicator (multiple can be selected)",
                choices = as.character(unique(questions$survey_text)), multiple = T, 
                selected = as.character(unique(questions$survey_text)), 
                options = list(`live-search`=TRUE))
    
    })
  
  chk_stats <- reactive({
    
    output <- list()
    stats <- stats()
    diffs <- diffs()
    
    output$chk_var <- c('mental_talk')
    output$chk_stats <- filter(stats, question %in% output$chk_var)
    output$chk_diff <- filter(diffs, question %in% output$chk_var)
    
    return(output)
    
  })
  

# Text --------------------------------------------------------------------

  output$text_summary <- renderText("Within Hertfordshire, 12420 students (96%) responded to this question. Of these, 6565 were female, 5700 were male, and 155 stated ‘Other’ for sex.
<br>
The most common response for all respondents was ‘Yes’, which made up 70.4% of responses and the least common response was ‘No’, with 10.8% of responses. For more detail, please see the Graph or Table tabs.
<br>
Overall there were 5 significant difference(s) found between groups. 
There were 2 significant difference(s) found between year groups. 

There were 1 significant difference(s) found between sexes. 

There were 1 significant difference(s) found between ethnic groups. 

There were 1 significant difference(s) found between districts. ")

# Plots -------------------------------------------------------------------

  output$plot <- renderEcharts4r({

    chk_stats <- isolate(chk_stats())
    create_basic_plot(df = chk_stats$chk_stats,
                      chk_var = chk_stats$chk_var)
    
    
  })
  
  output$plot2 <- renderEcharts4r({
    
    chk_stats <- isolate(chk_stats())
    create_basic_plot(df = chk_stats$chk_stats,
                      chk_var = chk_stats$chk_var)
    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
