
## IN DEVELOPMENT. 

library(tidyverse)
library(pins)
library(echarts4r)
library(reactable)
library(shiny)
#library(tablerDash)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)

board_register("rsconnect",
               server = "srv-gcp-ms-connect:3939",
               key = Sys.getenv("CONNECT_API_KEY"))

domains <- c("Living Conditions", "Diet and Lifestyle",     
             "Education", "Demographics",
             "Mental Health and Wellbeing", "Smoking and Vaping",         
             "Alcohol Consumption", "Drug Use",                   
             "Sexual Health", "Safety",                  
             "Sustainability", "COVID-19")
names(domains) <- domains

# UI ----------------------------------------------------------------

ui <- tablerDashPage(
  title = "Dashboard", 
    navbar = tablerDashNav(
      id = "nav",
      src = "img/yphws_logo.png",
      tablerNavMenu(id = "tabs",
                    selectInput("comp", label = "Select what to group by (default is sex)",
                                choices = list("Sex" = "sex", "Year group" = "year", "District" = "District"), 
                                selected = "sex", multiple = T), 
                    tablerNavMenuItem(
                      "Insight",
                      tabName = "Insight"
                    ),
                    tablerNavMenuItem(
                       "Key Points",
                       tabName = "Key Points"
                     )
                    
      )
    ),
    body = tablerDashBody(
      tablerTabItems(
        tablerTabItem(
          tabName = "Key Points"
        ),
        tablerTabItem(
          tabName = "Insight",
          fluidRow(
            column(2, 
                   br(),
                   br(),
                   # pick survey topic
                   pickerInput(
                     inputId = "domains",
                     label = "Select health topic/s:", 
                     choices = domains,
                     multiple = TRUE,
                     selected = "Safety"
                   ),
                   uiOutput("questions")
            ),
            column(10, uiOutput("explore_boxes"))
        )
      )
    )
  )
)
  



# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  # Uncomment for testing
  observe({
    
    if ("District" %in% input$comp) { browser() }
    
  })

  # --Load all data-----
  rv <- reactiveValues()
  rv$params <- get_params() # params
  rv$data <- get_data(params = isolate(rv$params), data = isolate(params$data_pin)) # Raw data
  rv$data_old <- get_data(params = isolate(rv$params), data = isolate(params$prev_data_pin)) # Raw data, previous. 
  
  # --Reactive UIs----
  output$questions <- renderUI({
    questions <- isolate(rv$data$q_coded) %>% 
      mutate(survey_text = as.character(survey_text)) %>% 
      filter(question_theme %in% input$domains) 
    
    pickerInput("questions", label = "Select/type in a question (multiple can be selected)",
                choices = as.character(unique(questions$survey_text)), multiple = T, 
                selected = as.character(unique(questions$survey_text)), 
                options = list(`live-search` = TRUE))
  })
  
  # --Generate statistics using raw data
  stats <- reactive({
    get_stats(
      data = rv$data$data,
      group = input$comp,
      q_coded = rv$data$q_coded
    )
  })
  
  diffs <- reactive({
    sel_comp <- rv$data$data %>% 
      select(input$comp) 
    sel_comp <- unique(na.omit(unlist(sel_comp)))
    
    get_stats_diffs(stats = stats(), 
                    levels = c("All Responses", sel_comp), 
                    compare_to_all = F) # TODO
  })
  
  observe({
    # vector of selected vars
    single <- rv$data$q_coded %>% 
      filter(survey_text %in% input$questions, !multicat)
    
    #TODO deduplicate multicat questions.
    rv$filtered$chk_var <- rv$data$q_coded %>% 
      filter(question_coded %in% single$question_coded) %>% 
      pull(question_coded)
    # rv$filtered$chk_var <- rv$data$q_coded %>% 
    #   filter(survey_text %in% input$questions, multicat) %>% 
    #   mutate(question_raw = gsub("\\..*", "", question_raw),
    #          question_coded = question_coded_gen) %>% 
    #   distinct(question_raw, .keep_all = TRUE) %>% 
    #   bind_rows(single) %>% 
    #   pull(question_coded)
    
    # filtered datasets
    rv$filtered$chk_stats <- filter(stats(), question %in% rv$filtered$chk_var)
    rv$filtered$chk_diff <- filter(diffs(), question %in% rv$filtered$chk_var)
  })
  

# Explore data --------------------------------------------------------------------

  ## --TEXT----
  # output$text_summary <- renderText({
  #   
  #   # create_sum_sentence(dataset = chk_stats, 
  #   #                     multi = F, 
  #   #                     value_of_interest = F, 
  #   #                     full_data = stats,
  #   #                     diffs = chk_diff,
  #   #                     custom_grp = plot_custom_grp,
  #   #                     group_of_interest = cat_of_interest)
  #   
  #   "WIP"
  #   
  # })

  boxes <- reactive({
    
    l <- list()
    for (i in 1:length(rv$filtered$chk_var)){
      
      # Current question
      current <- filter(rv$filtered$chk_stats, question %in% rv$filtered$chk_var[i])
      
      l[[i]] <- tabItem("name", 
                              tablerCard(status = "success", width = 12,
                                         title = rv$filtered$chk_var[i],
                                         tabPanel(
                                           create_sum_sentence(dataset = current, 
                                                               multi = F, 
                                                               value_of_interest = F, 
                                                               full_data = rv$filtered$chk_stats,
                                                               diffs = rv$filtered$chk_diff,
                                                               custom_grp = unique(current$breakdown),
                                                               group_of_interest = unique(current$breakdown)[2]),
                                           create_basic_plot(df = current, 
                                                             plot_custom_grp = unique(current$breakdown), 
                                                             rotate = 60,
                                                             plot_title = current$question_text[1])
                                         ),
                                         tabPanel(
                                           "Test"
                                         )
                                         
        
        
      ) 
      )
    }
    
    return(l)
    
  })

  output$explore_boxes <- renderUI(boxes())


  
}

# Run the application 
shinyApp(ui = ui, server = server)
