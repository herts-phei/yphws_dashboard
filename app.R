
## IN DEVELOPMENT. 

library(tidyverse)
library(pins)
library(echarts4r)
library(reactable)
library(shiny)
library(tablerDash)
library(shinyWidgets)
library(shinydashboard)
#library(bs4Dash)

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
                    pickerInput("comp", label = "Select what to group by",
                                choices = list("Sex" = "sex", "Year group" = "schyear", "District" = "District"), 
                                selected = "sex", multiple = FALSE), 
                    tablerNavMenuItem(
                      "Key Points",
                      tabName = "Key Points"
                    ),
                    tablerNavMenuItem(
                      "Explore Data",
                      tabName = "ExploreData"
                    ),
                    tablerNavMenuItem(
                      "Inequalities",
                      tabName = "Inequalities"
                    ),
                    tablerNavMenuItem(
                      "Export",
                      tabName = "Export"
                    ),
                    tablerNavMenuItem(
                      "About",
                      tabName = "About"
                    )
                    
                    
      )
    ),
    body = tablerDashBody(
      tablerTabItems(
        tablerTabItem(
          tabName = "Key Points"
        ),
        tablerTabItem(
          tabName = "ExploreData",
          fluidRow(
            column(2, 
                   # pick survey topic
                   pickerInput(
                     inputId = "domains", 
                     label = "Select health topic/s:", 
                     choices = c(domains),
                     selected = "Safety", multiple = T
                   ),
                   uiOutput("questions"),
                   HTML("<a href='#anchorid'>Working anchor example</a>")
            ),
            column(10, uiOutput("explore_boxes"))
        )
      ),
      tablerTabItem(
        tabName = "Inequalities"
      ),
      tablerTabItem(
        tabName = "Export"
      ),
      tablerTabItem(
        tabName = "About"
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
                              tabBox(width = 12, side = "right", status = "success",
                                         collapsible = TRUE, 
                                         title = HTML(paste0("<hr><br><a id='anchorid'></a>", rv$filtered$chk_var[i],"<br>")),
                                         tabPanel("Summary", 
                                           HTML(create_sum_sentence(dataset = current,
                                                               multi = F,
                                                               value_of_interest = F,
                                                               full_data = rv$filtered$chk_stats,
                                                               diffs = rv$filtered$chk_diff,
                                                               custom_grp = unique(current$breakdown),
                                                               group_of_interest = unique(current$breakdown)[2])),
                                           br(),
                                           create_basic_plot(df = current,
                                                             plot_custom_grp = unique(current$breakdown),
                                                             rotate = 60,
                                                             plot_title = current$question_text[1])
                                         ),
                                     tabPanel(
                                       "Table", 
                                       rv$filtered$chk_stats %>% 
                                         mutate(value = paste0(round(as.numeric(value) * 100, 2), "%"),
                                                lowercl = paste0(round(as.numeric(lowercl) * 100, 2), "%"),
                                                uppercl = paste0(round(as.numeric(uppercl) * 100, 2), "%")
                                                ) %>% 
                                         select(breakdown, question = question_text, response, value, count, denominator,
                                                lowercl, uppercl) %>% 
                                       reactable(groupBy = c("breakdown", "question"),
                                                 columns = list(
                                                   value = colDef(maxWidth = 70),
                                                   count = colDef(maxWidth = 65),
                                                   denominator = colDef(maxWidth = 70),
                                                   lowercl = colDef(maxWidth = 70),
                                                   uppercl = colDef(maxWidth = 70)
                                                 ))
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
